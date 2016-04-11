(* This file is shared between jenga/root.ml and programs in this directory. Jenga passes
   the metadata by writing a .sexp file. *)

open Core.Std

let opam_of_ocamlfind = function
  | "threads" -> "base-threads"
  | s ->
    match Option.value_map (String.lsplit2 s ~on:'.') ~f:fst ~default:s with
    | "bigarray"
    | "compiler-libs"
    | "num"
    | "unix"
    | "dynlink"
    | "str" -> "ocaml" (* "ocaml" is considered preinstalled *)
    | s -> s

module Library = struct
  module Kind = struct
    type t =
      | Normal
      | Ppx_rewriter         (** Need to build a standalone ppx executable *)
      | Ppx_type_conv_plugin (** Need to build a ppx_deriving plugin       *)
      | Ppx_library          (** To deal with an annoying corner case of
                                 ocamlfind and ppx_deriving *)
      | Ppx_alias            (** Don't even ask... *)
    [@@deriving sexp]
  end

  type t =
    { internal_name : string
    ; install_as    : string option
    ; kind          : Kind.t        [@default Normal]
    (** Sub-directory inside the public repository *)
    ; c_files       : string list   [@default []]
    ; wrapped       : bool          [@default true]
    ; short_desc    : string option [@default None]
    ; modules       : string list   [@default []]
    ; path          : string
    }
  [@@deriving sexp]
end

module Executable = struct
  type t =
    { main_module  : string
    (** name under which the executable is installed *)
    ; install_as   : string option
    ; path         : string
    }
  [@@deriving sexp]
end

(** Common build info for libraries and executables *)
module Buildable = struct
  module Dependency = struct
    module Context = struct
      type t = Build | Runtime | Both [@@deriving sexp]
    end

    module Global = struct
      type t =
        { opam_package      : string
        ; ocamlfind_package : string
        }
      [@@deriving sexp]

      let of_ocamlfind_package s =
        { ocamlfind_package = s
        ; opam_package      = opam_of_ocamlfind s
        }

      let t_of_sexp sexp =
        match sexp with
        | Sexp.Atom s -> of_ocamlfind_package s
        | Sexp.List _ -> t_of_sexp sexp

      let sexp_of_t t =
        if t.opam_package = opam_of_ocamlfind t.ocamlfind_package then
          Sexp.Atom t.ocamlfind_package
        else
          sexp_of_t t
    end

    module Local = struct
      type t =
        { internal_name     : string
        ; ocamlfind_package : string option
        }
      [@@deriving sexp]
    end

    module Kind = struct
      type t =
        | Local  of Local.t
        | Global of Global.t
      [@@deriving sexp]
    end

    type t =
      { kind     : Kind.t
      ; context  : Context.t [@default Both]
      (** Library is developped at janestreet *)
      ; internal : bool [@default false]
      ; lib_kind : Library.Kind.t
      (** Name of a wrapper module. This is for external libraries that are wrapped when
          imported in our tree. We need to generate a wrapper at use sites. *)
      ; wrapper  : (string * string list) option [@default None]
      }
    [@@deriving sexp]
  end

  type t =
    { (** Sub-directory inside the public repository *)
      path                  : string
    (** Public names of dependencies *)
    ; dependencies          : Dependency.t list        [@default []]
    ; js_ppxs               : Dependency.Global.t list [@default []]
    ; preprocessor_deps     : string list              [@default []]
    ; copy_config_from_core : bool                     [@default false]
    }
  [@@deriving sexp]
end

module Package = struct
  module Dependency = struct
    type t =
      { opam_package : string
      ; internal     : bool
      }
    [@@deriving sexp]
  end

  module Install_section = struct
    type t =
      | Lib
      | Libexec
      | Bin
      | Sbin
      | Toplevel
      | Share
      | Share_root
      | Etc
      | Doc
      | Stublibs
      | Man
    [@@deriving sexp, compare]
  end

  module File_to_install = struct
    type t =
      { src : string
      ; dst : string option
      }

    let t_of_sexp : Sexp.t -> t = function
      | Atom s -> { src = s; dst = None }
      | List [ Atom src; Atom "as"; Atom dst ] -> { src; dst = Some dst }
      | sexp ->
        Sexplib.Conv.of_sexp_error
          "files to install must be of the form: << filename >> or \
           << (src_filename as dst_filename) >>"
          sexp
    ;;

    let sexp_of_t : t -> Sexp.t = function
      | { src; dst = None     } -> Atom src
      | { src; dst = Some dst } -> List [ Atom src; Atom "as"; Atom dst ]
    ;;
  end

  (** All filenames are relative to the repository root *)
  type t =
    { name                : string
    ; synopsis            : string
    ; long_description    : string list
    ; copyright_start     : int
    ; libraries           : Library.t list
    ; executables         : Executable.t list
    (** Buildables by path *)
    ; buildables          : Buildable.t list
    ; tarball_filename    : string
    ; file_list_filename  : string
    ; dir_mapping         : (string * string) list
    ; dependencies        : Dependency.t list
    ; install_extra_files : (Install_section.t * File_to_install.t list) list
    ; install_extra_tags  : (Install_section.t * string list) list
    }
  [@@deriving sexp]
end
