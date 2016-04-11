
open Core.Std

module Names_spec = struct
  type old =
    | List of string list
    | All
  [@@deriving of_sexp]

  type t = Ordered_set_lang.t
  let t_of_sexp sexp =
    match old_of_sexp sexp with
    | exception _ -> Ordered_set_lang.t_of_sexp sexp
    | All -> Ordered_set_lang.standard
    | List l -> Ordered_set_lang.t_of_sexp ([%sexp_of: string list] l)
end

module Preprocess_kind = struct
  type t = [
  | `no_preprocessing
  | `command            of string
  | `metaquot
  | `pps                of string list
  ]
  [@@deriving of_sexp]
end

module Preprocess_spec = struct
  type t = Preprocess_kind.t * Names_spec.t
  [@@deriving of_sexp]
end

let preprocess_default = [
  (`pps ["JANE"], Ordered_set_lang.standard)
]

(** Configuration of the inline_tests_runner *)
module Inline_tests = struct
  type t = {
    (** The dependencies of running the inline tests *)
    deps : string sexp_list;
    (** Should inline_tests_runner be run as part of .runtest? *)
    in_runtest : bool; [@default true]
  }
  [@@deriving of_sexp]

  let t_of_sexp sexp =
    let t = t_of_sexp sexp in
    if not (List.is_empty t.deps) && not t.in_runtest
    then
      Sexplib.Conv.of_sexp_error
        "inline tests deps only apply when running as part of runtest"
        sexp;
    t
  ;;

  let default : t = {
    deps = [];
    in_runtest = true;
  }
end

module Preprocessor_conf = struct
  type t = {
    name : string;
    libraries : string sexp_list;
    extra_disabled_warnings : int sexp_list;
    preprocess : Preprocess_spec.t list [@default preprocess_default];
  } [@@deriving of_sexp, fields]
end

module Dep_conf = struct
  type t =
    | File of string
    | Alias of string
    | Glob_files of string
    | Files_recursively_in of string
  [@@deriving of_sexp]
  let t_of_sexp = function
    | Sexp.Atom s -> File s
    | Sexp.List _ as sexp -> t_of_sexp sexp
end

module Rule_conf = struct
  type t = {
    targets : string list;
    deps : Dep_conf.t list;
    action : string;
  } [@@deriving of_sexp, fields]
end

module Alias_conf = struct
  type t = {
    name : string;
    deps : Dep_conf.t list;
    action : string sexp_option;
  } [@@deriving of_sexp, fields]
end

module Compile_c_conf = struct
  type t = {
    names : string list;
    extra_cflags : string sexp_list;
    replace_cflags : string sexp_list;
    includes : string sexp_list;
  } [@@deriving of_sexp]
end

module LibraryX_conf = struct (* X - external library setup *)
  type t = {
    name : string;
    targets : string list; (* with default([]);*)
    deps : Dep_conf.t list; (* with default([]); *)
    action : string;
  } [@@deriving of_sexp, fields]
end

module Embed_conf = struct
  type style = No_preprocessing | Camlp4 | Ppx | Bilingual [@@deriving of_sexp]
  type t = {
    names : string list;
    libraries : string sexp_list;
    cmis : string sexp_list;
    pps : string sexp_list;
    code_style : style;
  } [@@deriving of_sexp]
end

module Library_conf = struct
  module Public_release = struct
    type bool_option = True | False | Default [@@deriving of_sexp]

    type extra_dep =
      { context : Package_types.Buildable.Dependency.Context.t
      ; package : Package_types.Buildable.Dependency.Global.t
      }
    [@@deriving of_sexp]

    let extra_dep_of_sexp (sexp : Sexp.t) =
      match sexp with
      | Atom _ ->
        { context = Both
        ; package = Package_types.Buildable.Dependency.Global.t_of_sexp sexp }
      | _ -> extra_dep_of_sexp sexp

    type status =
      | Build_and_install_as of string
      | Build
      | Ignore
    [@@deriving of_sexp]

    (* Information for the public release *)
    type internal_package =
      { status     : status [@default Ignore]
      ; desc       : string sexp_option
      (* This is only needed because threads and co are hardcoded in jenga/root.ml *)
      ; extra_deps : extra_dep list  [@default []]
      ; kind       : Package_types.Library.Kind.t     [@default Normal]
      }
    [@@deriving of_sexp]

    type external_package =
      { package           : Package_types.Buildable.Dependency.Global.t
      ; virtual_opam_deps : string list [@default []]
      ; wrapped           : bool_option [@default Default]
      }
    [@@deriving of_sexp]

    type t =
      | Internal of internal_package
      | External of external_package
    [@@deriving of_sexp]

    let t_of_sexp (sexp : Sexp.t) =
      match sexp with
      | List (Atom ("external" | "External") :: _) -> t_of_sexp sexp
      | _ -> Internal (internal_package_of_sexp sexp)

    let default = Internal (internal_package_of_sexp (Sexp.List []))
  end

  type t = {
    (* [name] is the name of the library; the name must be distinct from the name of any
       module contained by the library.  The only mandatory field in the config. *)
    name : string;

    public_release : Public_release.t [@default Public_release.default];

    (* [libraries] are libraries required for the compilation of this library
       Defaults to the empty list; but very unusual for this field to be omitted. *)
    libraries : string sexp_list;

    (* The following fields are all optional; the defaults are often fine... *)

    (* [wrapped] selects if the library should be built such that the modules are wrapped
       within an extra level of namespace, named for the library.  This field used to be
       named [packed].
       How this is achieved depends on the setting of [PACKING] env-var
         true ->
            Use the -pack/-for-pack flags of the ocaml compiler.
         false ->
            Dont use the -pack/-for-pack flags.
            Generate renaming file (.ml-gen) from list of modules.
            Use combination of -open and -o arg of 4.02 compiler. *)
    wrapped : bool [@default true];

    (* [modules] lists the names of all modules included in this library.  The names are
       written as lower case, corresponding to filenames containing the ML module
       definition & interface, but with the .ml/.mli suffix truncated.
       By default, the library contain modules for all .ml/.mli files in the directory
       containing the library. *)
    modules : Names_spec.t [@default Ordered_set_lang.standard];

    (* [copy_config_from_core] -- not for casual use.
       Used only in the jbuild for [core/experimental] and [core/extended] to avoid
       duplication of some build setup *)
    copy_config_from_core : bool [@default false];

    (* [extra_disabled_warnings] contains a list of warnings numbers, in addition to those
       listed by [disabled_warning] in jenga/root.ml which are not to be treated as
       compilation errors. It is very uncommon to need to extend this set.*)
    extra_disabled_warnings : int sexp_list;

    (* [flags], [ocamlc_flags] and [ocamlopt_flags] are used to modify the flags passed to
       the ocaml compilers.

       [ocamlc_flags] for the byte compiler only.
       [ocamlopt_flags] for the native compiler only.
       [flags] for both.

       These fields are interpreted w.r.t standard settings defined in module [Top] in
       jenga/root.ml and can make use of extension and filtering features provided by
       [Ordered_set_lang.t]

       Examples:
           (ocamlopt_flags (-inline 0 -nodynlink))  // replace standard settings
           (flags (:standard -I +ocamldoc))         // append standard
           (flags (-I +ocamldoc :standard))         // prepend standard
           (flags (:standard \ -strict-sequence))   // filter standard
    *)
    flags : Ordered_set_lang.t sexp_option;
    ocamlc_flags : Ordered_set_lang.t sexp_option;
    ocamlopt_flags : Ordered_set_lang.t sexp_option;

    (* [ocaml_includes] adds -I flags to setup ocaml include paths. Deprecated. Can get
       the same behaviour using [flags].
         (ocaml_includes path1 path2) == (flags -I path1 -I path2) *)
    ocaml_includes : string sexp_list;

    (* Modify c and cxx flags.
       The [extra_] [avoid_] [replace_] allow the default flags to be extended, flitered
       or replaced.  This functionality pre-dates and is similar to [Ordered_set_lang]
       used for ocaml flags settings, but is less flexible and more clunky. *)
    extra_cflags : string sexp_list;
    avoid_cflags : string sexp_list;
    replace_cflags : string sexp_list;
    extra_cxxflags : string sexp_list;
    avoid_cxxflags : string sexp_list;
    (* [replace_cxxflags] does not exist, for no good reason *)

    (* [cxx_suf] overrides the suffix expected for C++ files. The default is .cpp *)
    cxx_suf : string option [@default None];

    (* [self_build_stubs_archive] - not for casual use.
       Only used in base/re2/lib/jbuild *)
    self_build_stubs_archive : string option [@default None];


    includes : string sexp_list;
    library_flags : string sexp_list;

    c_names : string sexp_list;
    cxx_names : string sexp_list;
    o_names : string sexp_list;

    preprocess : Preprocess_spec.t list [@default preprocess_default];
    preprocessor_deps : string sexp_list;
    (** Configuration for building and running inline tests *)
    inline_tests : Inline_tests.t [@default Inline_tests.default];
    ocamllex : string sexp_list;
    ocamlyacc : string sexp_list;
    cclibs : string sexp_list;
    skip_from_default : bool [@default false];
  } [@@deriving of_sexp, fields]

end

module Projections_check = struct
  type t =
    { allow : string list
    ; output_result_to : string sexp_option
    }
  [@@deriving of_sexp]
end

module Executables_conf = struct
  module Public_release = struct
    type t =
      { build             : string list [@default []]
      ; build_and_install : (string * string) list [@default []]
      ; extra_deps        : Library_conf.Public_release.extra_dep list  [@default []]
    }
    [@@deriving of_sexp]

    let default = t_of_sexp (List [])
  end

  type t = {
    (* Each element of [names] is an executable, without the ".exe" suffix. *)
    names : string list;
    public_release : Public_release.t [@default Public_release.default];
    projections_check : Projections_check.t sexp_option;
    allowed_ldd_dependencies : Ordered_set_lang.t sexp_option;
    extra_disabled_warnings : int sexp_list;
    flags : Ordered_set_lang.t sexp_option;
    ocamlc_flags : Ordered_set_lang.t sexp_option;
    ocamlopt_flags : Ordered_set_lang.t sexp_option;
    (* Immediate dependencies.  It is not necessary to name transitive dependencies or
       modules defined in the same directory.  (For the latter, see [modules] below.) *)
    libraries : string sexp_list;
    (* Extra include directories to pass to ocaml compiler via [-I]. *)
    extra_ocaml_includes : string sexp_list;
    ocamllex : string sexp_list;
    ocamlyacc : string sexp_list;
    preprocess : Preprocess_spec.t list [@default preprocess_default];
    preprocessor_deps : string sexp_list;
    ocamlpacks : Ordered_set_lang.t sexp_option;
    link_flags : string sexp_list;
    js_of_ocaml_flags : string sexp_list;
    (* Modules in this directory to include in these executables.  By default all modules
       are included.  If a single jbuild has multiple Executables_conf values, they must
       each specify a list of [modules], and those lists must be disjoint. *)
    modules : Names_spec.t [@default Ordered_set_lang.standard];
    (* [review_help] dumps the help output into a file so it can be checked in and
       reviewed.  It must default to false because the build system should not run random
       executables.  Not all executables follow the help conventions of Command.t. *)
    review_help : bool [@default false];
  } [@@deriving of_sexp]
end

module Jane_script_conf = struct
  type t = {
    (* jane_script_conf are interpreted only when NODYNLINK matches
       [only_when_NODYNLINK_is], because the normal configuration item requires most of
       the tree to be dynlinkable, which isn't true normally (with NODYNLINK=true). *)
    only_when_NODYNLINK_is : bool;
    libraries : string sexp_list;
    pps : string sexp_list;
  } [@@deriving of_sexp]
end

module Unified_tests = struct
  (* Jenga will generate a "run-unified-tests" script that can be used to manually run
     your tests. [setup_script] will be sourced before running the tests. The tests must
     be named test-XXX.t  *)
  type t =
    { target : string [@default "runtest"]
    ; deps : Dep_conf.t list
    ; setup_script : string sexp_option
    }
  [@@deriving of_sexp]
end

module Public_repo = struct
  type t =
    { dirs                : (string * Import.Path.t) list
    ; copyright_start     : int
    (* Additional files to copy. They must be from one of the directory in [dirs]. This is
       for when we want to export a generated file. *)
    ; additional_files    : Import.Path.t sexp_list
    (* Extra files to install. The filenames are relative to the external repository. *)
    ; install_extra_files : (Package_types.Package.Install_section.t
                             * Package_types.Package.File_to_install.t list) sexp_list
    (* This is only for ppx_type_conv *)
    ; install_extra_tags  : (Package_types.Package.Install_section.t * string list)
                              sexp_list
    }
  [@@deriving of_sexp]
end

module Jbuild = struct
  (* [Jbuild.t] describes the various kinds of build configuration descriptions.
     A jbuild file contains the sexp-representation of a list of [Jbuild.t]

     The most common items are [library] and [executables], example of which look like:

     (library
      ((name mylib)
       (libraries (core async))))

     (executables
      ((names (prog1 prog2))
       (libraries (core async mylib))))
  *)
  type t = [
  | `library of Library_conf.t
  | `executables of Executables_conf.t
  | `preprocessor of Preprocessor_conf.t
  | `libraryX of LibraryX_conf.t
  | `embed of Embed_conf.t
  | `jane_script of Jane_script_conf.t
  | `compile_c of Compile_c_conf.t
  | `rule of Rule_conf.t
  | `alias of Alias_conf.t
  | `no_utop
  | `unified_tests of Unified_tests.t
  | `switched_to_ppx_style (* Code in this directory has been switched to ppx_style *)
  | `translate_from_camlp4 (* Code in this directory is in camlp4-style, but should be
                              converted (on the fly) to ppx_style *)
  | `requires_camlp4 (* Code in this directory requires camlp4;
                        For example: it might use a preprocessor with no ppx replacement *)
  | `public_repo of Public_repo.t
  ]
  [@@deriving of_sexp]
end
