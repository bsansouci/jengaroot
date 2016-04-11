open Import
open Jbuild_types

let opam_switches = ["4.02.3"; "4.02.3+32bit"]

module Make(Jenga_root : sig
    module LN : sig
      include Identifiable.S
    end

    module Libmap : sig
      type t
      val look_exn : t -> LN.t -> Path.t
    end

    module DC : sig
      type t
      val libmap : t -> Libmap.t
    end

    module BN : sig
      include Identifiable.S
      val to_module : t -> string
    end

    module Lib_modules : sig
      type t
      val impls_and_intfs : t -> BN.t list
      val load : dir:Path.t -> libname:LN.t -> t Dep.t
    end

    module Standard_pp_sets : sig
      val extract : string list -> string list
      val expand : string list -> string list
    end

    module PP : sig
      include Identifiable.S
    end

    module User_or_gen_config : sig
      val load : dir: Path.t -> Jbuild.t list Dep.t
    end

    val public_release_files : Path.t
    val deep_unignored_subdirs : dir:Path.t -> Path.t list Dep.t
    val libs_for_code_generated_by_pp : PP.t -> LN.t list
    val ocaml_libraries : [< Jbuild.t ] -> LN.t list
    val pps_of_jbuild : DC.t -> [< Jbuild.t ] -> LN.t list
    val expand_pps : string list -> PP.t list * string list
    val remap_pa_names : PP.t list -> string list
  end) : sig
  open Jenga_root

  val rules : DC.t -> dir:Path.t -> Public_repo.t -> Rule.t list

  (** Directory containing all public repo definitions *)
  val repos : Path.t

  module Package_map : sig
    (** Rules to build .package-map.sexp *)
    val global_rules : dir:Path.t -> Rule.t list
  end

end = struct
  open Jenga_root

  module T = Package_types

  (* Where the descriptions of external packages are *)
  let repos = root_relative "public-release/repos"

  (* Path to an external package *)
  let repo_path name = relative ~dir:repos name

  (* Files included in all packages *)
  let public_release_common_files = root_relative "public-release/common-files"

  (* The final tarball *)
  let tarball ~dir = relative ~dir "dist.tar.gz"

  (* List of files to copy in the given package *)
  let files_to_copy_filename = ".files-to-copy"
  let files_to_copy_path ~dir = relative ~dir files_to_copy_filename

  let escape_for_grep =
    let module S = struct type t = Ok | Invalid | Escape end in
    let char_status : char -> S.t = function
      | '.' | '?' | '*' | '+' | '[' | ']' | '\\' | '{' | '}' -> Escape
      | '\033' .. '\127' -> Ok
      | _ -> Invalid
    in
    fun word ->
      if String.for_all word ~f:(fun ch -> char_status ch = Ok) then
        word
      else
        String.concat_map word ~f:(fun ch ->
          match char_status ch with
          | Escape  -> sprintf "\\%c" ch
          | Ok      -> String.of_char ch
          | Invalid -> raise_s [%sexp "can't escape this word", ~~(word : string)])

  let or_regexp_for_grep words =
    List.map words ~f:escape_for_grep
    |> String.concat ~sep:"|"

  (* Map directories from the jane source tree to the public package they are part of (if
     any). *)
  module Package_map : sig
    val load : string Path.Map.t Dep.t

    (* List of directories in the jane repo that will be exported in the given package. *)
    val dirs_exported_by_package : package:string -> Path.t list Dep.t

    val jbuilds_rule : dir:Path.t -> conf:Public_repo.t -> Rule.t
    val global_rules : dir:Path.t -> Rule.t list
  end = struct
    (* List of jbuilds that a given external packages looks at *)
    let jbuilds_filename = ".jbuilds"
    let jbuilds_path ~dir = relative ~dir jbuilds_filename
    let jbuilds_rule ~dir ~(conf : Public_repo.t) =
      let target = jbuilds_path ~dir in
      Rule.create ~targets:[target] (
        Dep.both
          (Dep.path public_release_files)
          (Dep.List.concat_map conf.dirs ~f:(fun (_, dir) -> deep_unignored_subdirs ~dir))
        *>>| fun ((), unignored_subdirs) ->
        bashf ~dir:Path.the_root
          !"egrep '^(%s)/jbuild$' %{Path} > %{Path}"
          (List.map unignored_subdirs ~f:Path.to_string |> or_regexp_for_grep)
          public_release_files
          target
      )

    let dirs_exported_by_package ~package =
      Dep.contents (jbuilds_path ~dir:(repo_path package))
      *>>| fun s ->
      String.split_lines s
      |> List.map ~f:root_relative
      |> List.map ~f:Path.dirname

    let package_map_file = relative ~dir:repos ".package-map.sexp"

    let load =
      Dep.contents package_map_file
      *>>| fun s ->
      Sexp.of_string_conv_exn (String.strip s) [%of_sexp: string Path.Map.t]

    let all_repos_file = relative ~dir:repos ".repos"

    let global_rules ~dir =
      assert (Path.equal dir repos);
      let create_all_repos_file =
        Dep.path public_release_files
        *>>| fun () ->
        bashf ~dir:Path.the_root
          !"sed -nr 's|^(%s/[^/]*)/jbuild$|\\1|p' %{Path} > %{Path}"
          (Path.to_string repos |> escape_for_grep)
          public_release_files
          all_repos_file
      in
      let create_package_map =
        Dep.contents all_repos_file
        *>>= fun s ->
        let all_packages =
          List.map (String.split_lines s) ~f:Filename.basename
        in
        Dep.all (List.map all_packages ~f:(fun package ->
          dirs_exported_by_package ~package
          *>>| fun dirs ->
          List.map dirs ~f:(fun dir -> (dir, package))))
        *>>| fun l ->
        let l = List.concat l in
        let map =
          match Path.Map.of_alist l with
          | `Ok map -> map
          | `Duplicate_key dir ->
            let pkgs =
              List.filter_map l ~f:(fun (d, p) ->
                if Path.equal dir d then Some p else None)
            in
            failwiths "directory present in several packages"
              [%sexp
                { dir      = (dir  : Path.t     )
                ; packages = (pkgs : string list)
                }
              ] Fn.id
        in
        Action.save ~target:package_map_file
          (Sexp.to_string_hum ([%sexp_of: string Path.Map.t] map))
      in
      [ Rule.create ~targets:[package_map_file] create_package_map
      ; Rule.create ~targets:[all_repos_file  ] create_all_repos_file
      ]
  end

  (* Build the .metadata.sexp file, which is used both for creating the tarballs and get
     find dependencies between external packages. *)
  module Metadata : sig
    val path : package:string -> Path.t
    val load : package:string -> T.Package.t Dep.t
    val rule
      :  DC.t
      -> package:string
      -> conf:Public_repo.t
      -> Rule.t
  end = struct
    let filename = ".metadata.sexp"
    let path ~package = relative ~dir:(repo_path package) filename

    let lib_conf ~dir libname =
      User_or_gen_config.load ~dir *>>| fun jbuilds ->
      List.find_map_exn jbuilds
        ~f:(function
          | `library ({ name; _ } as lib)
            when name = LN.to_string libname ->
            Some lib
          | _ -> None)

    let dependency dc ~of_pkg ~of_dir package_map context libname =
      let dir = Libmap.look_exn (DC.libmap dc) libname in
      lib_conf ~dir libname *>>= fun lib ->
      let (kind, internal) : T.Buildable.Dependency.Kind.t * bool =
        match lib.public_release with
        | External { package; _ } ->
          (Global package, false)
        | Internal { status; _ } ->
          let fail msg sexp = failwiths msg sexp Fn.id in
          match status, Map.find package_map dir with
          | Ignore, _ ->
            fail "Library required but not buildable in the public release"
              [%sexp { dir     = (of_dir : Path.t)
                     ; require = `internal_library_name (libname : LN.t)
                     }]
          | (Build | Build_and_install_as _), None ->
            fail "Library required but not part of any released package"
              [%sexp { dir     = (of_dir : Path.t)
                     ; require = `internal_library_name (libname : LN.t)
                     }]
          | Build_and_install_as ocamlfind_package, Some opam_package ->
            if opam_package = of_pkg then
              (Local { internal_name     = LN.to_string libname
                     ; ocamlfind_package = Some ocamlfind_package
                     },
               true)
            else
              (Global { ocamlfind_package; opam_package }, true)
          | Build, Some opam_package ->
            if opam_package = of_pkg then
              (Local { internal_name     = LN.to_string libname
                     ; ocamlfind_package = None
                     },
               true)
            else
              fail "Library required from different package but not installed"
                [%sexp { dir          = (of_dir : Path.t)
                       ; require      = `internal_library_name (libname : LN.t)
                       ; from_package = (opam_package : string)
                       }]
      in
      let lib_kind =
        match lib.public_release with
        | Internal x -> x.kind
        | External _ -> Normal
      in
      let dep : T.Buildable.Dependency.t =
        { kind
        ; context
        ; internal
        ; lib_kind
        ; wrapper = None
        }
      in
      let virtual_opam_deps =
        match lib.public_release with
        | External e -> List.map e.virtual_opam_deps ~f:(fun x -> (context, x))
        | Internal _ -> []
      in
      let externally_wrapped =
        match lib.public_release with
        | Internal _ | External { wrapped = Default; _ } -> lib.wrapped
        | External { wrapped = True ; _ } -> true
        | External { wrapped = False; _ } -> false
      in
      match lib.wrapped, externally_wrapped with
      | true, true | false, false -> return (dep, virtual_opam_deps)
      | false, true ->
        failwiths "this internal/external wrapped pattern is not supported"
          [%sexp
            { library = (libname : LN.t)
            ; internally_wrapped = (lib.wrapped : bool)
            ; externally_wrapped = (externally_wrapped : bool)
            }]
          Fn.id
      | true, false ->
        Lib_modules.load ~dir ~libname *>>| fun modules ->
        let wrapper =
          Some (LN.to_string libname,
                List.map (Lib_modules.impls_and_intfs modules) ~f:BN.to_module)
        in
        ({ dep with wrapper }, virtual_opam_deps)

    let sub_dir_in_dest ~(conf : Public_repo.t) path =
      List.find_map_exn conf.dirs ~f:(fun (sub_dir, dir) ->
        if Path.is_descendant ~dir path then
          Some (let d = sub_dir ^/ reach_from ~dir path in
                let d =
                  if String.is_prefix d ~prefix:"./" then
                    String.slice d 2 (String.length d)
                  else
                    d
                in
                if String.is_suffix d ~suffix:"/." then
                  String.slice d 0 (String.length d - 2)
                else
                  d)
        else
          None)

    type thing_to_export =
      | Library    of T.Library.t
      | Executable of T.Executable.t

    let make_buildable dc ~package_map ~pkg_name
          ~(jbuild     : [ `library     of Library_conf.t
                         | `executables of Executables_conf.t ])
          ~(extra_deps : Library_conf.Public_release.extra_dep list)
          ~(conf       : Public_repo.t) ~dir () =
      let module D = T.Buildable.Dependency in

      (* If it is a ppx, find its runtime deps *)
      let rt_deps =
        match jbuild with
        | `library { name; _ } -> libs_for_code_generated_by_pp (PP.of_string name)
        | `executables _ -> []
      in

      let dependency = dependency dc ~of_dir:dir ~of_pkg:pkg_name package_map in
      let deps =
        (ocaml_libraries jbuild @ pps_of_jbuild dc jbuild)
        |> LN.Set.of_list
        |> LN.Set.to_list
      in
      Dep.all (List.map deps    ~f:(dependency D.Context.Both   ) @
               List.map rt_deps ~f:(dependency D.Context.Runtime))
      *>>= fun deps_and_virtual_opam_deps ->
      let dependencies, virtual_opam_deps =
        Tuple.T2.map_snd (List.unzip deps_and_virtual_opam_deps) ~f:List.concat
      in

      let preprocess, preprocessor_deps =
        match jbuild with
        | `library     { preprocess; preprocessor_deps; _ }
        | `executables { preprocess; preprocessor_deps; _ } ->
          (preprocess, preprocessor_deps)
      in

      let has_ppx_jane, other_ppxs =
        match preprocess with
        | [ (`pps pps, _) ] ->
          let pps =
            expand_pps pps
            |> fst
            |> remap_pa_names
            |> Standard_pp_sets.extract
          in
          let has_ppx_jane, others =
            if List.mem pps "JANE" then
              (true, List.filter pps ~f:((<>) "JANE"))
            else
              (false, pps)
          in
          (has_ppx_jane,
           Standard_pp_sets.expand others
           |> List.map ~f:LN.of_string)
        | _ -> (false, [])
      in
      Dep.all (List.map other_ppxs ~f:(dependency D.Context.Build))
      *>>| fun ppxs ->
      let ppxs =
        List.map ppxs ~f:(fun (ppx, virt_deps) -> assert (virt_deps = []); ppx)
      in
      let js_ppxs =
        List.filter_map ppxs ~f:(fun dep ->
          match dep.kind with
          | Local _  -> None
          | Global g -> Some g)
      in
      let js_ppxs =
        if not has_ppx_jane then
          js_ppxs
        else
          { ocamlfind_package = "ppx_jane"; opam_package = "ppx_jane" }
          :: js_ppxs
      in

      let extra_deps =
        let ext_dep pkg = D.Kind.Global (D.Global.of_ocamlfind_package pkg) in
        List.concat
          [ List.map extra_deps ~f:(fun { context; package } ->
              (context, D.Kind.Global package))
          ; if List.exists preprocess ~f:(fun (k, _) -> k = `metaquot) then
              [ (Build, ext_dep "ppx_tools.metaquot") ]
            else
              []
          ]
        |> List.map ~f:(fun (context, kind) ->
          { T.Buildable.Dependency.
            kind
          ; context
          ; lib_kind = Normal
          ; internal = false
          ; wrapper  = None
          })
      in
      let dependencies = dependencies @ extra_deps in

      let preprocessor_deps =
        List.map preprocessor_deps ~f:Filename.basename
      in

      ({ T.Buildable.
         path = sub_dir_in_dest ~conf dir
       ; dependencies
       ; js_ppxs
       ; preprocessor_deps
       ; copy_config_from_core =
           match jbuild with
           | `library     l -> l.copy_config_from_core
           | `executables _ -> false
       },
       virtual_opam_deps)

    let things_to_export dc ~package_map ~pkg_dir ~(conf : Public_repo.t) =
      let pkg_name = Path.basename pkg_dir in
      Package_map.dirs_exported_by_package ~package:pkg_name
      *>>= fun exported_dirs ->
      Dep.List.concat_map exported_dirs ~f:(fun dir ->
        User_or_gen_config.load ~dir *>>= fun jbuilds ->
        Dep.all @@ List.filter_map jbuilds ~f:(function
          | `library { name; public_release = External _; _ }  ->
            failwiths "External libraries cannot be released"
              [%sexp { library = (name     : string)
                     ; package = (pkg_name : string)
                     }]
              Fn.id
          | `library { public_release = Internal { status = Ignore; _ }; _ } -> None
          | `library { public_release = Internal info
                     ; name
                     ; c_names
                     ; wrapped
                     ; _ } as jbuild -> Some (
            let install_as =
              match info.status with
              | Build_and_install_as ocamlfind_package ->
                if T.opam_of_ocamlfind ocamlfind_package <> pkg_name then
                  failwiths
                    "Cannot install a library as an ocamlfind package that \
                     is not a children of the public package name"
                    [%sexp
                      { library           = (name              : string              )
                      ; ocamlfind_package = (ocamlfind_package : string              )
                      ; package           = (pkg_name          : string              )
                      }]
                    Fn.id
                else
                  Some ocamlfind_package
              | _ -> None
            in

            let libname = LN.of_string name in

            Dep.glob_listing (Glob.create ~dir ~kinds:[`File] "*.h")
            *>>= fun h_files ->
            let h_files =
              List.map h_files ~f:Path.basename
              |> List.filter ~f:(fun s -> not (String.is_prefix ~prefix:"utop." s))
            in
            let c_files = h_files @ List.map c_names ~f:(fun s -> s ^ ".c") in
            (* Special case for re2 *)
            let c_files = if name = "re2" then [] else c_files in

            Lib_modules.load ~dir ~libname
            *>>= fun modules ->
            let modules = Lib_modules.impls_and_intfs modules in

            make_buildable dc ~conf ~package_map ~dir ~pkg_name ~jbuild
              ~extra_deps:info.extra_deps ()
            *>>| fun (buildable, virtual_opam_deps) ->
            (buildable, virtual_opam_deps,
              [Library
                 { internal_name = name
                 ; kind          = info.kind
                 ; wrapped       = wrapped
                 ; short_desc    = info.desc
                 ; modules       = List.map modules ~f:BN.to_module
                 ; path          = buildable.path
                 ; install_as
                 ; c_files
                 }]))
          | `executables { public_release = { build = []; build_and_install = []; _ }
                         ; _ } ->
            None
          | `executables { public_release = info
                         ; _ } as jbuild -> Some (
            make_buildable dc ~conf ~dir ~pkg_name ~jbuild ~package_map
              ~extra_deps:info.extra_deps ()
            *>>| fun (buildable, virtual_opam_deps) ->
            (buildable, virtual_opam_deps,
             (List.map info.build             ~f:(fun  a     -> (a, None)) @
              List.map info.build_and_install ~f:(fun (a, b) -> (a, Some b)))
             |> List.map ~f:(fun (main_module, install_as) ->
               (Executable
                  { main_module
                  ; install_as
                  ; path = buildable.path
                  }))))
          | _ -> None))

    let rule dc ~package ~conf =
      let file = path ~package in
      let dir = repo_path package in
      Rule.create ~targets:[file] (
        Package_map.load
        *>>= fun package_map ->
        Dep.both
          (things_to_export dc ~package_map ~pkg_dir:dir ~conf)
          (Dep.contents (relative ~dir "descr"))
        *>>| fun (stuff, descr) ->
        let buildables, virtual_opam_deps, things =
          List.fold stuff ~init:([], [], []) ~f:(fun (acc1, acc2, acc3) (x1, x2, x3) ->
            (x1 :: acc1, x2 @ acc2, x3 @ acc3))
        in
        let libraries, executables =
          List.partition_map things ~f:(function
            | Library    lib -> `Fst lib
            | Executable exe -> `Snd exe)
        in
        let libraries =
          List.sort libraries ~cmp:(fun (a : T.Library.t) b ->
            String.compare a.internal_name b.internal_name)
        in
        let synopsis, long_description =
          match String.split_lines descr with
          | x :: l -> (x, l)
          | [] -> ("<no description>", [])
        in
        let buildables =
          List.map buildables ~f:(fun b -> (b.path, b))
          |> String.Map.of_alist_reduce ~f:(fun b1 b2 ->
            if b1 = b2 then
              b1
            else
              failwiths
                "Incompatible buildables for path"
                [%sexp
                   { package    = (package : string)
                   ; buildable1 = (b1 : T.Buildable.t)
                   ; buildable2 = (b2 : T.Buildable.t)
                   }]
                Fn.id)
          |> Map.data
        in
        let dependencies =
          List.concat_map buildables ~f:(fun b ->
            List.filter_map b.dependencies ~f:(fun dep ->
              match dep.kind with
              | Local _ -> None
              | Global { opam_package; _ } ->
                Some { T.Package.Dependency.
                       opam_package
                     ; internal     = dep.internal }) @
            List.map b.js_ppxs      ~f:(fun ppx ->
              { T.Package.Dependency.
                opam_package = ppx.opam_package
              ; internal     = true }))
        in
        let dependencies =
          if List.for_all buildables ~f:(fun b -> b.js_ppxs = []) then
            dependencies
          else
            { opam_package = "ppx_driver"; internal = true } :: dependencies
        in
        let dependencies =
          dependencies @ List.map virtual_opam_deps ~f:(fun (_context, pkg) ->
            { T.Package.Dependency.
              opam_package = pkg
            ; internal     = false
            })
        in
        let dependencies =
          let module S =
            Set.Make(struct
              type t = T.Package.Dependency.t [@@deriving sexp]
              let compare (a : t) (b : t) =
                let d = String.compare a.opam_package b.opam_package in
                if d = 0 then assert (a.internal = b.internal);
                d
            end)
          in
          S.of_list dependencies
          |> S.to_list
          |> List.filter ~f:(fun dep -> dep.opam_package <> "ocaml")
        in
        let name = Path.basename dir in
        let pkg : T.Package.t =
          { name
          ; synopsis
          ; long_description
          ; libraries
          ; executables
          ; buildables
          ; dependencies
          ; copyright_start     = conf.copyright_start
          ; dir_mapping         = List.map conf.dirs ~f:(Tuple.T2.map_snd ~f:Path.to_string)
          ; file_list_filename  = files_to_copy_path ~dir      |> Path.to_absolute_string
          ; tarball_filename    = tarball ~dir                 |> Path.to_absolute_string
          ; install_extra_files = conf.install_extra_files
          ; install_extra_tags  = conf.install_extra_tags
          }
        in
        Action.save ~target:file (pkg |> T.Package.sexp_of_t |> Sexp.to_string_hum)
      )

    let load ~package =
      Dep.contents (path ~package)
      *>>| fun s ->
      Sexp.of_string_conv_exn (String.strip s) [%of_sexp: T.Package.t]
  end

  let sed_script = root_relative "public-release/sed-manifest-files"

  let files_to_copy_rule ~dir ~(conf : Public_repo.t) =
    let target = files_to_copy_path ~dir in
    Rule.create ~targets:[target] (
      Dep.all_unit
        [ Dep.path sed_script
        ; Dep.path public_release_files
        ]
      *>>| fun () ->
      bashf ~dir:Path.the_root
        !"{ egrep '^(%s)/' %{Path} | sed -rf %{Path}; echo -ne '%s'; } > %{Path}"
        (List.map conf.dirs ~f:(fun (_, p) -> Path.to_string p) |> or_regexp_for_grep)
        public_release_files
        sed_script
        (List.map conf.additional_files ~f:(fun p -> Path.to_string p ^ "\\n")
         |> String.concat)
        target
    )

  let bin = root_relative "public-release/bin"
  let fe_bin = relative ~dir:Path.the_root "app/fe/bin/fe.exe"
  let create_tarball = relative ~dir:bin "create_tarball.exe"

  let opam_package_installed ~pkg ~switch =
    ksprintf root_relative "public-release/build/.opam-%s-%s-installed" switch pkg
  let bin_tarball_filename ~switch = sprintf "bin.%s.lzo" switch
  let bin_tarball_path ~dir ~switch = relative ~dir (bin_tarball_filename ~switch)

  let bin_checksum_filename ~switch = bin_tarball_filename ~switch ^ ".md5sum"
  let bin_checksum_path ~dir ~switch = relative ~dir (bin_checksum_filename ~switch)

  let build_repo = root_relative "public-release/bin/build_repo.exe"

  let common_external_deps =
    [ "ocamlfind"
    ; "oasis"
    ]

  let build_rule ~dir ~switch =
    let bin_tarball =  bin_tarball_path ~dir ~switch in
    Rule.create ~targets:[bin_tarball] (
      Metadata.load ~package:(basename dir)
      *>>= fun pkg ->
      let filter_deps packages =
        String.Set.of_list packages
        |> Set.elements
        |> List.filter ~f:((<>) pkg.name)
      in
      let internal_deps, external_deps =
        List.partition_map pkg.dependencies ~f:(fun d ->
          if d.internal then
            `Fst d.opam_package
          else
            `Snd d.opam_package)
      in
      let external_deps = common_external_deps @ external_deps in
      let internal_deps = filter_deps internal_deps
      and external_deps = filter_deps external_deps in
      Dep.all_unit
        (List.concat
           [ [ Dep.path (tarball ~dir)
             ; Dep.path build_repo
             ]
           ; List.map internal_deps ~f:(fun pkg ->
               (* Depend on the checksum file rather than the file itself as the archive
                  contains file times. *)
               Dep.path (bin_checksum_path ~dir:(repo_path pkg) ~switch))
           ; List.map external_deps ~f:(fun pkg ->
               Dep.path (opam_package_installed ~pkg ~switch))
           ])
      *>>| fun () ->
      Action.process ~dir:Path.the_root (Path.to_string build_repo)
        ["-quiet"; "-switch"; switch; pkg.name]
    )

  let rules dc ~dir (conf : Public_repo.t) =
    let tarball = tarball ~dir in
    let conf =
      { conf with
        dirs =
          List.concat
            [ [ (".", public_release_common_files) ]
            ; conf.dirs
            ; [ (".", dir) ]
            ]
      }
    in
    let pkg_name = basename dir in
    let create_tarball =
      Rule.create ~targets:[tarball] (
        let files_to_copy = files_to_copy_path ~dir in
        Dep.all_unit
          [ Dep.path create_tarball
          ; Dep.path fe_bin
          ; Dep.path (Metadata.path ~package:pkg_name)
          ]
        *>>= fun () ->
        Dep.contents files_to_copy
        *>>= fun s ->
        Dep.all_unit (List.map (String.split_lines s) ~f:(fun s ->
          Dep.path (root_relative s)))
        *>>| fun () ->
        bashf ~dir:Path.the_root
          !"%{Path} %{Path}" create_tarball (Metadata.path ~package:pkg_name)
      )
    in
    let bin_checksum ~switch =
      let bin_tarball  = bin_tarball_path  ~dir ~switch in
      let bin_checksum = bin_checksum_path ~dir ~switch in
      Rule.create ~targets:[bin_checksum]
        (Dep.path bin_tarball
         *>>| fun () ->
         (* Don't run md5sum on bin.lzo as it contains file times. Ignore warning because
            lzop warns about the fact that the archive contains several files. *)
         bashf ~dir !"lzop --no-warn --ignore-warn -d -c %s \
                      | md5sum \
                      | cut -d%{quote} -f1 > %s"
           (Path.basename bin_tarball) " " (Path.basename bin_checksum))
    in
    List.concat
      [ [ create_tarball
        ; files_to_copy_rule ~dir ~conf
        ; Package_map.jbuilds_rule ~dir ~conf
        ; Metadata.rule
            dc
            ~package:pkg_name
        ~conf
        ]
      ; List.concat_map opam_switches ~f:(fun switch ->
          [ bin_checksum ~switch
          ; build_rule ~dir ~switch
          ])
      ]
end
