open Import
open Jbuild_types

let bin_relative = relative ~dir:Config.bin_dir

let ppx_dir = root_relative "ppx"
let ppx_relative = relative ~dir:ppx_dir

(**
   Historically our use of [pack]ed libraries provides a namespace
   mechanism to allow the same module name to exist in different
   libraries, without requiring explicit prefixing by the
   programmer. This goal remains. The nopack build rules achieve this
   goal via the implicit prefixing of a library name on every module it
   contains. So [foo.ml] in library [mylib] is compiled to an ocaml unit
   [Mylib__Foo] written to files: [mylib__Foo.cmx] etc.

   The following shows the compilation commands run for a small library
   [mylib] containing files: foo.ml{i,} bar.ml for both settings of
   PACKING.

   PACKING=true (old scheme):

       Compile:
           foo.cmi :
               ocamlc.opt -for-pack Mylib -c foo.mli

           foo.{cmx,o} :
               ocamlopt.opt -for-pack Mylib -c foo.ml

           bar.{cmi,cmx,o} :
               ocamlopt.opt -for-pack Mylib -c bar.ml

       Pack:
           mylib.cmi mylib.cmx mylib.o :
               ocamlopt.opt -pack -o mylib.cmx foo.cmx bar.cmx

       Archive:
           mylib.{a,cmxa} :
               ocamlopt.opt mylib.cmx -a -o mylib.cmxa

   PACKING=false (new scheme):

       Generate & compile renaming wrapper:
           mylib.ml-gen :
               module Foo = Mylib__Foo
               module Bar = Mylib__Bar

           mylib.cmi mylib.cmx mylib.o :
               ocamlopt.opt -w @a-49 -no-alias-deps -c -impl mylib.ml-gen

       Compile:
           mylib__Foo.cmi :
               ocamlc.opt -o mylib__Foo -no-alias-deps -open Mylib -c foo.mli

           mylib__Foo.{cmx,o} :
               ocamlopt.opt -o mylib__Foo -no-alias-deps -open Mylib -c foo.ml

           mylib__Bar.{cmi,cmx,o} :
               ocamlopt.opt -o mylib__Bar -no-alias-deps -open Mylib -c bar.ml

       Archive:
           mylib.{a,cmxa} :
               ocamlopt.opt mylib__Foo.cmx mylib__Bar.cmx mylib.cmx -a -o mylib.cmxa

   The `renaming' wrapper [mlib.ml-gen] is the only place ocaml code
   makes reference to modules under their prefixed name. An explicit
   requirement of the new scheme is that no `user' ocaml code should ever
   have to reference an ocaml module under its prefixed name.
*)

(* THE control for whether prefixing is used in bin-dirs *)

let wrapped_bindirs = false

let remove_dups_preserve_order xs =
  let set = String.Hash_set.create ()  in
  let rec loop acc = function
    | [] -> List.rev acc
    | x::xs ->
      if Hash_set.mem set x
      then loop acc xs
      else (Hash_set.add set x; loop (x::acc) xs)
  in
  loop [] xs

let remove_dups_and_sort xs =
  String.Set.to_list (String.Set.of_list xs)

module List = struct
  include List
  let concat_cartesian_product l1 l2 =
    List.map (List.cartesian_product l1 l2) ~f:(fun (x, y) -> x ^ y)
end

let put = Misc.put

let dummy_position path =
  { Lexing.pos_fname = Path.to_string path; pos_cnum = 0; pos_bol = 0; pos_lnum = 1 }
;;
let failposf : pos:Lexing.position -> ('a, unit, string, unit -> 'b) format4 -> 'a =
  fun ~pos fmt ->
    let {Lexing.pos_fname; pos_lnum; pos_cnum; pos_bol} = pos in
    let col = pos_cnum - pos_bol in
    ksprintf (fun s ->
      let fname =
        Path.to_absolute_string (Path.relative_or_absolute ~dir:Path.the_root pos_fname)
      in
      put "File %S, line %d, characters %d-%d:"
        fname pos_lnum col col;
      put "Error: %s" s;
      failwithf "%s: %s" pos_fname s ();
    ) fmt

let failheref : Lexing.position -> ('a, unit, string, unit -> 'b) format4 -> 'a =
  fun here fmt ->
    let pos = {here with pos_fname = "jenga/root.ml"} in
    failposf ~pos fmt

let path_remove_dups_and_sort xs =
  Path.Set.to_list (Path.Set.of_list xs)

let simple_rule ~targets ~deps ~action =
  Rule.create ~targets (
    Dep.all_unit deps *>>| fun () -> action
  )

let relative_rule ~dir ~targets ~deps ~non_relative_deps monadic_action =
  let targets = List.map targets ~f:(fun name -> Path.relative ~dir name) in
  let deps =
    non_relative_deps @
      List.map deps ~f:(fun name -> Dep.path (Path.relative ~dir name))
  in
  Rule.create ~targets (Dep.all_unit deps *>>= fun () -> monadic_action)

let alias_dot_filename_hack ~dir dot_name =
  (* Sadly names given on the command line which begin with a dot (i.e. ".merlin") are
     currently always interpreted as a reference to an alias. Workaround this problem for
     specific instances by creating an alias to the dot-filename, named the same as the
     filename (minus the dot). *)
  let name = String.chop_prefix_exn dot_name ~prefix:"." in
  Rule.alias (Alias.create ~dir name) [Dep.path (relative ~dir dot_name)]

module Glob = struct
  include Glob
  let of_string ~dir s =
    let path_ish =
      (* This is not actually a path, but a relative glob-string.  We use use [relative]
         to prepend [dir] onto any leading relative-path in the string [s], before
         splitting into the dirname/basename components. *)
      Path.relative ~dir s
    in
    Glob.create ~dir:(dirname path_ish) (basename path_ish)
end

let lines_of_string string =
  let lines = String.split string ~on:'\n' in
  let lines = List.filter lines ~f:(function | "" -> false | _ -> true) in
  lines

let uncomment_line s =
  match String.lsplit2 s ~on:'#' with None -> s | Some (s,_comment) -> s

let uncomment s =
  String.concat ~sep:"\n" (List.map ~f:uncomment_line (String.split ~on:'\n' s))

let file_words path =
  Dep.contents path *>>| words_of_string

let file_words_allow_commments path =
  Dep.contents path *>>| uncomment *>>| words_of_string

module Alias1 = struct
  include Alias
  let of_string ~dir s =
    let path_ish =
      (* This is not actually a path, but a relative alias-string.  We use use [relative]
         to prepend [dir] onto any leading relative-path in the string [s], before
         splitting into the dirname/basename components. *)
      Path.relative ~dir s
    in
    Alias.create ~dir:(dirname path_ish) (basename path_ish)
end

module Alias = struct
  include Alias1
  let c ~dir = Alias.create ~dir "c"
  let default ~dir = Alias.create ~dir "DEFAULT"
  let runtest ~dir = Alias.create ~dir "runtest"
  let qtest ~dir = Alias.create ~dir "qtest"
  let pp ~dir = Alias.create ~dir "pp"
  let diff ~dir = Alias.create ~dir "diff" (* validate no camlp4/ppx diff *)
  let runbench ~dir = Alias.create ~dir "runbench"
  let libdeps ~dir = Alias.create ~dir "libdeps"
  let merlin ~dir = Alias.create ~dir "merlin"
  let utop ~dir = Alias.create ~dir "utop"
  (* aliases not intended to be recursive.. *)
  let lib_artifacts ~dir = Alias.create ~dir "lib_artifacts"
  (* These aliases are used to group and share the dependencies on *.cmi or *.cmx for a
     given library, because we need a lot of these dependencies (one per ml/mli), and they
     are big (number of modules in all the libraries the source file depends on). *)
  let submodule_cmis ~dir = Alias.create ~dir "submodule_cmis"
  let submodule_cmxs ~dir = Alias.create ~dir "submodule_cmxs"
  let api ~dir = Alias.create ~dir "api"
end

let read_sexp_throttle =
  (* The throttle is to avoid exceeding fd limits. *)
  let open Async.Std in
  assert (Thread_safe.am_holding_async_lock ());
  Throttle.create ~continue_on_error:true ~max_concurrent_jobs:32

let read_then_convert_string_via_reader :
    (
      path : Path.t ->
      contents : (Path.t -> string Dep.t) ->
      do_read : (Async.Std.Reader.t -> 'a Async.Std.Deferred.t) ->
      'a Dep.t
    ) =
  fun ~path ~contents ~do_read ->
    let open Async.Std in
    contents path *>>= fun string ->
    Dep.deferred (fun () ->
      try_with (fun () ->
        let outcome_ivar = Ivar.create () in
        Throttle.enqueue read_sexp_throttle (fun () ->
        (* This trick enables the reading of a sexp from a string using [Reader]'s
           parsing functions.  This is easier than using the low-level [Sexplib]
           interface (partially since we don't want to use the lex/yacc-based
           implementation, since it blocks, and the interface not using that is more
           involved). *)
          let info = Info.of_string ("Description.convert_using_reader " ^ Path.to_string path) in
          let pipe = Pipe.init (fun writer -> Pipe.write writer string) in
          Reader.of_pipe info pipe >>= fun reader ->
          do_read reader >>= fun outcome ->
          Reader.close reader >>| fun () ->
          Ivar.fill outcome_ivar outcome
        ) >>= fun () ->
        Ivar.read outcome_ivar
      ) >>| function
      | Ok x -> x
      | Error exn ->
        put "File %S, line 1, characters 1-1:" (Path.to_string path);
        put "Error: sexp conversion\n%s" (Exn.to_string exn);
        failwithf "%S: sexp conversion" (Path.to_string path) ()
    )


let glob_ml = Glob.create "*.ml"
let glob_mli = Glob.create "*.mli"

(** Returns a list of options you need to give to ocaml compiler
    so it passes [args] to the c compiler at link-time. *)
let link_time_args_for_c_compiler = function
  | [] -> []
  | _ :: _ as args ->
    (* note that [-cclib], unlike [-ccopt], expects a whitespace-separated list of
       arguments, not a shell command string, so we can't use [concat_quoted]. *)
    assert (not (List.exists args ~f:(String.exists ~f:(Char.is_whitespace))));
    [ "-cclib"; String.concat ~sep:" " args ]

let args_for_linker args =
  link_time_args_for_c_compiler
    (List.concat_map args ~f:(fun arg ->
       ["-Xlinker"; arg]))

let ccopts = function
  | [] -> []
  | _ :: _ as l -> [ "-ccopt"; concat_quoted l ]
;;

let use_compiler_flavor flavor =
  (* Using g++ instead of gcc to link in the c++ stdlib if needed. *)
  (* note that [-cc] and [-ccopt] expect shell syntax, which is why we do quoting here *)
  "-cc" :: quote (C.Flavor.prog flavor)
  :: ccopts Compiler_selection.arch_cflags

(*----------------------------------------------------------------------
 getenv...
----------------------------------------------------------------------*)

let version_util_support =
  Misc.getenv_bool "VERSION_UTIL_SUPPORT" ~default:true

let link_executables =
  Misc.getenv_bool "LINK_EXECUTABLES" ~default:true

let x_library_inlining =
  Misc.getenv_bool "X_LIBRARY_INLINING" ~default:false

let with_utop =
  Misc.getenv_bool "WITH_UTOP" ~default:false

let nodynlink =
  Misc.getenv_bool "NODYNLINK" ~default:true

let bin_annot =
  Misc.getenv_bool "BIN_ANNOT" ~default:true

let select_lib_packing =
  Misc.getenv_bool "PACKING" ~default:false

let transitive_runners =
  Misc.getenv_bool "TRANSITIVE_RUNNERS" ~default:false

let use_new_sqml =
  Misc.getenv_bool "USE_NEW_SQML" ~default:false

let build_info_app_fields =
  Option.map (Sys.getenv "BUILD_INFO_APP_FIELDS") ~f:(fun s ->
    Sexp.of_string_conv_exn s [%of_sexp: Sexp.t String.Map.t])

let drop_test =
  Misc.getenv_bool "DROP_TEST" ~default:Compiler_selection.javascript

let drop_bench =
  Misc.getenv_bool "DROP_BENCH" ~default:Compiler_selection.javascript

let inline_test_color =
  Misc.getenv_bool "INLINE_TEST_COLOR" ~default:true

let alias_for_inline_runners =
  if transitive_runners then Alias.lib_artifacts else Alias.default

let cmi_maybe_cmx =
  if x_library_inlining
  then [".cmi"; ".cmx"]
  else [".cmi"]

let using_no_alias_deps = not select_lib_packing

module Top = struct

  let ocamlcflags =
    Misc.getenv_args "OCAMLCFLAGS" ~default:["-g"]

  let ocamloptflags =
    let default =
      let base = ["-inline"; "20"; "-g"] in
      if nodynlink then "-nodynlink" :: base else base
    in
    let flags = Misc.getenv_args "OCAMLOPTFLAGS" ~default in
    if not nodynlink && List.mem flags "-nodynlink"
    then failwith "OCAMLOPTFLAGS shouldn't contain -nodynlink when NODYNLINK is set to false";
    flags

  let bin_annot_flag =
    if bin_annot
    then ["-bin-annot"]
    else []

  let default_common_flags ~disabled_warnings =
    let ocamlwarnings =
      "@a" ^ String.concat (List.map disabled_warnings ~f:(fun n -> "-" ^ Int.to_string n))
    in
    [
      "-I"; "+camlp4";
      "-w"; ocamlwarnings;
      "-strict-sequence";
      "-short-paths"
    ]

  let default_merlinflags = default_common_flags

  let default_ocamlflags ~disabled_warnings =
    List.concat [
      default_common_flags ~disabled_warnings;
      ["-strict-formats"];
      bin_annot_flag;
      (* It seems that the only effect of -thread is to add +thread to the include
         paths. If we treat the threads library like internal libraries, we should be able
         to remove this and still get errors in javascript when using modules from the
         thread library. *)
      if not Compiler_selection.javascript
      then ["-thread"]
      else []
    ]
end

let common_cflags = [
  "-pipe";
  "-g";
  "-fPIC";
  "-DPIC";
  "-O0";
  "-Wall";
  "-pedantic";
  "-Wextra";
  "-Wunused";
  "-Werror";
  "-Wno-long-long";
  "-DCAML_NAME_SPACE";
  "-O2";
]

let default_cflags = common_cflags @ Compiler_selection.cflags
let default_cxxflags = default_cflags

let ocaml_bin = Compiler_selection.compiler_dir ^/ "bin"
let ocaml_where = Compiler_selection.compiler_dir ^/ "/lib/ocaml"

let ocamldep_path   = ocaml_bin ^/ "ocamldep.opt"
let ocamlc_path     = ocaml_bin ^/ "ocamlc.opt"
let ocamlopt_path   = ocaml_bin ^/ "ocamlopt.opt"
let ocamlcomp_path (module Mode : Ocaml_mode.S) =
  match Mode.which with
  | `Byte -> ocamlc_path
  | `Native -> ocamlopt_path
let camlp4orf_path  = ocaml_bin ^/ "camlp4orf"
let ocamllex_path   = ocaml_bin ^/ "ocamllex"
let ocamlyacc_path  = ocaml_bin ^/ "ocamlyacc"
let ocaml_path      = ocaml_bin ^/ "ocaml"

let camlp4o_path         = root_relative "app/camlp4_with_compiler_libs/camlp4o.exe"
let camlp4o_path_non_opt = root_relative "app/camlp4_with_compiler_libs/camlp4o-byte.exe"

let ocamlobjinfo_path = ocaml_bin ^/ "ocamlobjinfo"

let ocamlpacks =
  ["nums"; "bigarray"; "unix"]
  @ if Compiler_selection.javascript
    then []
    else ["threads"; "str"; "dynlink"]

let expand_dollar_vars_between ~left ~right ~lookup orig =
  match String.lsplit2 orig ~on:'$' with
  | None -> orig (* no dollars, do nothing *)
  | Some (before_first_dollar, after_first_dollar) ->
    let translate after_dollar =
      match (
        match String.chop_prefix after_dollar ~prefix:(String.make 1 left) with
        | None -> None
        | Some after_lp ->
          match String.lsplit2 after_lp ~on:right with
          | None -> None
          | Some (var_name, after_rp) ->
            match (lookup ~var_name) with
            | None -> None
            | Some x -> Some (x, after_rp)
      ) with
      | None -> "$" ^ after_dollar (* cant translate - leave the string as it is*)
      | Some (expansion, after_rp) -> expansion ^ after_rp
    in
    let rec loop acc = function
      | [] -> assert false
      | [last] -> String.concat (List.rev (translate last::acc))
      | after_dollar::xs -> loop (translate after_dollar :: acc) xs
    in
    loop [before_first_dollar] (String.split after_first_dollar ~on:'$')

let expand_dollar_vars ~lookup s =
  let s = expand_dollar_vars_between ~left:'(' ~right:')' ~lookup s in
  let s = expand_dollar_vars_between ~left:'{' ~right:'}' ~lookup s in
  s

let table_to_lookup ~table =
  match String.Table.of_alist table with
  | `Ok h -> fun ~var_name -> Hashtbl.find h var_name
  | `Duplicate_key var ->
    failwithf "duplicate binding for $-var: %S" var ()

(* Expand some $-vars within action strings of rules defined in jbuild files *)
let root_var_table = [
  "-verbose"       , ""; (*"-verbose";*)
  "CPP"            , "cpp";
  "PA_CPP"         , concat_quoted ["cpp"; "-undef"; "-traditional"; "-Werror";];
  "CC"             , concat_quoted
                       (C.Flavor.prog `C :: Compiler_selection.arch_cflags);
  "CXX"            , concat_quoted
                       (C.Flavor.prog `Cxx :: Compiler_selection.arch_cflags);
  "ocaml_bin"      , ocaml_bin;
  "OCAML"          , ocaml_path;
  "OCAMLC"         , ocamlc_path;
  "OCAMLOPT"       , ocamlopt_path;
  "camlp4orf"      , camlp4orf_path;
  "OCAMLCFLAGS"    , String.concat ~sep:" " Top.ocamlcflags;
  "OCAMLOPTFLAGS"  , String.concat ~sep:" " Top.ocamloptflags;
  "ocaml_version"  , Compiler_selection.compiler_dir;
  "ocaml_where"    , ocaml_where;
  "ABSOLUTE_ROOT"  , Path.to_absolute_string Path.the_root;
  "NODYNLINK"      , Bool.to_string nodynlink;
  "USE_NEW_SQML"   , Bool.to_string use_new_sqml;
]

let root_var_lookup =
  let lookup = table_to_lookup ~table:root_var_table in
  fun ?dir ~var_name ->
    match dir, var_name with
    | Some dir, "ROOT" -> Some (reach_from ~dir Path.the_root)
    | Some dir, "bin" -> Some (reach_from ~dir Config.bin_dir)
    | Some dir, "ppx_tools" -> Some (reach_from ~dir Config.ppx_tools)
    | _ -> lookup ~var_name

let expand_vars_root ?dir s =
  expand_dollar_vars ~lookup:(root_var_lookup ?dir) s

(*----------------------------------------------------------------------
 bash
----------------------------------------------------------------------*)

module Bash : sig

  type t
  val create : prog:string -> args:string list -> target:string option -> t
  val action: dir:Path.t -> t list -> Action.t

end = struct

  type t = string

  let create ~prog ~args ~target =
    let com = concat_quoted (prog :: args) in
    match target with
    | None -> com
    | Some target -> sprintf !"%s > %{quote}" com target

  let action ~dir ts =
    let command_string = String.concat ~sep:"; " ts in
    bash ~dir command_string

end

let bash1 ?target prog args = Bash.create ~prog ~args ~target

let write_string_action ?chmod_x string ~target =
  Action.save ?chmod_x (string^"\n") ~target

let write_string_rule ?chmod_x string ~target =
  Rule.create ~targets:[target] (return (write_string_action ?chmod_x string ~target))

let write_names_rule names ~target =
  write_string_rule (String.concat ~sep:" " names) ~target

(*----------------------------------------------------------------------
 remove_target_and_fail
----------------------------------------------------------------------*)

let remove_target_and_fail ~target ~filename_for_error fmt =
  (* Construct an action which when run will echo an error message in a format suitable
     for omake-server. The action then removes the target file & exits 1. *)
  ksprintf (fun mes ->
    bashf ~dir:(dirname target) !"%s rm -f -- %{quote}; exit 1" (
      String.concat (List.map ~f:(fun text -> sprintf "echo %s;" (Shell.escape text)) [
        sprintf "File %S, line 1, characters 0-0:" filename_for_error;
        sprintf "Error: %s" mes;
        sprintf !"when building target: %S" (basename target);
      ]))
      (basename target)
  ) fmt

(*----------------------------------------------------------------------
 jswrap for Centos release 5
----------------------------------------------------------------------*)

module Centos : sig
  val ocamllibflags : string list Dep.t
end = struct

  type t = Release_5 | Release_6 [@@deriving sexp]

  let parse_redhat_release_file_contents string =
    if String.is_prefix string ~prefix:"CentOS release 5" then Release_5 else
    if String.is_prefix string ~prefix:"CentOS release 6" then Release_6 else
    failwith "Unknown CentOS release.  Giving up."

  (* Reads the redhat file only on startup.
     And is NOT sensitive to changes to the file - this would be silly! *)
  let version_where_building() =
    let def =
      Async.Std.(>>|)
        (run_action_now_stdout (
           bash ~dir:Path.the_root "cat /etc/redhat-release"
         )) parse_redhat_release_file_contents
    in
    Dep.deferred (fun () -> def)

  let wrap_funs = [
    "timerfd_settime";
    "timerfd_gettime";
    "timerfd_create";
    "recvmmsg";
  ]

  let c5compatroot = "/usr/jane"

  let c5_compat_root_dirs =
    List.map ~f:(fun x -> c5compatroot ^ "/" ^ x) [
      "lib64";
      "usr/lib64";
      "usr/lib64/mysql";
    ]

  let wrap_rpath_for_ocaml x =
    ["-ccopt"; concat_quoted ["-Xlinker"; "-rpath"; "-Xlinker"; x ]]

  let wrap_wrap_for_ocaml x =
    ["-ccopt"; concat_quoted ["-Xlinker"; "--wrap"; "-Xlinker"; x ]]

  let ocamllibflags =
    (* The wrapping lib doesn't exist in 32 bit, and it doesn't matter because we don't
       use 32 bit executables in production on CentOS 5. *)
    if Compiler_selection.m32
    then return []
    else
      if Config.do_jswrap
      then
        version_where_building() *>>| function
        | Release_5 -> []
        | Release_6 ->
           ccopts (
             List.concat_map c5_compat_root_dirs ~f:wrap_rpath_for_ocaml
             @ List.concat_map wrap_funs ~f:wrap_wrap_for_ocaml
             @ ["-ljswrap"])
      else
        return []
end

(*----------------------------------------------------------------------
 jbuild-ignore
----------------------------------------------------------------------*)

let ignore_filter ~dir =
  let path = relative ~dir "jbuild-ignore" in
  Dep.file_exists path *>>= function
  | false -> return (fun _ -> false) (* no subdir is ignored *)
  | true ->
    file_words_allow_commments path *>>| fun words ->
    List.iter words ~f:(fun word ->
      if String.mem word '/' then
        failwithf "%S: %S can't be a directory basename"
          (Path.to_string path) word ()
    );
    let set = String.Hash_set.of_list words in
    fun path -> Hash_set.mem set (basename path)

let unignored_subdirs ~dir =
  Dep.subdirs ~dir *>>= fun paths ->
  ignore_filter ~dir *>>| fun ignore_p ->
  List.filter paths ~f:(fun path -> not (ignore_p path))

let deep_unignored_subdirs ~dir =
  let rec traverse dir =
    unignored_subdirs ~dir *>>= fun dirs ->
    (Dep.all (List.map dirs ~f:traverse) *>>| List.concat) *>>| fun dirs ->
    dir::dirs
  in
  traverse dir

let does_ignore ~dir path =
  ignore_filter ~dir
  *>>| fun ignore_p ->
  ignore_p path

let rec is_ignored dir =
  if Path.(=) dir Path.the_root
  then Dep.return false
  else
    is_ignored (Path.dirname dir)
    *>>= function
    | true -> Dep.return true
    | false ->
      does_ignore ~dir:(Path.dirname dir) dir

(*----------------------------------------------------------------------
 recursive aliases
----------------------------------------------------------------------*)

let recursive_alias_list = [
  Alias.default;
  Alias.runtest;
  Alias.runbench;
  Alias.qtest;
  Alias.pp;
  Alias.diff;
  Alias.libdeps;
  Alias.merlin;
  Alias.utop;
  Alias.c;

  (* As the names imply, these are used only in [../app/emacs], for building and deploying
     [jane-elisp], for now. *)
  Alias.create "jane-elisp";
  Alias.create "jane-elisp-test";
]

let setup_recursive_aliases ~dir =
  Scheme.rules (
    List.map recursive_alias_list ~f:(fun make_alias ->
      Rule.alias (make_alias ~dir) [
        unignored_subdirs ~dir *>>= fun subs ->
        Dep.all_unit (
          List.map subs ~f:(fun sub -> Dep.alias (make_alias ~dir:sub))
        )]))

let empty_recursive_aliases ~dir =
  Scheme.rules (
    List.map recursive_alias_list ~f:(fun make_alias ->
      Rule.alias (make_alias ~dir) []))

(*----------------------------------------------------------------------
 all_the_repos
----------------------------------------------------------------------*)

let all_the_repos =
  begin
    Dep.subdirs ~dir:Path.the_root *>>| fun xs -> Path.the_root :: xs
  end
  *>>= fun candidate_dirs ->
  Dep.List.concat_map candidate_dirs ~f:(fun dir ->
    Dep.file_exists (relative ~dir ".hg") *>>| function
    | true -> [dir]
    | false -> []
  )

(*----------------------------------------------------------------------
 hg manifest
----------------------------------------------------------------------*)

let hg_prog = Path.to_absolute_string Config.hg_prog

let manifest_dirs_filename = ".manifest.dirs"
let manifest_dirs_path ~repo = relative ~dir:repo manifest_dirs_filename

let manifest_dirs_rule ~repo =
  let target = manifest_dirs_path ~repo in
  Rule.create ~targets:[target] (
    Dep.all_unit [
      (*
         This lists files in the hg equivalent of git index.
         The set of such files does not depend on tree contents, but only on dirstate. *)
      Dep.path Config.hg_prog;
      Dep.path (Path.relative ~dir:repo ".hg/dirstate");
    ] *>>| fun () ->
    (* Here and in the various calls to hg, we ignore stderr because otherwise we can get
       random failures because hg outputs messages about taking the lock. Unfortunately,
       there seems to be no way to silence these messages. *)
    bashf ~ignore_stderr:true ~dir:repo
      !"%{quote} status -acdmn | sed 's|^|./|' | rev | cut -d/ -f2- | rev | sort -u > %{quote}"
      hg_prog (basename target)
  )

let setup_manifest ~dir =
  Scheme.dep (
    all_the_repos *>>| fun repos ->
    if not (List.mem repos dir) then Scheme.no_rules else
      Scheme.rules [
        manifest_dirs_rule ~repo:dir;
        alias_dot_filename_hack ~dir manifest_dirs_filename;
      ])

let manifest_dirs ~repo =
  Dep.contents (manifest_dirs_path ~repo) *>>| fun s ->
  List.map (lines_of_string s) ~f:(relative ~dir:repo)

module Fe = Js_fe.Make(struct
    let all_the_repos = all_the_repos
    let manifest_dirs = manifest_dirs
  end)

let public_release_files_filename = ".public-release.files"
let public_release_files_path ~repo = relative ~dir:repo public_release_files_filename

let public_release_files_rule =
  let repo = Path.the_root in
  Fe.rule_for_projection_files
    (Fe.Projection.create ~repo ~name:"public-release")
    ~target:(public_release_files_path ~repo)

(*----------------------------------------------------------------------
 prefixing for library compilation
----------------------------------------------------------------------*)

(* double underscore has low probability of colliding with a library name *)
let library_prefix_sep = "__"

(*----------------------------------------------------------------------
 libname
----------------------------------------------------------------------*)

(** library names must be identifiers starting with a lowercase letter
    and not conaining "__" anywhere in their name.
    [of_string] conversion takes care of uncapitalizing the first letter so it can
    be used to convert ocaml unit names to library names
*)
module Libname : sig
  include Identifiable.S
  val of_string_opt : string -> t option
  val file_words : Path.t -> t list Dep.t
  val remove_dups_preserve_order : t list -> t list
  val remove_dups_and_sort : t list -> t list
  val suffixed : dir:Path.t -> t -> string -> Path.t
  val to_module : t -> string
end = struct
  include String
  let of_string_opt s =
    if String.is_substring s ~substring:library_prefix_sep
    then None
    else
      (* [Libname.of_string "foo"] and [Libname.of_string "Foo"] refer to the same library,
         represented as the ocaml module [Foo], but written [foo] in .libdeps files *)
      Some (of_string (String.uncapitalize s))
  let of_string s =
    match of_string_opt s with
      Some t -> t
    | None ->
      failwithf "Library names may not contain [%s] as a substring: [%s]"
        library_prefix_sep s ()
  let file_words = file_words
  let remove_dups_preserve_order = remove_dups_preserve_order
  let remove_dups_and_sort = remove_dups_and_sort
  let suffixed = suffixed
  let to_module = String.capitalize
end
module LN = Libname

let prefix_for_lib ~libname = LN.to_string libname ^ library_prefix_sep

(*----------------------------------------------------------------------
 Libmap
----------------------------------------------------------------------*)

module Libmap : sig

  type t
  val create_exn : (LN.t * Path.t) list -> t
  val look_exn : t -> LN.t -> Path.t
  val exists : t -> LN.t -> bool
  val fold : t -> init:'a -> f:(lib:LN.t -> path:Path.t -> 'a -> 'a) -> 'a

end = struct

  type t = Path.t LN.Table.t

  let create_exn xs =
    Hashtbl.mapi (LN.Table.of_alist_multi xs)
      ~f:(fun ~key:lib ~data ->
        match data with
        | [path] -> path
        | _ ->
          failwithf !"Duplicate definition of library '%{LN}' in %s"
            lib (String.concat ~sep:" and " (List.map data ~f:(fun p ->
              sprintf "%S" (Path.to_string p)))) ())

  let look_exn t libname =
    match Hashtbl.find t libname with
    | None -> failheref [%here] !"dont know about library: %{LN}" libname ()
    | Some x -> x

  let exists = Hashtbl.mem

  let fold t ~init ~f =
    Hashtbl.fold t ~init
      ~f:(fun ~key ~data -> f ~lib:key ~path:data)

end

(*----------------------------------------------------------------------
 Use types to capture different varieties of module names
----------------------------------------------------------------------*)

(**
   Name of a single module within a library, with its first letter matching the case of
   the first letter of the .ml/i files containing the module code.

   Both bare names 'foo' and 'Foo' refer to the same module, but the case of the first
   letter is used to determine the file names of .ml, .mli and build artifacts.

   Bare module name that matches the name of the library is treated specially:
   users are not allowed to write such a module in wrapped libraries
   and users can only write such module in unwrapped libraries.
**)


let do_prefixing ~wrapped = not select_lib_packing && wrapped

module Bare_module_name : sig
  include Identifiable.S
  val of_libname : LN.t -> t
  val file_words : Path.t -> t list Dep.t
  val is_lib : t -> libname:LN.t -> bool
  val suffixed : dir:Path.t -> t -> string -> Path.t
  val to_module : t -> string
end = struct
  include String
  let of_libname x = LN.to_string x
  let file_words = file_words
  let is_lib t ~libname = (t = LN.to_string libname)
  let suffixed = suffixed
  let to_module = String.capitalize
end
module BN = Bare_module_name

module Prefixed_module_name : sig
  include Identifiable.S
  val of_barename : wrapped:bool -> libname:LN.t -> BN.t -> t
  val suffixed : dir:Path.t -> t -> string -> Path.t
end = struct
  include String
  let of_barename ~wrapped ~libname name =
    let prefixing = do_prefixing ~wrapped in
    assert (not wrapped ==> not prefixing);
    assert (wrapped ==> not (BN.is_lib ~libname name));

    if prefixing then
      prefix_for_lib ~libname ^ (String.capitalize (BN.to_string name))
    else
      BN.to_string name

  let suffixed = suffixed
end
module PN = Prefixed_module_name

(*----------------------------------------------------------------------
 end - names
----------------------------------------------------------------------*)

module User_or_gen_config : sig

  val load : dir: Path.t -> Jbuild.t list Dep.t
  val source_file : dir:Path.t -> Path.t
  val libnames : dir: Path.t -> LN.t list Dep.t

end = struct

  let source_file ~dir = relative ~dir "jbuild"
  let load ~dir =
    let jbuild = source_file ~dir in
    Dep.file_exists jbuild *>>= function
    | false -> return []
    | true ->
      read_then_convert_string_via_reader
        ~path:jbuild
        ~contents:Dep.contents
        ~do_read:(fun reader ->
          let open Async.Std in
          Pipe.to_list (Reader.read_sexps reader)
          >>| List.map ~f:[%of_sexp: Jbuild.t]
        )

  let the_real_libnames_for_libmap j =
    match j with
    | `preprocessor x -> [Preprocessor_conf.name x]
    | `library x -> [Library_conf.name x]
    | `libraryX x -> [LibraryX_conf.name x]
    | `executables _ -> []
    | `embed _ -> []
    | `jane_script _ -> []
    | `compile_c _ -> []
    | `rule _ -> []
    | `alias _ -> []
    | `no_utop -> []
    | `unified_tests _ -> []
    | `expect_tests _ -> []
    | `switched_to_ppx_style -> []
    | `translate_from_camlp4 -> []
    | `requires_camlp4 -> []
    | `public_repo _ -> []

  let libnames ~dir =
    load ~dir *>>| List.concat_map ~f:the_real_libnames_for_libmap
      *>>| List.map ~f:LN.of_string

end

let prefix_or_pack_args ~wrapped ~libname ~name =
  let prefixing = do_prefixing ~wrapped in
  assert (not wrapped ==> not prefixing);
  assert (wrapped ==> not (BN.is_lib ~libname name));
  if not prefixing
  then
    if wrapped
    then ["-for-pack"; LN.to_module libname]
    else []
  else
    ["-o"; PN.to_string (PN.of_barename ~wrapped ~libname name)]

(*----------------------------------------------------------------------
 generate/compile renaming file (replacement for -pack)
----------------------------------------------------------------------*)

let dash_ml_gen = ".ml-gen"

let gen_renaming_file ~dir ~libname ~modules =
  (* Generate renaming file: [mylib.ml-gen] *)
  assert(not select_lib_packing);
  let wrapped = true in
  let target = LN.suffixed ~dir libname dash_ml_gen in
  let ml_text =
    String.concat ~sep:"\n" (List.map modules ~f:(fun name ->
      let prefixed_name = PN.of_barename ~wrapped ~libname name in
      sprintf "module %s = %s"
        (String.capitalize (BN.to_string name))
        (String.capitalize (PN.to_string prefixed_name))
    ))
  in
  (* [No_such_module] is a name for a module that (hopefully) doesn't exist,
     but we can still make an alias to it as long as the alias stays unused.
     This makes the error messages slightly better in rare cases
     compared to if we used [struct end] *)
  let shadow =
    sprintf !"module No_direct_access_to_%{LN} = struct\
              \n  module %s = No_such_module\
              \n%s\
              \nend\
              \n\n"
      libname (LN.to_module libname)
      (String.concat ~sep:"\n" (List.map modules ~f:(fun name ->
         let prefixed_name = PN.of_barename ~wrapped ~libname name in
         sprintf "  module %s = No_such_module"
           (String.capitalize (PN.to_string prefixed_name))
       )))
  in
  Rule.create ~targets:[target] (
    return (
      write_string_action (shadow ^ ml_text) ~target
    )
  )

(* -intf-suffix is used to make the compiler look for a file other than the .mli to find
   out whether there is a .cmi. We abuse it a bit by passing ".ml" (which will exist) to
   force the compiler to read the cmi, or some random suffix to force the compiler to not
   read the .cmi. *)
let read_or_create_cmi v ml_suf =
  match v with
  | `Read -> [ "-intf-suffix"; ml_suf ]
  | `Create -> [ "-intf-suffix"; ".no-mli" ]

let compile_renaming (module Mode : Ocaml_mode.S) ~libname ~dir =
  (* Special compile rule for renaming file: [mylib.ml-gen]. *)
  let name = BN.of_libname libname in
  let suffixed = BN.suffixed ~dir name in
  let ml_gen = suffixed dash_ml_gen in
  let cmi = suffixed ".cmi" in
  let cmt = suffixed ".cmt" in
  let targets = List.map ~f:suffixed Mode.cmx_and_o in
  let deps = [Dep.path ml_gen] in
  let more_targets, more_deps, bin_annot_flag, cmi_action =
    match Mode.which with
    | `Native -> [cmt; cmi], [], Top.bin_annot_flag, `Create
    | `Byte -> [], [Dep.path cmi], [], `Read
  in
  Rule.create ~targets:(targets @ more_targets) (
    Dep.all_unit (deps @ more_deps) *>>| fun () ->
    assert(using_no_alias_deps);
    Action.process
      ~dir
      (ocamlcomp_path (module Mode))
      (List.concat [
        bin_annot_flag;
        read_or_create_cmi cmi_action dash_ml_gen;
        ["-w"; "@a-49"]; (* warning 49: Absent cmi file when looking up module alias. *)
        ["-no-alias-deps"];
        ["-c"];
        ["-impl"; basename ml_gen];
      ])
  )

let renaming_rules ~dir ~libname ~modules =
  assert(not select_lib_packing);
  [
    gen_renaming_file ~dir ~libname ~modules;
    compile_renaming Ocaml_mode.native ~libname ~dir;
    compile_renaming Ocaml_mode.byte ~libname ~dir;
  ]

(*----------------------------------------------------------------------
 liblinks
----------------------------------------------------------------------*)

let liblinks_dirname = ".liblinks"

let liblinks_dir = root_relative liblinks_dirname

let link_to_remote ~remote ~local =
  let dir = dirname local in
  let deps = [Dep.path remote] in
  Rule.create ~targets:[local] (
    Dep.all_unit deps *>>= fun () ->
    return (
      Bash.action ~dir [
        bash1 "rm" ["-f"; basename local];
        bash1 "ln" ["-s"; reach_from ~dir remote; basename local];
      ]
    )
  )

module Lib_modules : sig
  (** A representation of the list of modules in a library, other than the module
      named after the library itself. *)
  type t =
    { impls : BN.t list
    ; intfs : BN.t list
    ; impls_and_intfs : BN.t list
    ; bin_annot : bool
    }
  [@@deriving fields]
  val empty : t
  val rule : dir:Path.t -> libname:LN.t -> t -> Rule.t
  val load : dir:Path.t -> libname:LN.t -> t Dep.t
end = struct
  type t =
    { impls : BN.t list
    ; intfs : BN.t list
    ; impls_and_intfs : BN.t list
    ; bin_annot : bool
    }
  [@@deriving sexp, fields]

  (* bin_annot is irrelevant when the rest is empty *)
  let empty = { impls = []; intfs = []; impls_and_intfs = []; bin_annot = false }

  let file ~dir ~libname = LN.suffixed ~dir libname ".modules"

  let rule ~dir ~libname t =
    let t =
      { impls = List.sort ~cmp:BN.compare t.impls
      ; intfs = List.sort ~cmp:BN.compare t.intfs
      ; impls_and_intfs = List.sort ~cmp:BN.compare t.impls_and_intfs
      ; bin_annot = t.bin_annot;
      }
    in
    write_string_rule
      (t |> sexp_of_t |> Sexp.to_string_hum)
      ~target:(file ~dir ~libname)

  let load ~dir ~libname =
    Dep.contents (file ~dir ~libname)
    *>>| fun str -> Sexp.of_string_conv_exn (String.strip str) t_of_sexp
end

let pack_order_file ~dir ~libname = LN.suffixed ~dir libname ".pack-order"
let stub_names_file ~dir ~libname = LN.suffixed ~dir libname ".stub.names"

let stub_names_rule ~dir ~libname ~stub_names =
  write_names_rule stub_names ~target:(stub_names_file ~dir ~libname)

let stubs_archive_file name = "lib" ^ name ^ "_stubs.a"
let stubs_dependencies ~dir ~lib =
  file_words (LN.suffixed ~dir lib ".stub.names") *>>= fun stub_names ->
  Dep.all_unit (List.map stub_names ~f:(fun name ->
    Dep.path (Path.relative ~dir (stubs_archive_file name))
  ))

module LL : sig

  val liblink_dir : lib:LN.t -> Path.t
  val liblink_refname : lib:LN.t -> name:string -> Path.t
  val liblink_deps : libs:LN.t list -> suffixes:string list -> unit Dep.t list
  val liblink_default : libs:LN.t list -> unit Dep.t list

  val liblinks_stubs : libs:LN.t list -> unit Dep.t
  val liblink_includes : dir:Path.t -> libs:LN.t list -> string list
  val liblink_interfaces : lib:LN.t -> LN.t list Dep.t

  val api_rule : dir:Path.t -> Libmap.t -> Rule.t
  val rules : dir:Path.t -> Libmap.t -> Rule.t list Dep.t

  (** does not establish the .cmi file dependencies, just lists their paths (including
      the main library .cmi) *)
  val liblink_submodule_cmi_paths : lib:LN.t -> Libmap.t -> Path.t list Dep.t

end = struct

  let liblink_dir ~lib = relative ~dir:liblinks_dir (LN.to_string lib)
  let liblink_refname ~lib ~name = relative ~dir:(liblink_dir ~lib) name
  let liblink_ref ~lib ~suf = liblink_refname ~lib ~name:(LN.to_string lib ^ suf)

  let submodule_cmis ~lib =
    (* We setup a glob dependency on *.cmx (the list, not the files themselves) to
       ensure rebuilds when X_LIBRARY_INLINING transitions from true -> false *)
    if select_lib_packing
    then Dep.glob_change (Glob.create ~dir:(liblink_dir ~lib) "*.cmx")
    else
      (* This alias includes the dependency on the list *.cmx. *)
      Dep.alias (Alias.submodule_cmis ~dir:(liblink_dir ~lib))
  ;;

  let submodule_cmxs ~lib =
    if select_lib_packing
    then Dep.return ()
    else Dep.alias (Alias.submodule_cmxs ~dir:(liblink_dir ~lib))

  let liblink_deps ~libs ~suffixes =
    List.concat_map libs ~f:(fun lib ->
      List.map suffixes ~f:(fun suf ->
        Dep.all_unit [
          Dep.path (liblink_ref ~lib ~suf);
          begin match suf with
          | ".cmi" -> submodule_cmis ~lib
          | ".cmx" -> submodule_cmxs ~lib
          | _ -> Dep.return ()
          end;
        ]))

  let liblink_default ~libs =
    List.map libs ~f:(fun lib ->
      Dep.alias (Alias.default ~dir:(liblink_dir ~lib))
    )

  let liblink_interfaces ~lib =
    LN.file_words (liblink_ref ~lib ~suf:".interface.deps")

  let liblinks_stubs ~libs =
    Dep.all_unit (
      List.map libs ~f:(fun lib ->
        stubs_dependencies ~dir:(liblink_dir ~lib) ~lib
      )
    )

  let liblink_includes ~dir ~libs =
    List.concat_map libs ~f:(fun lib ->
      ["-I"; reach_from ~dir (liblink_dir ~lib)]
    )

  let make_liblink_rule ~remote_dir ~lib name =
    let remote = relative ~dir:remote_dir name in
    let local = liblink_refname ~lib ~name in
    link_to_remote ~remote ~local

  let libname_from_liblink_path ~dir = LN.of_string (basename dir)

  (* Weirdly, we create symlinks for the .cmt/.cmti of the main module even though the
     target may not be buildable. When we get rid of packing, we should consider
     - removing the cmt from here
     - using the same -bin-annot to compile the main module than for the other modules
     - including the main module in the Lib_modules.t
     so that we only have rules creating symlinks to existing files. *)
  let the_liblink_suffixes =
    cmi_maybe_cmx
    @ [".cmxs"; ".cmo";".cma";".cmxa";".a";".libdeps";".interface.deps";
       ".stub.names"; ".cmti"; ".cmt";
      ]

  let submodule_names_even_if_packed ~lib libmap =
    let remote_dir = Libmap.look_exn libmap lib in
    Lib_modules.load ~dir:remote_dir ~libname:lib

  let submodule_names ~lib libmap =
    if select_lib_packing then return Lib_modules.empty else
      submodule_names_even_if_packed ~lib libmap

  let liblink_submodule_cmi_paths ~lib libmap =
    submodule_names ~lib libmap *>>| fun subs ->
    liblink_refname ~lib ~name:(LN.to_string lib ^ ".cmi") ::
      List.filter_map subs.impls_and_intfs ~f:(fun sub ->
        if BN.is_lib ~libname:lib sub
        then None
        else
          let wrapped = true in
          let pn = PN.to_string (PN.of_barename ~wrapped ~libname:lib sub) ^ ".cmi" in
          Some (liblink_refname ~lib ~name:pn)
      )

  let main_cmti_and_cmt ~remote_dir ~lib =
    let cmti_name = LN.to_string lib ^ ".cmti" in
    let cmti_path = relative ~dir:remote_dir cmti_name in
    let cmt_name = LN.to_string lib ^ ".cmt" in
    let cmt_path = relative ~dir:remote_dir cmt_name in
    Dep.both
      (Dep.file_exists cmti_path)
      (Dep.file_exists cmt_path) *>>| fun (cmti, cmt) ->
    List.concat [
      if cmti then [cmti_name] else [];
      if cmt then [cmt_name] else []
    ]

  let api_rule ~dir libmap =
    let deps =
      Libmap.fold libmap ~init:[] ~f:(fun ~lib ~path:_ acc ->
        Dep.alias (Alias.api ~dir:(liblink_dir ~lib)) :: acc
      )
    in
    Rule.alias (Alias.api ~dir) deps

  let rules ~dir libmap =
    let lib = libname_from_liblink_path ~dir in
    let remote_dir = Libmap.look_exn libmap lib in
    let remote_stub_names_file = stub_names_file ~dir:remote_dir ~libname:lib in
    file_words remote_stub_names_file *>>= fun stub_names ->
    submodule_names_even_if_packed ~lib libmap *>>| fun modules ->
    let files_for bns ~suffix =
      List.filter_map bns ~f:(fun mod_ ->
        if BN.is_lib ~libname:lib mod_
        then None
        else
          let wrapped = true in
          Some (PN.to_string (PN.of_barename ~wrapped ~libname:lib mod_) ^ suffix)
      )
    in
    let cmis =
      if not select_lib_packing then
        files_for modules.impls_and_intfs ~suffix:".cmi"
      else []
    in
    let cmxs =
      if not select_lib_packing && x_library_inlining then
        files_for modules.impls ~suffix:".cmx"
      else []
    in
    let cmtis =
      if modules.bin_annot then files_for modules.intfs ~suffix:".cmti"
      else []
    in
    let cmts =
      if modules.bin_annot then
        let ml_no_mli =
          List.filter
            ~f:(fun m -> not (List.mem modules.intfs m))
            modules.impls
        in
        files_for ml_no_mli ~suffix:".cmt"
      else []
    in
    let submodule_files_to_link = List.concat [ cmis; cmxs; cmts; cmtis ] in
    let submodule_link_rules =
      List.map submodule_files_to_link ~f:(make_liblink_rule ~remote_dir ~lib)
    in
    let submodule_alias_rules =
      if not select_lib_packing then
        let make_alias ?(dep = Dep.return ()) alias_in_dir names =
          let dependencies =
            dep :: List.map names ~f:(fun name -> Dep.path (relative ~dir name))
          in
          Rule.alias (alias_in_dir ~dir)
            [Dep.group_dependencies (Dep.all_unit dependencies)]
        in
        [
          make_alias Alias.submodule_cmis cmis
            ~dep:(Dep.glob_change (Glob.create ~dir "*.cmx")) ;
          make_alias Alias.submodule_cmxs cmxs;
        ]
      else []
    in
    let default_rule =
        (* Setup a .DEFAULT alias in the lib-links directory, to indirect to the
           .lib_artifacts alias in the directory where the library code is actually found.
           This .DEFAULT alias is for the use of remote references to the library (i.e. when
           linking .exe) to force the entire default build, including inline_tests *)
      Rule.default ~dir:(liblink_dir ~lib) [
        Dep.alias (Alias.lib_artifacts ~dir:remote_dir)
      ]
    in
    let api_rule =
      Rule.alias (Alias.api ~dir:(liblink_dir ~lib)) [
        main_cmti_and_cmt ~remote_dir ~lib *>>= fun main_cmts ->
        let api = List.concat [ main_cmts; cmtis; cmts; cmis ] in
        Dep.all_unit
          (List.map api ~f:(fun name ->
             Dep.path (relative ~dir:(liblink_dir ~lib) name)));
      ]
    in
    let linked_names =
      List.map the_liblink_suffixes ~f:(fun suf -> LN.to_string lib ^ suf)
      @ List.map stub_names ~f:(fun name -> stubs_archive_file name)
    in
    let link_rules = List.map linked_names ~f:(make_liblink_rule ~remote_dir ~lib) in
    List.concat [
      link_rules;
      submodule_link_rules;
      submodule_alias_rules;
      [api_rule; default_rule];
    ]
end

(*----------------------------------------------------------------------
 code_style
----------------------------------------------------------------------*)

type code_style =

| Requires_camlp4
  (* ML source code is in camlp4-style, i.e. "with sexp".
     But is not suitable for on-the-fly translation to ppx-style. *)

| Translate_from_camlp4
  (* ML source code is in camlp4-style, i.e. "with sexp"
     And is translated on the fly to ppx-style.
     And the ppx-style version is used for the actual compilation. *)

| Switched_to_ppx_style
  (* ML source code is in ppx-style, i.e. [@@deriving sexp]
     So there is no way to run with camlp4-style preprocessing.
     And so no diff rules are setup. *)

(*----------------------------------------------------------------------
 directory context (dc)
----------------------------------------------------------------------*)

module DC = struct
  type t = {
    code_style : code_style;
    dir : Path.t;
    ocamllibflags : string list;
    merlinflags : string list;
    ocamlflags : string list;
    ocamlcflags : string list;
    ocamloptflags : string list;
    js_of_ocaml_flags : string list;
    xlibnames : LN.t list; (* broader set: from preprocessor/library/libraryX configs
                                and includes "bin" if there is an executables config *)

    ocaml_plugin_libraries : (string -> string list option);
    no_utop_alias : bool;
    libmap : Libmap.t;
    impls : BN.t list;
    intfs : BN.t list;
    impl_is_buildable : (BN.t -> bool);
    intf_is_buildable : (BN.t -> bool);
  } [@@deriving fields]
end

let libdeps_for libmap names =
  Dep.List.concat_map names ~f:(fun name ->
    let path = LN.suffixed ~dir:(Libmap.look_exn libmap name) name ".libdeps" in
    LN.file_words path *>>| fun libs ->
    libs @ [ name ]
  )

(*----------------------------------------------------------------------
 local_dependencies
----------------------------------------------------------------------*)

let dep_append ys xsd =
  xsd *>>| fun xs -> ys @ xs

(* Ml compilation requires we setup .cmi/.cmx dependencies for modules listed in the
   corresponding .d file, AND a dependency on the .d file to ensure recompilation in
   case a referenced .ml file has been removed (and so no longer listed in the .d).

   If the .d dependency is missing, we may (incorrectly) fail to rerun the
   compilation, because jenga regards a strict decrease in dependencies as not
   sufficient cause to trigger an action.

*)
let local_dependencies : (
  [`ml | `mli] -> Ocaml_mode.t -> DC.t -> wrapped:bool -> libname:LN.t
  -> BN.t -> unit Dep.t
) =
  fun ml_kind (module Mode : Ocaml_mode.S) dc ~wrapped ~libname x ->
    begin
      let {DC.dir;impl_is_buildable;_} = dc in
      let artifact name suf =
        PN.suffixed ~dir (PN.of_barename ~wrapped ~libname name) suf
      in
      let artifacts_ml name =
        let sufs = [".cmi"] @ (
          if Mode.compilation_depends_on_cmx
          then
            if impl_is_buildable name then [Mode.cmx] else []
          else [])
        in
        List.map ~f:(artifact name) sufs
      in
      let artifacts_mli name =
        let sufs = [".cmi"] in
        List.map ~f:(artifact name) sufs
      in
      let dsuf,artifacts =
        match ml_kind with
        | `mli -> ".mli.d", artifacts_mli
        | `ml ->  ".ml.d", artifacts_ml
      in
      let xd = BN.suffixed ~dir x dsuf in
      if using_no_alias_deps
      then
        dep_append [xd] (
          BN.file_words xd *>>= fun ys ->
          Dep.List.concat_map ys ~f:(fun y ->
            let yd = BN.suffixed ~dir y ".cmi.deps" in
            dep_append (yd :: artifacts y) (
              BN.file_words yd *>>| fun zs ->
              List.concat_map zs ~f:artifacts)))
        *>>| path_remove_dups_and_sort
      else
        dep_append [xd] (
          BN.file_words xd *>>| fun ys ->
          List.concat_map ys ~f:artifacts)
    end
    *>>= fun paths ->
    Dep.all_unit (List.map paths ~f:Dep.path)

(*----------------------------------------------------------------------
 objdeps/libdeps
----------------------------------------------------------------------*)

let gen_transitive_deps : (
  one_step : string list Dep.t ->
  dep_wrt_dir : Path.t ->
  template : (string -> string) ->
  target : Path.t ->
  Rule.t
) =
  fun ~one_step ~dep_wrt_dir ~template ~target ->
    let path_of_name = (fun name -> relative ~dir:dep_wrt_dir (template name)) in
    Rule.create ~targets:[target] (
      one_step *>>= fun names1 ->
      let paths = List.map names1 ~f:path_of_name in
      Dep.List.concat_map paths ~f:file_words *>>| fun namesM ->
      let names = remove_dups_preserve_order (namesM @ names1) in
      write_string_action (String.concat ~sep:" " names) ~target
    )

let libs_transitive_closure libs =
  let libdeps_files =
    List.map libs ~f:(fun lib ->
      LL.liblink_refname ~lib ~name:(LN.to_string lib ^ ".libdeps"))
  in
  Dep.List.concat_map libdeps_files ~f:LN.file_words
  *>>| fun additional_libs ->
  LN.remove_dups_preserve_order (additional_libs @ libs)
;;

let gen_objdeps ~dir name ~exists_ml =
  let suf = ".objdeps" in (* transitive closure of .ml.d *)
  let target = BN.suffixed ~dir name suf in
  if exists_ml
  then
    let one_step =
      BN.file_words (BN.suffixed ~dir name ".ml.d") *>>| fun bns ->
      List.map bns ~f:BN.to_string
    in
    gen_transitive_deps
      ~one_step
      ~dep_wrt_dir:dir
      ~template:(fun x -> x ^ suf)
      ~target
  else
    Rule.create ~targets:[target] (return (
      remove_target_and_fail
        ~target
        ~filename_for_error:(BN.to_string name ^ ".mli")
        !"%{BN}.ml is missing" name))
;;

let cmideps ~dir name = BN.file_words (BN.suffixed ~dir name ".cmi.deps")
let gen_cmideps dc name =
  (* [foo.cmi.deps], lists for module [Foo]
     the cmis which might be read when compiling an ml/mli which refers to Foo *)
  let {DC.dir;intf_is_buildable;_} = dc in
  let suf = ".cmi.deps" in
  let one_step =
    let dsuf = (if intf_is_buildable name then ".mli.d" else ".ml.d") in
    BN.file_words (BN.suffixed ~dir name dsuf) *>>| fun names ->
    List.map names ~f:BN.to_string
  in
  gen_transitive_deps
    ~one_step
    ~dep_wrt_dir:dir
    ~template:(fun x -> x ^ suf)
    ~target:(BN.suffixed ~dir name suf)

let get_inferred_1step_deps ~dir ~libname =
  LN.file_words (LN.suffixed ~dir libname ".inferred-1step.deps")

module Objinfo : sig

  type t
  val interface_names : t -> string list
  val parse : string -> t

end = struct

  type crc = Crc of string | Weak
  type name = string
  type import = crc * name
  type t = {
    interfaces : import list;
  } [@@deriving fields]

  let interface_names t = List.map t.interfaces ~f:snd
  let weak_crc = String.make 32 '-'

  let read_import_line line =
    match String.split line ~on:'\t' with
      ["";crc;name] ->
        assert (String.length crc = 32);
        let crc = if String.(=) crc weak_crc then Weak else Crc crc in
        (crc,name)
    | _ -> failwith "read_import_line"

  let rec skip_to_interface_banner = function
    | [] -> failwith "skip_to_interface_banner"
    | line::lines ->
      if String.(line = "Interfaces imported:")
      then lines
      else skip_to_interface_banner lines

  let parse stdout =
    let lines = lines_of_string stdout in
    let lines = skip_to_interface_banner lines in
    let interfaces = List.map lines ~f:read_import_line in
    { interfaces }

end

let gen_interface_deps_from_objinfo dc ~dir ~wrapped ~libname =
  let self = libname in
  let in_libmap x = Libmap.exists dc.DC.libmap x in
  let target = LN.suffixed ~dir libname ".interface.deps" in
  Rule.create ~targets:[target] (
    Lib_modules.load ~dir ~libname
    *>>= fun { impls_and_intfs; impls = _; intfs = _; bin_annot = _; } ->
    let names = LN.to_string libname :: (List.map impls_and_intfs ~f:(fun bn ->
      PN.to_string (PN.of_barename ~wrapped ~libname bn)
    )) in
    Dep.List.concat_map names ~f:(fun name ->
      let cmi = suffixed ~dir name ".cmi" in
      Dep.action_stdout (
        Dep.path cmi *>>| fun () ->
        bashf ~dir !"%{quote} %{quote} | fgrep -v -- %{quote}"
          ocamlobjinfo_path (basename cmi) library_prefix_sep
      ) *>>| fun stdout ->
      let obi = Objinfo.parse stdout in
      let words = Objinfo.interface_names obi in
      let words = List.map ~f:String.uncapitalize words in
      words
    ) *>>| fun words ->

    let is_local_module =
      (* The [not (is_local_module..)] check prevents local modules from being
         misinterpreted as libnames, when there is a library with the same name.
         This can only happen when PACKING=true - there is no prefixing - and so a local
         module [foo.ml] can clash with a libray [foo]. *)
      if select_lib_packing
      then
        let modules =
          let names = List.map ~f:BN.to_string (dc.DC.impls @ dc.DC.intfs) in
          let names = List.map ~f:String.uncapitalize names in
          String.Hash_set.of_list names
        in
        Hash_set.mem modules
      else
        (* In the case PACKING=false, this [is_local_module] check is not necessary.
           Because, either:
           (a) wrapped=true, so submodules have prefixed names, which can never be
           interpreted as a library name, because of the invariant that libnames do not
           contain the [library_prefix_sep]
           (b) wrapped=false, containing a single submodule [self], which is filtered anyway
        *)
        fun _ -> false
    in
    let candidate_libs =
      (* Filters any word which is not permitted as a libname.
         In particular, any word containing the [library_prefix_sep] *)
      List.filter_map words ~f:LN.of_string_opt
    in
    let libs =
      List.filter candidate_libs ~f:(fun lib ->
        in_libmap lib
        && not (is_local_module (LN.to_string lib))
        && lib <> self)
    in
    let libs = LN.remove_dups_and_sort libs in
    let words = List.map libs ~f:LN.to_string in
    write_string_action (String.concat ~sep:" " words) ~target
  )

let gen_libdeps ~dir ~libs libname =
  [
    gen_transitive_deps
      ~one_step:(return (List.map libs ~f:LN.to_string))
      ~dep_wrt_dir:liblinks_dir
      ~template:(fun x -> sprintf "%s/%s.interface.deps" x x)
      ~target:(LN.suffixed ~dir libname ".inferred-1step.deps");

    gen_transitive_deps
      ~one_step:(return (List.map libs ~f:LN.to_string))
      ~dep_wrt_dir:liblinks_dir
      ~template:(fun x -> sprintf "%s/%s.libdeps" x x)
      ~target:(LN.suffixed ~dir libname ".libdeps");

    Rule.alias (Alias.libdeps ~dir) [
      Dep.path (LN.suffixed ~dir libname ".libdeps")
    ];
  ]

(*----------------------------------------------------------------------
 user flag control - replace/extend
----------------------------------------------------------------------*)

let combine_replace_extra ~tag ~replace ~extra xs =
  match replace,extra with
  | [],[] -> xs
  | [],_::_ -> xs @ extra
  | _::_,[] -> replace
  | _::_,_::_ ->
    failwithf "%s: both extra_%s & replace_%s are defined" tag tag tag ()

(*----------------------------------------------------------------------
 Rule_conf, Alias_conf (user rules)
----------------------------------------------------------------------*)

let standard_c_include_search_path_from ~dir = [
  ".";
  ocaml_where;
  reach_from ~dir Config.include_dir
]

let expand_vars_in_rule dc ~dir ~targets ~deps ~cflags orig =
  let prefixed_includes ~dir =
    List.concat_map (standard_c_include_search_path_from ~dir) ~f:(fun path -> ["-I"; path])
  in
  let {DC. ocamlflags; _} = dc in
  let dep_exn name = function
    | Some dep -> dep
    | None -> failwithf "Cannot use ${%s} with files_recursively_in" name ()
  in
  let lookup ~var_name =
    match var_name with
    | "@" -> Some (String.concat ~sep:" " targets)
    | "<" -> Some (match deps with [] -> "" | dep1::_ -> dep_exn var_name dep1)
    | "^" ->
      let deps = List.map deps ~f:(dep_exn var_name) in
      Some (String.concat ~sep:" " deps)
    | "CFLAGS"  -> Some (String.concat ~sep:" " cflags)
    | "OCAMLFLAGS"  -> Some (String.concat ~sep:" " ocamlflags)
    | "PREFIXED_INCLUDES" -> Some (String.concat ~sep:" " (prefixed_includes ~dir))
    | _ -> root_var_lookup ~dir ~var_name
  in
  expand_dollar_vars ~lookup orig

let expand_vars_in_dep ~dir orig = expand_vars_root ~dir orig

module Dep_conf_interpret = struct

  include Dep_conf

  let to_depends ~dir = function
    | File s -> Dep.path (Path.relative_or_absolute ~dir (expand_vars_in_dep ~dir s))
    | Alias s -> Dep.alias (Alias.of_string ~dir (expand_vars_in_dep ~dir s))
    | Glob_files s ->
      Dep.glob_listing (Glob.of_string ~dir (expand_vars_in_dep ~dir s)) *>>= fun paths ->
      (* Add deps on the contents of all files matching the glob-string [s], including
         source files AND buildable files. *)
      Dep.all_unit (
        (* Hack around stale artifact deletion not working well enough (apparently jenga
           relies on self-triggering). *)
        List.filter_map paths ~f:(fun path ->
          match Path.basename path with
          | "inline_tests_runner.ml"
          | "inline_benchmarks_runner.ml" -> None
          | _ -> Some (Dep.path path)))
    | Files_recursively_in spec_dir ->

      (* Add deps on the recursive list of files, *and* their contents. Only works
         if the directory only contains source files, otherwise we wouldn't depend
         on what is buildable but not built. *)
      let spec_dir = relative ~dir (expand_vars_in_dep ~dir spec_dir) in
      deep_unignored_subdirs ~dir:spec_dir *>>= fun dirs ->
      Dep.all_unit (
        List.map dirs ~f:(fun dir ->
          Dep.glob_listing (Glob.create ~dir ~kinds:[`File] "*") *>>= fun paths ->
          Dep.all_unit (List.map paths ~f:Dep.source_if_it_exists)
        )
      )

  let list_to_depends ~dir ts =
    List.map ts ~f:(to_depends ~dir)

  let only_plain_file ~dir = function
    | File s -> Some (expand_vars_in_dep ~dir s)
    | Alias _ -> None
    | Glob_files _ -> None
    | Files_recursively_in _ -> None
end

let expanded_action dc ~dir ~targets ~cflags deps action =
  let command_string =
    let deps = List.map deps ~f:(Dep_conf_interpret.only_plain_file ~dir) in
    expand_vars_in_rule dc ~dir ~targets ~deps ~cflags action
  in
  bash ~dir command_string

let rule_conf_to_rule dc ~dir ~cflags conf =
  let {Rule_conf. targets; deps; action} = conf in
  let action = expanded_action dc ~dir ~targets ~cflags deps action in
  simple_rule
    ~targets:(List.map targets ~f:(relative ~dir))
    ~deps:(Dep_conf_interpret.list_to_depends ~dir deps)
    ~action

let alias_conf_to_rule dc ~dir ~cflags conf =
  let {Alias_conf. name; deps; action} = conf in
  let action = Option.map action ~f:(expanded_action dc ~dir ~targets:[] ~cflags deps) in
  let deps = Dep_conf_interpret.list_to_depends ~dir deps in
  let deps =
    match action with
    | None -> deps
    | Some action -> [Dep.action (Dep.all_unit deps *>>| fun () -> action)]
  in
  Rule.alias (Alias.create ~dir name) deps


module Lib_clients = struct
  module Cache = struct
    let file = ".lib-clients-cache"
    let path = root_relative file
    type t = Path.Set.t LN.Map.t [@@deriving sexp]

    let rule =
      Rule.create ~targets:[path]
        begin
          deep_unignored_subdirs ~dir:Path.the_root *>>= fun dirs ->
           Dep.all (List.map dirs ~f:(fun dir ->
            (* If we wanted to check the build of the immediate clients only (instead of
               the transitive clients) we would use .inferred-1step.deps instead of
               .libdeps *)
            Dep.glob_listing (Glob.create ~dir "*.libdeps")
          )) *>>= fun paths ->
          let paths = List.concat paths in
          Dep.all (
            List.map paths ~f:(fun path ->
              let dir = Path.dirname path in
              LN.file_words path *>>| List.map ~f:(fun lib -> (lib, dir))
          )) *>>| fun client_dir_by_lib ->
          (* Map and Set give consistent ordering across reexecution of the Dep.t. *)
          let client_dir_by_lib = List.concat client_dir_by_lib in
          let t = LN.Map.of_alist_fold ~init:Path.Set.empty ~f:Set.add client_dir_by_lib in
          Action.save ~target:path (Sexp.to_string_hum (sexp_of_t t))
        end
  end

  let rules ~dir ~libname:sought =
    List.map
      [ ("clients", Alias.default)
      ; ("clients-runtest", Alias.runtest)
      ]
      ~f:(fun (alias, what_to_build) ->
        Rule.alias (Alias.create ~dir alias) [
          Dep.contents Cache.path *>>= fun contents ->
          let map = Sexp.of_string_conv_exn contents [%of_sexp: Cache.t] in
          let dirs = Option.value ~default:Path.Set.empty (LN.Map.find map sought) in
          Dep.all_unit (List.map (Set.to_list dirs) ~f:(fun dir ->
            Dep.alias (what_to_build ~dir)))
        ])

end

(*----------------------------------------------------------------------
 libraryX_rules
----------------------------------------------------------------------*)

let libraryX_rules dc ~dir ~cflags conf =
  let { LibraryX_conf. name; targets; deps; action; } = conf in
  let libname = LN.of_string name in
  let rule =
    rule_conf_to_rule dc ~dir ~cflags
      { Rule_conf. targets; deps; action; }
  in
  let targets = List.map targets ~f:(relative ~dir) in
  let default_rule =
    Rule.default ~dir (List.map targets ~f:Dep.path)
  in
  let stub_names = [LN.to_string libname] in
  let stubs_rule = stub_names_rule ~dir ~libname ~stub_names in
  let wrapped = false in
  [
    gen_interface_deps_from_objinfo dc ~dir ~wrapped ~libname;
    Lib_modules.rule ~dir ~libname Lib_modules.empty;
    rule;
    default_rule;
    stubs_rule;
  ] @ Lib_clients.rules ~dir ~libname

(*----------------------------------------------------------------------
 c/cxx compilation
----------------------------------------------------------------------*)

let compile_c_or_cxx ~dir ~include_search_path ~non_include_flags ~suf ~flavor name =
  let source = name ^ suf in
  let include_search_path =
    include_search_path @ standard_c_include_search_path_from ~dir
  in
  (* shared [flags] for gcc -MM and compile *)
  let flags =
    let include_flags =
      List.concat_map include_search_path ~f:(fun path -> ["-I"; path])
    in
    non_include_flags @ include_flags
  in
  let deps = suffixed ~dir name ".deps" in
  let o = suffixed ~dir name ".o" in
  [
    Rule.create ~targets:[deps] (
      C.deps ~dir ~source ~flavor ~flags
      *>>| fun (`Includes includes, `Search_path search_path) ->
      let search_path_in_comments =
        String.concat (List.map search_path ~f:(sprintf "# %s\n"))
      in
      let includes = String.concat ~sep:"\n" includes in
      write_string_action (search_path_in_comments ^ includes) ~target:deps
    );
    Rule.create ~targets:[o] (
      file_words_allow_commments deps *>>= fun includes ->
      C.known_deps ~dir ~flags ~includes
      *>>| fun () ->
      Action.process ~dir
        (C.Flavor.prog flavor)
        (Compiler_selection.arch_cflags @ flags @ ["-c"; source; "-o"; basename o])
    );
    Rule.alias (Alias.c ~dir) [Dep.path o];
  ]

let compile_c ~dir ~include_search_path ~cflags name =
  compile_c_or_cxx ~dir ~include_search_path
    ~flavor:`C
    ~non_include_flags:cflags
    ~suf:".c"
    name

let compile_cxx ~dir ~include_search_path ~cxxflags ~cxx_suf name =
  compile_c_or_cxx ~dir
    ~include_search_path
    ~flavor:`Cxx
    ~non_include_flags:cxxflags
    ~suf:cxx_suf
    name

let static_archive_c ~dir ~o_names ~target =
  let o_files = List.map o_names ~f:(fun file -> file ^ ".o") in
  let deps = List.map ~f:(fun x -> Dep.path (relative ~dir x)) o_files in
  simple_rule ~deps ~targets:[relative ~dir target]
    ~action:(Bash.action ~dir [
      bash1 "rm" ["-f"; target];
      bash1 "ar" (["Drc"; target] @ o_files);
      bash1 "ranlib" [target];
    ])

let user_configured_compile_c_rules ~dir conf =
  let { Compile_c_conf. names; extra_cflags; replace_cflags; includes; } = conf
  in
  let cflags =
    combine_replace_extra
      ~tag:"cflags"
      ~replace:replace_cflags
      ~extra:extra_cflags
      default_cflags
  in
  List.concat_map names ~f:(fun name ->
    compile_c ~dir ~cflags ~include_search_path:includes name
  )

(*----------------------------------------------------------------------
 ocamllex/ocamlyacc
----------------------------------------------------------------------*)

let ocamllex_rule ~dir name =
  let suf x = suffixed ~dir name x in
  let ml = suf ".ml" in
  let mll = suf ".mll" in
  simple_rule
    ~deps:[Dep.path mll]
    ~targets:[ml]
    ~action:(
      Action.process ~dir ocamllex_path ["-q"; basename mll]
    )

let ocamlyacc_rule ~dir name =
  let suf x = suffixed ~dir name x in
  let ml = suf ".ml" in
  let mli = suf ".mli" in
  let mly = suf ".mly" in
  simple_rule
    ~deps:[Dep.path mly]
    ~targets:[ml;mli]
    ~action:(
      Action.process ~dir ocamlyacc_path ["-q"; basename mly]
    )

(*----------------------------------------------------------------------
 ml/mli
----------------------------------------------------------------------*)

type ml_kind = ML | MLI

let ml_kind_to_suf = function
  | ML -> ".ml"
  | MLI -> ".mli"

let ml_kind_to_flag = function
  | ML -> "-impl"
  | MLI -> "-intf"

(*----------------------------------------------------------------------
 Preprocessor_name (PP)
----------------------------------------------------------------------*)

module PP : sig
  include Identifiable.S
  val remove_dups_preserve_order : t list -> t list
  val to_libname : t -> LN.t
end = struct
  include String
  let remove_dups_preserve_order = remove_dups_preserve_order
  let to_libname = LN.of_string
end

(*----------------------------------------------------------------------
 preprocessing ocaml file (pps)
----------------------------------------------------------------------*)

let pa_jane = List.map ~f:PP.of_string [
  "pa_type_conv";
  "pa_sexp_conv";
  "pa_bin_prot";
  "pa_fields_conv";
  "pa_variants_conv";
  "pa_typerep_conv";
  "pa_compare";
  "pa_pipebang";
  "pa_here";
  "pa_custom_printf";
  "pa_test";
  "pa_enumerate";
  "pa_fail";
  "pa_structural_sexp";
  "ppx_let";
  "ppx_sexp_message";
  "pa_ounit";
  "pa_bench";
  "ppx_expect";
]

let pa_sexp_conv = List.map ~f:PP.of_string [
  "pa_type_conv";
  "pa_sexp_conv";
]

let bitstring_preprocessor_libs =
  List.map ~f:PP.of_string
  [
    "bitstring_types";
    "bitstring_config";
    "bitstring";
    "bitstring_persistent";
    "pa_bitstring";
  ]

let expand_pps (names:string list) =
  let flags, pp_names = List.partition_tf names ~f:(String.is_prefix ~prefix:"-") in
  let pps =
    PP.remove_dups_preserve_order (
      List.map ~f:PP.of_string ["pa_untabbed"] @
      List.concat_map pp_names ~f:(function
      | "JANE"          -> pa_jane
      | "SEXP_CONV"     -> pa_sexp_conv
      | "pa_sexp_conv"  -> pa_sexp_conv
      | "BITSTRING"     -> bitstring_preprocessor_libs
      | name            -> [PP.of_string name]))
  in
  pps, flags

let eval_names_spec ~dc names_spec =
  let {DC.intfs; intf_is_buildable; impls; impl_is_buildable; _} = dc in
  let filter_out_inline_modules l =
    List.filter l ~f:(function
      | "inline_tests_runner"
      | "inline_benchmarks_runner" -> false
      | _ -> true)
  in
  let iis = List.map ~f:BN.to_string (impls @ intfs) in
  let standard = remove_dups_preserve_order (filter_out_inline_modules iis) in
  let xs = Ordered_set_lang.eval_with_standard (Some names_spec) ~standard in
  let xs = List.map ~f:BN.of_string xs in
  List.filter xs ~f:intf_is_buildable,
  List.filter xs ~f:impl_is_buildable,
  (* Do not filter all names given by user, to allow detection of any names for which
     there is neither a .ml or .mli *)
  xs

(*----------------------------------------------------------------------
 PPXset
----------------------------------------------------------------------*)

let remap_pa_string =
  (* convert camlp4 preprocessor names (pa_) to replacement ppx version *)
  function
  | "pa_untabbed" (* for compatibility with ppx *)
  | "pa_macro"    (* provided by optcomp, which is linked in ppx_driver *)
    -> None
  | "pa_test" -> Some "ppx_assert"
  | "pa_structural_sexp" -> Some "ppx_sexp_value"
  | "pa_ounit" -> Some "ppx_inline_test"
  (* default: replace "pa_" with "ppx_" *)
  |  s ->
    match String.chop_prefix s ~prefix:"pa_" with
    | Some s -> Some ("ppx_" ^ s)
    | None -> Some s

let remap_pa_flag = function
  | "-pa-ounit-drop" -> "-inline-test-drop"
  | s -> s

let remap_pa_names pps =
  pps
  |> List.map ~f:PP.to_string
  |> List.filter_map ~f:remap_pa_string


module Standard_pp_sets : sig

  val extract : string list -> string list
  val expand : string list -> string list

end = struct

  let standard_sets = String.Map.of_alist_exn [
    "JANE", (remap_pa_names pa_jane);
    "SEXP", ["ppx_type_conv";"ppx_sexp_conv"];
  ]

  let (--) xs ys = (* preserves order *)
    List.filter xs ~f:(fun x -> not (List.mem ys x))

  let extract names =
    let names,extracted =
      Map.fold standard_sets ~init:(names,[]) ~f:(
        fun ~key:tag ~data:potential_extraction (names,extracted) ->
          match (potential_extraction -- names) with
          | _::_ -> names,extracted
          | [] ->
            let remaining = names -- potential_extraction in
            remaining, tag :: extracted)
    in
    List.rev extracted @ names

  let expand names =
    List.concat_map names ~f:(fun name ->
      Option.value (Map.find standard_sets name) ~default:[name])

end

let ppx_cache_dir = root_relative ".ppx"

module PPXset : sig

  type t
  val create : PP.t list -> t
  val to_libs : t -> LN.t list
  val parse_subdir_name : dir:Path.t -> t option
  val exe_path : t -> Path.t

end = struct

  type t = { sorted : string list }

  let to_libs t =
    List.map t.sorted ~f:LN.of_string

  let create1 names = {
    sorted = remove_dups_and_sort names;
  }

  let create pps = create1 (remap_pa_names pps)

  let subdir_name t =
    match Standard_pp_sets.extract t.sorted with
    | [] -> "NONE"
    | _ :: _ as names -> String.concat names ~sep:"+"

  let parse_subdir_name ~dir =
    let basename = basename dir in
    let names =
      match basename with
      | "NONE" -> []
      | _ as base -> Standard_pp_sets.expand (String.split base ~on:'+')
    in
    let result = create1 names in
    if String.(=) (subdir_name result) basename
    then Some result
    else None

  let exe_path t =
    relative ~dir:(relative ~dir:ppx_cache_dir (subdir_name t)) "ppx.exe"

end

let ppx_executable pps = PPXset.exe_path (PPXset.create pps)

let metaquot_exe =
  relative ~dir:Config.ppx_tools "syntax/ppx_metaquot.exe"

(*----------------------------------------------------------------------
 pp kind
----------------------------------------------------------------------*)

let retabbed_suf =
  (* camlp4-style ml file with extra spaces and tabbing *)
  "-retabbed"

let ppxed_suf =
  (* ml file translated to ppx-style, with extra spaces and tabbing *)
  "-ppxed"

let pristine_ppxed_suf =
  (* ml file translated to ppx-style, with NO extra spaces or tabbing *)
  "-pristine"

module PP_style = struct

  type t =
  | Nothing
  | Command of string
  | Metaquot (* ML source code uses ppx-style meta-quotations: i.e. [%expr ],. [%e ]
                (probably to implement a pre-processor) *)
  | PP of PP.t list * string list * code_style

  let of_kind dc (kind:Preprocess_kind.t) =
    let {DC. code_style; _} = dc in
    match kind with
    | `no_preprocessing        -> Nothing
    | `command s               -> Command (expand_vars_root s)
    | `metaquot                -> Metaquot
    | `pps names ->
      let pps, flags = expand_pps names in
      PP (pps, flags, code_style)

end

(*----------------------------------------------------------------------
 ML compilation context (mc)
----------------------------------------------------------------------*)

module MC = struct
  type mc = {
    dc : DC.t;
    dir : Path.t;
    libname : LN.t;

    can_setup_inline_runners : bool;
    x_libs : LN.t list;
    pp_style : PP_style.t;
    preprocessor_deps : string sexp_list;

    exists_ml : bool;
    exists_mli : bool;
    wrapped : bool;
    must_be_sharable : bool;
  }
end

(*----------------------------------------------------------------------
 lookup_pp
----------------------------------------------------------------------*)

let lookup_pp dc ~default_pp ~preprocess_spec =
  let map =
    BN.Map.of_alist_exn (List.concat_map preprocess_spec ~f:(fun (kind, names_spec) ->
      let _, _, modules = eval_names_spec ~dc names_spec in
      let pp_style = PP_style.of_kind dc kind in
      List.map modules ~f:(fun name -> (name, pp_style))
    ))
  in
  let default : PP_style.t =
    match default_pp with
    | None -> Nothing
    | Some s -> Command s
  in
  stage (fun name -> Option.value (Map.find map name) ~default)

(*----------------------------------------------------------------------
 local ppx.exe
----------------------------------------------------------------------*)

let link_quietly = bin_relative "link-quietly"

let ppx_driver_runner = LN.of_string "ppx_driver_runner"

let link_deps_of_unforced_libs (module Mode : Ocaml_mode.S) ~libs =
  LL.liblink_deps ~libs ~suffixes:Mode.cmxa_and_a
  @ [LL.liblinks_stubs ~libs]

let generate_ppx_exe_rules libmap ~dir =
  match PPXset.parse_subdir_name ~dir with
  | None -> []
  | Some ppxset ->
    let target = PPXset.exe_path ppxset in
    let libs = PPXset.to_libs ppxset in
    let libs = libs @ [ppx_driver_runner] in
    [Rule.create ~targets:[target] (
       libdeps_for libmap libs *>>= fun libs ->
       let libs = LN.remove_dups_preserve_order libs in
       (* Make sure we do link ppx_driver_runner last, otherwise we have a problem. *)
       assert ((List.last libs) = (Some ppx_driver_runner));
       let deps =
         Dep.path link_quietly ::
         link_deps_of_unforced_libs ~libs Ocaml_mode.native
       in
       Dep.all_unit deps *>>| fun () ->
       let cmxa_for_packs = List.map ocamlpacks ~f:(fun name -> name ^ ".cmxa") in
       let link_exe_command =
         concat_quoted (List.concat [
           [reach_from ~dir link_quietly;
            ocamlopt_path;
            "-g";"-linkall";
            "-I"; "+compiler-libs"; "ocamlcommon.cmxa";
            "-thread";
           ];
           cmxa_for_packs;
           List.concat_map libs ~f:(fun lib -> [
               "-I"; reach_from ~dir (LL.liblink_dir ~lib);
               LN.to_string lib ^ ".cmxa";
             ]);
           ["-o"; basename target]
         ])
       in
       bash ~dir link_exe_command)]

(*----------------------------------------------------------------------
 pp libs
----------------------------------------------------------------------*)

let libs_for_code_generated_by_pp pp =
  begin match remap_pa_string (PP.to_string pp) with
    | Some "ppx_sexp_conv"     -> [ "sexplib" ]
    | Some "ppx_bin_prot"      -> [ "bin_prot" ]
    | Some "ppx_fields_conv"   -> [ "fieldslib" ]
    | Some "ppx_variants_conv" -> [ "variantslib" ]
    | Some "ppx_typerep_conv"  -> [ "typerep_lib" ]
    | Some "ppx_assert"        -> [ "ppx_assert_lib" ]
    | Some "ppx_inline_test"   -> [ "ppx_inline_test_lib" ]
    | Some "ppx_bench"         -> [ "ppx_bench_lib" ]
    | Some "ppx_expect"        -> [ "expect_test_collector" ]
    | _                        -> []
  end |> List.map ~f:LN.of_string

let eval_preprocess_style_libs style : LN.t list =
  match style with
  | PP_style.Nothing | Command _ | Metaquot -> []
  | PP (pps, _flags, _style) ->
    List.concat_map pps ~f:libs_for_code_generated_by_pp

let get_pp_libs ~mc : LN.t list =
  let {MC. x_libs; pp_style; _} = mc in
  x_libs @ eval_preprocess_style_libs pp_style

(*----------------------------------------------------------------------
 pp deps
----------------------------------------------------------------------*)

let camlp4_pps pps =
  List.filter pps ~f:(fun pp ->
    not (String.is_prefix ~prefix:"ppx" (PP.to_string pp)))
;;

(* this expresses the dependencies of the commands produced by [get_camlp4_com] *)
let camlp4_deps_without_user_deps (pps:PP.t list) : unit Dep.t list =
  let pps = camlp4_pps pps in
  Dep.path camlp4o_path_non_opt
  :: List.concat_map pps ~f:(fun pp ->
    let name = PP.to_string pp in
    match
      match name with
      | "pa_macro" -> None
      | _ -> Some ".cmo"
    with
    | None -> []
    | Some suf ->
      [Dep.path (LL.liblink_refname ~lib:(LN.of_string name) ~name:(name ^ suf))])

let ppx_deps_without_user_deps (pps:PP.t list) : unit Dep.t list =
  [Dep.path (ppx_executable pps)]

let get_pp_user_deps ~mc : unit Dep.t list =
  let {MC. dir; preprocessor_deps; _} = mc in
  List.map preprocessor_deps ~f:(fun s ->
    Dep.path (Path.relative_or_absolute ~dir s))

let get_pp_deps ~mc : unit Dep.t list =
  let {MC. pp_style; _} = mc in
  get_pp_user_deps ~mc @
    begin match pp_style with
    | PP_style.Nothing | Command _ -> []
    | Metaquot -> [Dep.path metaquot_exe]
    | PP (pps, _flags, Requires_camlp4)       -> camlp4_deps_without_user_deps pps
    | PP (pps, _flags, Switched_to_ppx_style)
    | PP (pps, _flags, Translate_from_camlp4) -> ppx_deps_without_user_deps pps
    end

(*----------------------------------------------------------------------
  pp com
----------------------------------------------------------------------*)

module Camlp4_com : Stringable = String

let get_camlp4_com ~dir ~libname ~can_setup_inline_runners ~flags kind (pps:PP.t list) =
  let pps = camlp4_pps pps in
  Camlp4_com.of_string
    (concat_quoted (
       reach_from ~dir camlp4o_path_non_opt ::
        List.concat_map pps ~f:(fun pp ->
          let name = PP.to_string pp in
          let path ~suf =
            reach_from ~dir (LL.liblink_refname ~lib:(LN.of_string name) ~name:(name ^ suf))
          in
          match name with
          | "pa_macro" -> "pa_macro.cmo" :: Compiler_selection.pa_macro_flags
          | "pa_here" -> [ path ~suf:".cmo"; "-pa-here-dirname"; Path.to_string dir ]
          | "pa_ounit" ->
            if can_setup_inline_runners
            then
              [ path ~suf:".cmo"; "-pa-ounit-lib"; LN.to_string libname ]
              @ (if drop_test
                 then [ "-pa-ounit-drop-with-deadcode" ]
                 else [])
            else []
          | "pa_bench" ->
            if can_setup_inline_runners
            then
              [ path ~suf:".cmo" ]
              @ (if drop_bench
                 then [ "-pa-bench-drop-with-deadcode" ]
                 else [])
            else []
          | _ -> [path ~suf:".cmo"]
        ) @ flags @ [ml_kind_to_flag kind]))

let get_ppx_command
    ~kind
    ~dir ~libname ~can_setup_inline_runners ~flags (pps:PP.t list) : (string * string list) =
  let exe = ppx_executable pps in
  let prog = reach_from ~dir exe in
  let kind_flag =
    match kind with
    | None -> [] (* will be inferred from file extension *)
    | Some kind -> [ml_kind_to_flag kind]
  in
  let uses_inline_test = ref false in
  let uses_inline_bench = ref false in
  let args =
    List.concat_map pps ~f:(fun pp ->
      match remap_pa_string (PP.to_string pp) with
      | Some "ppx_here" -> ["-dirname"; Path.to_string dir]
      | Some ("ppx_inline_test" | "ppx_expect") -> uses_inline_test := true; []
      | Some "ppx_bench" -> uses_inline_bench := true; []
      | _ -> [])
    @ List.map flags ~f:remap_pa_flag
    @ kind_flag
  in
  let args =
    List.concat
      [ (if !uses_inline_test && drop_test
         then [ "-inline-test-drop-with-deadcode" ]
         else [])
      ; (if !uses_inline_bench && drop_bench
         then [ "-bench-drop-with-deadcode" ]
         else [])
      ; (if can_setup_inline_runners
         && (!uses_inline_test || !uses_inline_bench)
         then [ "-inline-test-lib"; LN.to_string libname ]
         else [])
      ; args
      ]
  in
  prog, args


(* Preprocessing, with either [-pp] or [-ppx].

   A [-ppx] command should expect two additional arguments: name of the input file with
   marshalled AST and name of the output file.

   A [-pp] command should each expect one additional argument: name of the
   source file, and should write the preprocessed output to stdout.

   This command depends on [get_pp_deps ~mc ~name].
*)
let get_pp_com_args ~(kind:ml_kind) ~mc ~name : string list =
  let {MC. dir; libname; can_setup_inline_runners; pp_style; _} = mc in
  match pp_style with
  | PP_style.Nothing -> []
  | Metaquot -> ["-ppx"; reach_from ~dir metaquot_exe ]
  | Command string -> ["-pp"; string]
  | PP (pps, flags, code_style) ->
    match code_style with
    | Requires_camlp4 ->
      ["-pp"; Camlp4_com.to_string (get_camlp4_com ~dir ~libname ~flags ~can_setup_inline_runners kind pps)]
    | Translate_from_camlp4
    | Switched_to_ppx_style ->
      let prog, args = get_ppx_command ~kind:(Some kind) ~dir ~libname ~can_setup_inline_runners ~flags pps in
      let args =
        ["-dump-ast";
         "-loc-filename"; (BN.to_string name ^ ml_kind_to_suf kind)
        ] @ args
      in
      ["-pp"; concat_quoted (prog :: args)]

(*----------------------------------------------------------------------
 pp via_suffix
----------------------------------------------------------------------*)

(* [via_suffix] is the suffix of the files we actually run compilation on.
   For example, when doing translation from [camlp4] followed by compilation
   via [ppx], the translator writes down [foo.ml-pristine] and that's what
   [ocamlopt] receives as the input.
*)
let get_pp_via_suffix ~mc : string =
  let {MC. pp_style; _} = mc in
  match pp_style with
  | Nothing
  | Command _
  | Metaquot
  | PP (_, _, Requires_camlp4)
  | PP (_, _, Switched_to_ppx_style)
    -> ""
  | PP (_, _, Translate_from_camlp4) -> pristine_ppxed_suf

(*----------------------------------------------------------------------
 generate_diff
----------------------------------------------------------------------*)

let generate_diff kind name ~jump file1 file2 target =
  let filename =
    match jump with
    | `diff -> basename target
    | `file -> (BN.to_string name) ^ ml_kind_to_suf kind
  in
  let line1 = sprintf "File %S, line 1, characters 1-1:" filename in
  let line2 = sprintf "Generated code diffs between ppx and camlp4 for: %s"
    (BN.to_string name) in
  let echo_message = sprintf !"echo %{quote}; echo %{quote}" line1 line2 in
  let dir = dirname target in
  let diff_command =
    sprintf !"diff -u %{quote} %{quote} > %{quote}"
      (reach_from ~dir file1)
      (reach_from ~dir file2)
      (reach_from ~dir target)
  in
  let action =
    bashf ~dir !"trap %{quote} ERR; %s" echo_message diff_command
  in
  simple_rule
    ~deps:[Dep.path file1; Dep.path file2]
    ~targets:[target]
    ~action

let generate_parsetree_nopos source target =
  let dir = dirname target in
  simple_rule
    ~deps:[Dep.path source]
    ~targets:[target]
    ~action:(
      bashf ~dir !"cat %{quote} | sed 's/ *(.*\\[.*\\]\\.\\..*\\[.*\\]).*//' > %{quote}"
        (reach_from ~dir source)
        (reach_from ~dir target))

(* Note on extensions:
    (.pp) - pretty printed Ocaml
    (.parsetree) - ocaml parsetree AST in human readable form
    (.pt) - ocaml parsetree AST in human readable form (with position info stripped)
*)
let generate_ppx_diff_files kind mc ~dir ~name =
  let x suf = suffixed ~dir (BN.to_string name ^ ml_kind_to_suf kind) suf in
  let {MC. pp_style; _} = mc in
  match pp_style with
  | PP (_, _, Translate_from_camlp4) -> [
    generate_diff kind name ~jump:`diff
      (x ".pp")
      (x (ppxed_suf^".pp"))
      (x ".pp.diff");
    generate_diff kind name ~jump:`file
      (x ".pt")
      (x (ppxed_suf^".pt"))
      (x ".pt.diff");
    generate_parsetree_nopos
      (x ".parsetree")
      (x ".pt");
    generate_parsetree_nopos
      (x (ppxed_suf^".parsetree"))
      (x (ppxed_suf^".pt"));
    Rule.alias (Alias.diff ~dir) [
      Dep.path (x ".pp.diff");
      Dep.path (x ".pt.diff");
    ]
  ]
  | _ -> []


(*----------------------------------------------------------------------
 .ml-ppxed
----------------------------------------------------------------------*)

let generate_ppxed_and_diff mc ~name =
  let translator = ppx_relative "translate/camlp4_to_ppx.exe" in
  let {MC. dir; exists_ml; exists_mli; _} = mc in
  let make_rule kind =
    let suf = ml_kind_to_suf kind in
    let file = relative ~dir (sprintf !"%{BN}%s" name suf) in
    let target1 = relative ~dir (sprintf !"%{BN}%s%s" name suf pristine_ppxed_suf) in
    let target2 = relative ~dir (sprintf !"%{BN}%s%s" name suf ppxed_suf) in
    let target3 = relative ~dir (sprintf !"%{BN}%s%s" name suf retabbed_suf) in
    [simple_rule
       ~deps:[Dep.path translator; Dep.path file]
       ~targets:[target1;target2;target3]
       ~action:(
         bashf ~dir !"%{quote} %{quote} %{quote} %{quote} %{quote}"
           (reach_from ~dir translator)
           (reach_from ~dir file)
           (reach_from ~dir target1)
           (reach_from ~dir target2)
           (reach_from ~dir target3));
    ] @
    generate_ppx_diff_files kind mc ~dir ~name
  in
  (if exists_ml then make_rule ML else [])
  @ (if exists_mli then make_rule MLI else [])

(*----------------------------------------------------------------------
 generate .pp
----------------------------------------------------------------------*)

let generate_pp_using_ppx mc ~kind ~name ~raw ~via_suffix ~pps ~flags =
  let {MC. dir; libname; can_setup_inline_runners; _} = mc in
  let prog, args = get_ppx_command ~kind:(Some kind) ~dir ~libname ~can_setup_inline_runners ~flags pps in
  let pp_suf = if raw then ".parsetree" else ".pp" in
  let pp_deps = get_pp_user_deps ~mc @ ppx_deps_without_user_deps pps in
  let suf = ml_kind_to_suf kind in
  let source = BN.suffixed ~dir name (suf ^ via_suffix) in
  let deps = [Dep.path source] @ pp_deps in
  let target = BN.suffixed ~dir name (suf ^ via_suffix ^ pp_suf) in
  let action =
    let args =
      args @ (if raw then ["-dparsetree"] else []) @ [
        "-loc-filename"; basename (BN.suffixed ~dir name suf);
        (ml_kind_to_flag kind); reach_from ~dir source;
        "-o"; reach_from ~dir target
      ]
    in
    Action.process ~dir prog args
  in [
    simple_rule ~deps ~targets:[target] ~action;
    Rule.alias (Alias.pp ~dir) [Dep.path target];
  ]

let generate_pp_using_camlp4 mc kind ~raw ~name ~via_suffix ~pps ~flags =
  let {MC. dir; libname; can_setup_inline_runners; _} = mc in
  let com = Camlp4_com.to_string (get_camlp4_com ~dir ~libname ~can_setup_inline_runners ~flags kind pps) in
  let pp_deps = get_pp_user_deps ~mc @ camlp4_deps_without_user_deps pps in
  let pp_suf = if raw then ".parsetree" else ".pp" in
  let expander = ppx_relative "validate/expand_camlp4/bin/expand_camlp4.exe" in
  let suf = ml_kind_to_suf kind in
  let source = BN.suffixed ~dir name (suf ^ via_suffix) in
  let target = BN.suffixed ~dir name (suf ^ pp_suf) in
  [simple_rule
     ~deps:([Dep.path expander; Dep.path source] @ pp_deps)
     ~targets:[target]
     ~action:(bashf ~dir !"%{quote}%s %{quote} %{quote} > %{quote}"
                (reach_from ~dir expander)
                (if raw then " -dparsetree" else "")
                com
                (basename source)
                (basename target));
   Rule.alias (Alias.pp ~dir) [Dep.path target];
  ]

let generate_pp mc ~kind ~name =
  let gen raw =
    let {MC. pp_style; _} = mc in
    match pp_style with
    | PP_style.Nothing -> []
    | Command _com ->
      (*generate_pp_using_camlp4 mc kind ~raw ~name ~via_suffix:"" ~com*)
      []

    | Metaquot ->
      []

    | PP (pps, flags, Switched_to_ppx_style) ->
      generate_pp_using_ppx mc ~kind ~name ~raw ~via_suffix:"" ~pps ~flags

    | PP (pps, flags, Requires_camlp4) ->
      generate_pp_using_camlp4 mc kind ~raw ~name ~via_suffix:"" ~pps ~flags

    | PP (pps, flags, Translate_from_camlp4) ->
      generate_pp_using_camlp4 mc kind ~raw ~name ~via_suffix:retabbed_suf ~pps ~flags
      @ generate_pp_using_ppx mc ~kind ~name ~raw ~via_suffix:ppxed_suf ~pps ~flags
  in
  gen true @ gen false

(*----------------------------------------------------------------------
 ocamldep / .d files
----------------------------------------------------------------------*)

let gen_dfile kind ~disallowed_module_dep mc ~name =
  let parse_ocamldep_output_exn ~filename s = match String.lsplit2 ~on:':' s with
    | Some (before_colon, res) when String.(=) before_colon filename ->
      words_of_string res
    | _ ->
      failwithf
        "ocamldep failed to produce dependency information for %S. The output was: %S."
        (BN.to_string name) s ()
  in
  let {MC. dc; dir; _ } = mc in
  let pp_deps = get_pp_deps ~mc in
  let pp_args = get_pp_com_args ~kind ~mc ~name in
  let via_suffix = get_pp_via_suffix ~mc in
  let suf = ml_kind_to_suf kind in
  let dsuf = suf ^ ".d" in
  let source = BN.suffixed ~dir name (suf ^ via_suffix) in
  let dfile = BN.suffixed ~dir name dsuf in
  let targets = [dfile] in
  let action =
    let prog = ocamldep_path in
    let args =
      ["-modules"] @ pp_args @ [ml_kind_to_flag kind] @ [basename source]
    in
    Action.process ~dir prog args
  in
  Rule.create ~targets (
     Dep.action_stdout
       (Dep.all_unit (Dep.path source :: pp_deps) *>>| fun () -> action)
     *>>| fun output ->
     let is_actual_dep =
       let self = name in
       let actual_modules =
         let names = dc.DC.impls @ dc.DC.intfs in
         BN.Set.of_list names
       in
       fun name -> name <> self && Set.mem actual_modules name
     in
     let potential_dependencies =
       output
       |> parse_ocamldep_output_exn ~filename:(basename source)
       |> List.sort ~cmp:String.compare
       (* can depend on both a.ml and A.ml, depending on which one exists *)
       |> List.concat_map ~f:(fun x ->
         (* Consider both [x] and an uncapitalised version.
            Following call to [is_actual_dep] will choose the correct one. *)
         [x; String.uncapitalize x;]
       )
       |> List.map  ~f:BN.of_string
     in
     let actual_dependencies =
       List.filter potential_dependencies ~f:is_actual_dep
     in
     match
       List.filter_map actual_dependencies ~f:(fun name ->
         Option.map (disallowed_module_dep name) ~f:(fun msg -> name, msg))
     with
     | (bad_bn, _) :: _ as errors ->
       let bad_module = BN.to_module bad_bn in
       let error_messages = List.map errors ~f:snd in
       remove_target_and_fail ~target:dfile ~filename_for_error:(BN.to_string name ^ suf)
         "this module apparently has forbidden dependencies: %s. If these are not \
          actual dependencies of your module, you can make ocamldep understand it by \
          qualifying module paths: for instance instead of %s.something, using \
          Library.Std.%s.something."
         (String.concat ~sep:", " error_messages) bad_module bad_module
     | [] ->

       Action.save (String.concat ~sep:" " (List.map actual_dependencies ~f:BN.to_string))
         ~target:dfile
    )

(*----------------------------------------------------------------------
 ocaml compilation
----------------------------------------------------------------------*)

(** returns the additional compiler arguments to compile in the presence of
    the renaming module if any,
    and includes the corresponding dependencies into the dep list *)
let open_renaming deps mc =
  let {MC. dir; libname; wrapped; _ } = mc in
  let prefixing = do_prefixing ~wrapped in
  if not prefixing || not wrapped
  then deps, []
  else begin
    assert(using_no_alias_deps);
    let renaming_deps =
      List.map [".cmi";".cmx"] ~f:(fun suf ->
        Dep.path (suffixed ~dir (LN.to_string libname) suf))
    in
    let deps = renaming_deps @ deps in
    let args = [ "-open"; LN.to_module libname; "-open"; "No_direct_access_to_" ^ LN.to_string libname ] in
    deps, args
  end

let conditional = function
  | true -> fun x -> [x]
  | false -> fun _ -> []

let compile_mli mc ~name =
  let {MC. dc; dir; libname; wrapped; _ } = mc in
  let kind = MLI in
  let pp_libs = get_pp_libs ~mc in
  let pp_deps = get_pp_deps ~mc in
  let pp_args = get_pp_com_args ~kind ~mc ~name in
  let via_suffix = get_pp_via_suffix ~mc in
  let {DC. ocamlflags; ocamlcflags; _} = dc in
  let prefix_args = prefix_or_pack_args ~wrapped ~libname ~name in
  let prefixed_name = PN.of_barename ~wrapped ~libname name in
  let flags = ocamlflags @ ocamlcflags in
  let mli = BN.suffixed ~dir name (".mli" ^ via_suffix) in
  let cmi = PN.suffixed ~dir prefixed_name ".cmi" in
  let targets =
    List.concat
      [ [cmi]
      ; conditional (List.mem flags "-bin-annot")
          (PN.suffixed ~dir prefixed_name ".cmti")
      ]
  in
  Rule.create ~targets (
    get_inferred_1step_deps ~dir ~libname *>>= fun libs ->
    let libs = pp_libs @ libs in
    let libdeps = LL.liblink_deps ~libs ~suffixes:[".cmi"] in
    let deps =
      [Dep.path mli; local_dependencies `mli Ocaml_mode.byte dc ~wrapped ~libname name]
      @ pp_deps @ libdeps
    in
    let deps,open_renaming_args = open_renaming deps mc in
    Dep.all_unit deps *>>| fun () ->
    Action.process
      ~dir
      ocamlc_path
      (List.concat [
          (* In .mli's, we disable warning 33: "Unused open statement".  This is a new
             warning reported by 4.01, and we would like to be able to work around it by
             using [open!] to squelch the warning.  But [open!] isn't properly supported
             by 4.01 yet.  So, we're leaving the warning off until a stock OCaml is
             released that supports [open!]. *)
        flags; ["-w"; "-33"];
        pp_args;
        LL.liblink_includes ~dir ~libs;
        prefix_args;
        (if using_no_alias_deps then ["-no-alias-deps"] else []);
        open_renaming_args;
        ["-o"; basename cmi];
        [ "-c"; "-intf"; basename mli]
      ])
  )

let remove_nodynlink =
  List.filter ~f:(fun x -> x <> "-nodynlink")

let native_compile_ml mc ~name =
  let {MC. dc; dir; libname; wrapped; exists_mli; must_be_sharable; _ } = mc in
  let kind = ML in
  let pp_libs = get_pp_libs ~mc in
  let pp_deps = get_pp_deps ~mc in
  let pp_args = get_pp_com_args ~kind ~mc ~name in
  let via_suffix = get_pp_via_suffix ~mc in
  let {DC. ocamlflags; ocamloptflags; _} = dc in
  let prefix_args = prefix_or_pack_args ~wrapped ~libname ~name in
  let prefixed_name = PN.of_barename ~wrapped ~libname name in
  let ml = BN.suffixed ~dir name (".ml" ^ via_suffix) in
  let o = PN.suffixed ~dir prefixed_name ".o" in
  let cmx = PN.suffixed ~dir prefixed_name ".cmx" in
  let cmi = PN.suffixed ~dir prefixed_name ".cmi" in
  let flags = ocamlflags @ ocamloptflags in
  let flags = if must_be_sharable then remove_nodynlink flags else flags in
  let targets =
    List.concat
      [ [o; cmx]
      ; conditional (not exists_mli) cmi
      ; conditional (List.mem flags "-bin-annot")
          (PN.suffixed ~dir prefixed_name ".cmt")
      ; conditional (List.mem flags "-S")
          (PN.suffixed ~dir prefixed_name ".s")
      ; conditional (List.mem flags "-dtypes" || List.mem flags "-annot")
          (PN.suffixed ~dir prefixed_name ".annot")
      ]
  in
  Rule.create ~targets (
    get_inferred_1step_deps ~dir ~libname *>>= fun libs ->
    let libs = LN.remove_dups_preserve_order (pp_libs @ libs) in
    let libdeps = LL.liblink_deps ~libs ~suffixes:cmi_maybe_cmx in
    let deps =
      [ Dep.path ml; local_dependencies `ml Ocaml_mode.native dc ~wrapped ~libname name ]
      @ pp_deps
      @ libdeps
    in
    let deps = if exists_mli then deps @ [Dep.path cmi] else deps in
    let deps,open_renaming_args = open_renaming deps mc in
    Dep.all_unit deps *>>| fun () ->
    Action.process
      ~dir
      ocamlopt_path
      (List.concat [
        flags;
        pp_args;
        LL.liblink_includes ~dir ~libs;
        prefix_args;
        read_or_create_cmi (if exists_mli then `Read else `Create) (".ml" ^ via_suffix);
        (if using_no_alias_deps then ["-no-alias-deps"] else []);
        open_renaming_args;
        ["-o"; basename cmx];
        ["-c"; "-impl"; basename ml];
      ])
  )

let byte_compile_ml mc ~name =
  let {MC. dc; dir; libname; wrapped; _ } = mc in
  let kind = ML in
  let pp_libs = get_pp_libs ~mc in
  let pp_deps = get_pp_deps ~mc in
  let pp_args = get_pp_com_args ~kind ~mc ~name in
  let via_suffix = get_pp_via_suffix ~mc in
  let {DC. ocamlflags; ocamlcflags; _} = dc in
  let ocamlflags = List.filter ocamlflags ~f:(function "-bin-annot" -> false | _ -> true) in
  let ml = BN.suffixed ~dir name (".ml" ^ via_suffix) in
  let prefixed_name = PN.of_barename ~wrapped ~libname name in
  let cmi = PN.suffixed ~dir prefixed_name ".cmi" in
  let cmo = PN.suffixed ~dir prefixed_name ".cmo" in
  (* We run the byte compiler with an extended "-o" prefix so the generated file names are
     different from the native compile. In particular we care about generated .cmi files,
     which are written by both native and byte compilers when there is no .mli file.
     After byte compilation, the .cmo is [mv]ed back to the original prefixed name. *)
  let prefix_args = prefix_or_pack_args ~wrapped ~libname ~name in
  let targets = [ cmo ] in
  Rule.create ~targets (
    get_inferred_1step_deps ~dir ~libname *>>= fun libs ->
    let libs = LN.remove_dups_preserve_order (pp_libs @ libs) in
    let libdeps = LL.liblink_deps ~libs ~suffixes:[".cmi"] in
    let deps =
      [ Dep.path ml;
        local_dependencies `ml Ocaml_mode.byte dc ~wrapped ~libname name ]
      @ pp_deps @ libdeps
    in
    let deps = deps @ [Dep.path cmi] in
    let deps,open_renaming_args = open_renaming deps mc in
    Dep.all_unit deps *>>| fun () ->
    Action.process
      ~dir
      ocamlc_path
      (List.concat [
        ocamlflags; ocamlcflags;
        pp_args;
        LL.liblink_includes ~dir ~libs;
        prefix_args;
        read_or_create_cmi `Read (".ml" ^ via_suffix);
        (if using_no_alias_deps then ["-no-alias-deps"] else []);
        open_renaming_args;
        ["-c"; "-impl"; basename ml];
      ])
  )

let infer_mli_auto mc ~name =
  let {MC. dc; dir; libname; wrapped; _ } = mc in
  let kind = ML in
  let pp_deps = get_pp_deps ~mc in
  let pp_args = get_pp_com_args ~kind ~mc ~name in
  let via_suffix = get_pp_via_suffix ~mc in
  let {DC. ocamlflags; _} = dc in
  let prefix_args = prefix_or_pack_args ~wrapped ~libname ~name in
  let ml = BN.suffixed ~dir name (".ml" ^ via_suffix) in
  let mli_auto = BN.suffixed ~dir name ".mli.auto" in
  Rule.create ~targets:[mli_auto] (
    get_inferred_1step_deps ~dir ~libname *>>= fun libs ->
    let libdeps = LL.liblink_deps ~libs ~suffixes:[".cmi"] in
    let deps =
      [Dep.path ml; local_dependencies `ml Ocaml_mode.byte dc ~wrapped ~libname name]
      @ pp_deps @ libdeps in
    let deps,open_renaming_args = open_renaming deps mc in
    Dep.all_unit deps *>>| fun () ->
    Bash.action ~dir [
      bash1 ocamlc_path (List.concat [
        ["-i"];
        ocamlflags;
        pp_args;
        LL.liblink_includes ~dir ~libs;
        prefix_args;
        (if using_no_alias_deps then ["-no-alias-deps"] else []);
        open_renaming_args;
        [ "-c"; "-impl"; basename ml];
      ])
        ~target:(basename mli_auto);
    ]
  )

(*----------------------------------------------------------------------
 rules for a directory of ml
----------------------------------------------------------------------*)

let mem_of_list l =
  let set = BN.Hash_set.of_list l in
  fun x -> Hash_set.mem set x

let setup_ml_compile_rules
      ?(disallowed_module_dep = fun _ -> None)
      dc ~dir ~libname ~wrapped ~for_executable ~can_setup_inline_runners
      ~preprocessor_deps ~preprocess_spec ~names_spec =
  let default_pp = None in
  let names_spec_intfs, names_spec_impls, names_spec_modules =
    eval_names_spec ~dc names_spec
  in
  if not for_executable then
    begin
      List.iter names_spec_modules ~f:(fun name ->
        if wrapped
        then
          (if BN.is_lib ~libname name then
             failwithf
               "Library %S has a module with the same name %S in it. \
                This is not allowed for wrapped libraries."
               (Libname.to_string libname)
               (BN.to_string name)
               ())
        else
          (if not (BN.is_lib ~libname name) then
             failwithf
               "Library %S has a module with a different name %S in it. \
                This is not allowed for non-wrapped libraries."
               (Libname.to_string libname)
               (BN.to_string name)
               ()))
    end;
  let names_spec_has_impl = mem_of_list names_spec_impls in
  let names_spec_has_intf = mem_of_list names_spec_intfs in
  let lookup_pp = unstage (lookup_pp dc ~default_pp ~preprocess_spec) in
  List.concat_map names_spec_modules ~f:(fun name ->
    let exists_ml = names_spec_has_impl name in
    let exists_mli = names_spec_has_intf name in
    match exists_ml with
    | false ->
      if exists_mli
      then failposf ~pos:(dummy_position (BN.suffixed ~dir name ".mli"))
             "this .mli doesn't have a corresponding .ml" ()
      else failposf ~pos:(dummy_position (User_or_gen_config.source_file ~dir))
             !"there is neither .ml nor .mli for module %{BN}" name ()
    | true ->
      let must_be_sharable = false in
      let mc = {MC.
        dc;
        dir;
        libname;
        can_setup_inline_runners;
        x_libs = [];
        pp_style = lookup_pp name;
        preprocessor_deps;
        exists_ml;
        exists_mli;
        wrapped;
        must_be_sharable;
      }
      in
      let disallowed_module_dep x =
        if names_spec_has_impl x || names_spec_has_intf x
        then disallowed_module_dep x
        else Some (sprintf !"%{BN} is not part of the library/executable spec" x)
      in
      List.concat [
        [gen_cmideps dc name];
        generate_ppxed_and_diff mc ~name;
        (if for_executable
         then [gen_objdeps ~dir name ~exists_ml]
         else []);
        (if exists_ml then List.concat [
          generate_pp mc ~kind:ML ~name;
          [gen_dfile ML ~disallowed_module_dep mc ~name;
           byte_compile_ml mc ~name;
           native_compile_ml mc ~name;
           infer_mli_auto mc ~name];
         ]
         else []);
        (if exists_mli then List.concat [
          generate_pp mc ~kind:MLI ~name;
          [gen_dfile MLI ~disallowed_module_dep mc ~name;
          compile_mli mc ~name;]
        ] else []);
      ]
  )
;;

(*----------------------------------------------------------------------
 preprocessor_conf
----------------------------------------------------------------------*)

(* .cmx/.o -> .cmxs *)
let share_preprocessor dc ~dir (name : BN.t) : Rule.t =
  let {DC. ocamlflags; ocamloptflags; _} = dc in
  let file suf = BN.suffixed ~dir name suf in
  let cmx = file ".cmx" in
  let o = file ".o" in
  let cmxs = file ".cmxs" in
  let must_be_sharable = true in
  let flags = ocamlflags @ ocamloptflags in
  let flags = if must_be_sharable then remove_nodynlink flags else flags in
  simple_rule ~deps:[Dep.path cmx; Dep.path o] ~targets:[cmxs] ~action:(
    Action.process ~dir ocamlopt_path
      (flags @ ["-shared"; "-o"; basename cmxs; basename cmx])
  )

module Preprocessor_conf_interpret = struct

  include Preprocessor_conf

  let disabled_warnings t =
    Compiler_selection.disabled_warnings @ t.extra_disabled_warnings

  let ocamlflags t =
    ["-I"; "+compiler-libs"] @
    Top.default_ocamlflags ~disabled_warnings:(disabled_warnings t)

  let extend_dc t dc =
    let ocamlflags = ocamlflags t in
    {dc with DC.
      ocamlflags;
    }

end

let preprocessor_rules dc ~dir conf =
  let wrapped = false in
  let dc = Preprocessor_conf_interpret.extend_dc conf dc in
  let libname = LN.of_string (Preprocessor_conf.name conf) in
  let can_setup_inline_runners = false in
  let preprocess_spec = Preprocessor_conf.preprocess conf in
  let default_pp = Some camlp4orf_path in
  let name = BN.of_libname libname in
  let pp_style = unstage (lookup_pp dc ~default_pp ~preprocess_spec) name in
  let {DC.impl_is_buildable; intf_is_buildable;_} = dc in
  let exists_ml = impl_is_buildable name in
  let exists_mli = intf_is_buildable name in
  let must_be_sharable = true in
  let mc = {MC.
    dc;
    dir;
    libname;
    can_setup_inline_runners;
    x_libs = [];
    pp_style;
    preprocessor_deps = [];
    exists_ml;
    exists_mli;
    wrapped;
    must_be_sharable;
  }
  in
  assert (BN.is_lib name ~libname);
  let default_rule =
    let suffixes = [".cmi";".cmo"] in
    Rule.default ~dir (
      List.map suffixes ~f:(fun suf ->
        Dep.path (BN.suffixed ~dir name suf))
    )
  in
  List.concat [
    [gen_interface_deps_from_objinfo dc ~dir ~wrapped ~libname];
    [Lib_modules.rule ~dir ~libname Lib_modules.empty];
    [gen_cmideps dc name];
    [
      gen_dfile ML ~disallowed_module_dep:(fun _ -> None) mc ~name;
      stub_names_rule ~stub_names:[] ~dir ~libname;
      byte_compile_ml mc ~name;
      native_compile_ml mc ~name ;
      share_preprocessor dc ~dir name;
      default_rule;
    ];
    generate_pp mc ~kind:ML ~name;
    generate_ppxed_and_diff mc ~name;
    [infer_mli_auto mc ~name];
    if exists_mli then ([
      gen_dfile MLI ~disallowed_module_dep:(fun _ -> None) mc ~name;
      compile_mli mc ~name;
    ] @ generate_pp mc ~kind:MLI ~name)
    else [];
  ]

(*----------------------------------------------------------------------
 pack ordering
----------------------------------------------------------------------*)

module Ordering = struct
  let find_shortest_cycle_using_floyd_warshal
        ~dir (graph : (BN.t * BN.t list) list) : BN.t list =
    (* cycles (especially in core) are already confusing enough that there is no need to
       put unrelated modules in there *)
    let to_int, of_int =
      let h1 = Int.Table.create () in
      let h2 = BN.Table.create () in
      List.iteri graph ~f:(fun i (name, _) ->
        Hashtbl.add_exn h1 ~key:i ~data:name;
        Hashtbl.add_exn h2 ~key:name ~data:i;
      );
      (fun name ->
         match Hashtbl.find h2 name with
         | Some x -> x
         | None ->
           failwithf !"The library in %S doesn't contain the module %S \
                       but depends on it" dir (BN.to_string name) ()),
      (fun i -> Hashtbl.find_exn h1 i)
    in
    let n = List.length graph in
    let dist = Array.init n ~f:(fun _ -> Array.create ~len:n 100000) in
    let next = Array.init n ~f:(fun _ -> Array.create ~len:n None) in
    List.iter graph ~f:(fun (name, deps) ->
      List.iter deps ~f:(fun dep ->
        dist.(to_int name).(to_int dep) <- 1
      )
    );
    for k = 0 to n - 1 do
      for i = 0 to n - 1 do
        for j = 0 to n - 1 do
          if dist.(i).(k) + dist.(k).(j) < dist.(i).(j) then begin
            dist.(i).(j) <- dist.(i).(k) + dist.(k).(j);
            next.(i).(j) <- Some k
          end
        done;
      done;
    done;
    let min_index = ref (-1) in
    let min_len = ref 100000000 in
    for i = 0 to n - 1; do
      if dist.(i).(i) < !min_len then begin
        min_len := dist.(i).(i);
        min_index := i
      end
    done;
    let rec build_cycle acc i j =
      match next.(i).(j) with
      | None -> acc
      | Some k -> build_cycle (of_int k :: build_cycle acc i k) k j
    in
    build_cycle [of_int !min_index] !min_index !min_index

  let pack ~(module_and_dfiles : (BN.t*Path.t)list) ~target =
    let rec loop acc = function
      | [] -> return (List.rev acc)
      | (name, dfile) :: tl ->
        BN.file_words dfile *>>= fun words ->
        loop ((name, words) :: acc) tl
    in
    loop [] module_and_dfiles *>>| fun l ->
    let alist_from_module_to_pack_dep =
      (* regroup the dependencies from the mli.d and the ml.d together *)
      Map.to_alist (BN.Map.of_alist_fold l ~init:[] ~f:(@)) in
    let rec sort rev_acc alist =
      (* returns a deterministic list: the smallest permutation of the list of modules
         wrt to lexicographic comparison on the list of string that respects the
         dependencies *)
      if List.is_empty alist then List.rev rev_acc else begin
        match List.find_map alist ~f:(fun (mod_, deps) ->
          if List.is_empty deps then Some mod_ else None)
        with
        | None ->
          let target = Path.to_string target in
          let dir = Filename.dirname target in
          let base = Filename.basename target in
          let cycle = find_shortest_cycle_using_floyd_warshal ~dir alist in
          (* putting the output if whatever format will please the omake-mode *)
          Async.Std.Print.printf "\n- build %s %s\n" dir base;
          Async.Std.Print.printf "File \"jbuild\", line 1, characters 1-1:\n";
          Async.Std.Print.printf "Error: dependency cycle: %s\n"
            (String.concat ~sep:" " (List.map cycle ~f:BN.to_string));
          Async.Std.Print.printf "- exit %s %s\n%!" dir base;
          raise Exit
        | Some mod_ ->
          let alist = List.filter alist ~f:(fun (mod', _) -> BN.(<>) mod_ mod') in
          let alist = List.map alist ~f:(fun (mod1, deps) ->
            (mod1, List.filter deps ~f:(fun dep -> BN.(<>) mod_ dep))
          ) in
          let rev_acc = mod_ :: rev_acc in
          sort rev_acc alist
      end
    in
    sort [] alist_from_module_to_pack_dep

  let comparison ~wrapped ~dir ~libname =
    let order_file = pack_order_file ~dir ~libname in
    BN.file_words order_file *>>| fun ordered_list ->
    let ordered_list = List.map ordered_list ~f:(PN.of_barename ~wrapped ~libname) in
    let ordered_list = List.map ordered_list ~f:PN.to_string in
    let _, map =
      List.fold ~init:(0, String.Map.empty) ordered_list ~f:(fun (count, map) elt ->
        (count + 1, Map.add map ~key:elt ~data:count)
      ) in
    fun elt1 elt2 ->
      match Map.find map elt1 with
      | None ->
        failwithf "can't find %s in %s" elt1 (String.concat ~sep:" " ordered_list) ()
      | Some count1 ->
        match Map.find map elt2 with
        | None ->
          failwithf "can't find %s in %s" elt2 (String.concat ~sep:" " ordered_list) ()
        | Some count2 -> Int.compare count1 count2

  let sort ~wrapped ~dir ~libname unsorted_cmxs =
    comparison ~wrapped ~dir ~libname *>>| fun comparison ->
    List.sort unsorted_cmxs ~cmp:(fun cmx1 cmx2 ->
      let mod1 = Filename.chop_extension cmx1 in
      let mod2 = Filename.chop_extension cmx2 in
      comparison mod1 mod2)

end

(* rules that generate libname.pack-order, that contains the modules of the libraries
   sorted in topological order *)
let library_module_order ~dir ~impls ~intfs ~libname =
  let module_and_dfiles =
    List.map impls ~f:(fun impl ->
      impl,
      BN.suffixed ~dir impl ".ml.d"
    )
    @ List.map intfs ~f:(fun intf ->
      intf,
      BN.suffixed ~dir intf ".mli.d"
    )
  in
  let target = pack_order_file ~dir ~libname in
  Rule.create ~targets:[target] (
    Ordering.pack ~module_and_dfiles ~target *>>| fun sorted_modules ->
    write_string_action (String.concat ~sep:" " (List.map sorted_modules ~f:BN.to_string))
      ~target
  )

(*----------------------------------------------------------------------
 building ocaml libraries - packing/arching
----------------------------------------------------------------------*)

let ocaml_library_archive dc ~dir ~impls ~wrapped ~libname =
  let prefixing = do_prefixing ~wrapped in
  let {DC. ocamlflags; ocamlcflags; ocamloptflags; ocamllibflags; _} = dc in
  assert (not wrapped ==> not prefixing);
  assert (not wrapped ==> Int.(List.length impls = 0));
  let impls = List.map impls ~f:(PN.of_barename ~wrapped ~libname) in
  let impls = List.map impls ~f:PN.to_string in
  List.concat_map [ [], LN.to_string libname
                  ; [ "-linkall" ], LN.to_string libname ^ ".linkall"
                  ]
    ~f:(fun (linkall, cmxa_without_suf) ->
      List.map
        [ ocamlc_path, ocamlcflags, ".cma", [], ".cmo", []
        ; ocamlopt_path, ocamloptflags, ".cmxa", [".a"], ".cmx", [".o"]
        ] ~f:(fun (ocamlcomp, ocamlcompflags, lib, lib_implicit, mod_, mod_implicit) ->
          let targets = List.concat_cartesian_product [cmxa_without_suf] (lib :: lib_implicit) in
          let deps =
            let impls = impls @ [LN.to_string libname] in
            List.concat_cartesian_product impls (mod_ :: mod_implicit)
          in
          let unsorted_mod_args = List.concat_cartesian_product impls [mod_] in
          relative_rule ~dir ~targets ~deps ~non_relative_deps:[] (
            (begin match unsorted_mod_args with
             | [] | [_] -> return unsorted_mod_args
             | _ :: _ :: _ -> Ordering.sort ~wrapped ~dir ~libname unsorted_mod_args
             end *>>| fun sorted_mod_args ->
             let sorted_mod_args = sorted_mod_args @ [LN.to_string libname ^ mod_] in
             Action.process ~dir ocamlcomp
               (ocamlflags
                @ ocamlcompflags
                @ sorted_mod_args
                @ ocamllibflags
                @ linkall
                @ ["-a"; "-o"; cmxa_without_suf ^ lib ])))))
;;

let ocaml_library_pack =
  let native_action ~dir ~prog ~args ~target:_ = Action.process ~dir prog args in
  let tmpdir = ".tempdir_for_cmo_packing" in
  let bytecode_action ~dir ~prog ~args ~target =
    (* Build the packed .cmo in a temporary sub-dir to avoid clashing with native
       compilation.  Name the subdir with a leading "." to avoid triggering jenga subdir
       build-setup *)
    Bash.action ~dir [
      bash1 "rm" ["-rf"; tmpdir];
      bash1 "mkdir" [tmpdir];
      bash1 prog args;
      bash1 "mv" [tmpdir ^ "/" ^ target; target];
      bash1 "rm" ["-rf"; tmpdir];
    ]
  in
  fun dc ~dir ~impls ~libname ~exists_libname_cmi ->
    assert(select_lib_packing);
    let wrapped = true in (* doesn't matter *)
    let impls = List.map impls ~f:BN.to_string in
    let {DC. ocamlflags; ocamloptflags; ocamlcflags; _} = dc in
    List.map
      [ ocamlc_path, ocamlcflags, ".cmo", [], Some (tmpdir ^ "/"), bytecode_action
      ; ocamlopt_path, ocamloptflags, ".cmx", [".o"], None, native_action
      ] ~f:(fun (ocamlcomp, ocamlcompflags, mod_, mod_implicit, target_subdir, action) ->
        let unsorted_mod_args = List.concat_cartesian_product impls [mod_] in
        let mod_deps = List.concat_cartesian_product impls (".cmi" :: mod_ :: mod_implicit) in
        let target_arg = LN.to_string libname ^ mod_ in
        let targets = List.concat_cartesian_product [LN.to_string libname] (mod_ :: mod_implicit) in
        let mod_deps, targets =
          let intf = LN.to_string libname ^ ".cmi" in
          if exists_libname_cmi
          then intf :: mod_deps, targets
          else mod_deps, (if Option.is_none target_subdir then intf :: targets else targets)
        in
        let flags = ocamlflags @ ocamlcompflags in
        let targets =
          let cmt = LN.to_string libname ^ ".cmt" in
          if target_subdir = None && (List.mem flags "-bin-annot") then
            cmt :: targets
          else targets
        in
        relative_rule
          ~dir
          ~targets
          ~deps:mod_deps
          ~non_relative_deps:[]
          (Ordering.sort ~wrapped ~dir ~libname  unsorted_mod_args
           *>>| fun sorted_mod_args ->
           let args =
             flags
             @ ["-g"]
             @ ["-pack"; "-o"; (Option.value target_subdir ~default:"") ^ target_arg]
             @ sorted_mod_args
           in
           action ~dir ~prog:ocamlcomp ~args ~target:target_arg
          )
      )

(*----------------------------------------------------------------------
 copy_config_from_core
----------------------------------------------------------------------*)

let atomic_copy_action ~source ~target =
  let dir = dirname target in
  bashf ~dir
    !"tmp=\"$(mktemp --tmpdir=./)\"; cp -p -- %{quote} \"$tmp\"; mv -- \"$tmp\" %{quote}"
    (reach_from ~dir source)
    (basename target)

let atomic_copy_rule ~source ~target =
  simple_rule ~targets:[target] ~deps:[Dep.path source]
    ~action:(atomic_copy_action ~source ~target)

let copy_config_from_core ~dir =
  let core_dir = root_relative "lib/core/src" in
  let files_to_copy  = ["config.h"; "config.mlh"] in
  List.map files_to_copy ~f:(fun name ->
    let source = relative ~dir:core_dir name in
    let target = relative ~dir name in
    atomic_copy_rule ~source ~target
  )

(*----------------------------------------------------------------------
 hg_version
----------------------------------------------------------------------*)

(* Given that version_util is made available in core, there is little point in adding
   version util (same thing for build_info) to executables without access to it. This way
   we avoid the preprocessor executables constantly changing and causing jenga to do
   spurious work. *)
let hg_version_required_by = LN.of_string "core"

let hg_dirstate_suffix =
  Dep.deferred (fun () ->
    let open Async.Std in
    run_action_now_stdout (
      Action.process ~ignore_stderr:true ~dir:Path.the_root
        hg_prog ["showconfig"; "jhg.omake-dirstate-suffix";]
    ) >>| String.strip)

let hg_version_out = root_relative "hg_version.out"

let hg_version_out_rule =
  Rule.create ~targets:[hg_version_out] (
    begin
      if not version_util_support
      then return "NO_VERSION_UTIL"
      else
        Dep.both hg_dirstate_suffix all_the_repos
        *>>= fun (dirstate_suffix, repos) ->
        Dep.all (List.map repos ~f:(fun repo ->
          Dep.action_stdout (
            Dep.path (Path.relative ~dir:repo (".hg/dirstate" ^ dirstate_suffix))
            *>>| fun () ->
            bash ~ignore_stderr:true ~dir:repo
              "echo -n \"$(hg showconfig 'paths.default')_$(hg id -i)\""
          )
        ))
        *>>| String.concat ~sep:"\n"
    end
    *>>| write_string_action ~target:hg_version_out)

let hg_version_base ~base = base ^ ".hg_version"

let generate_static_string_c_code_sh = bin_relative "generate_static_string_c_code.sh"

let hg_version_rules ~dir ~exe =
  let base = hg_version_base ~base:exe in
  let c = relative ~dir (base ^ ".c") in
  let o = relative ~dir (base ^ ".o") in
  let o_rule =
    simple_rule ~targets:[o] ~deps:[Dep.path c]
      ~action:(
        Action.process ~dir ocamlc_path [basename c; "-o"; basename o;]
      )
  in
  let c_rule =
    simple_rule ~targets:[c]
      ~deps:[Dep.path generate_static_string_c_code_sh; Dep.path hg_version_out]
      ~action:(
        bashf ~dir !"%{quote} __wrap_generated_hg_version < %{quote} > %{quote}"
          (reach_from ~dir generate_static_string_c_code_sh)
          (reach_from ~dir hg_version_out)
          (basename c))
  in
  [c_rule; o_rule]

(*----------------------------------------------------------------------
 build_info
----------------------------------------------------------------------*)

let build_info_required_by = LN.of_string "core"

let build_info_base ~base = base ^ ".build_info"

let build_info_sh = bin_relative "build_info.sh"

let link_deps_of_forced_lib (module Mode : Ocaml_mode.S) ~dir lib =
  (* Not going through liblinks, because this is only when linking a library from the
     current directory. Also we can't go through liblink_default, otherwise we create
     dependency cycles with TRANSITIVE_RUNNERS. *)
  let linkall_cmxa = LN.to_string lib ^ ".linkall" in
  List.map Mode.cmxa_and_a ~f:(fun suf ->
    Dep.path (relative ~dir (linkall_cmxa ^ suf)))
  @ [stubs_dependencies ~dir ~lib]
;;

let link_deps_of_libs mode ~dir ~libs_maybe_forced ~force_link ~build_libs_DEFAULT =
  (* Both the rules which generate the .exe and which generate the .build_info
     depend on these link_deps *)
  let unforced ~libs =
    link_deps_of_unforced_libs mode ~libs
    @ (if build_libs_DEFAULT then LL.liblink_default ~libs else [])
  in
  (match force_link with
  | None -> unforced ~libs:libs_maybe_forced
  | Some lib ->
    unforced ~libs:(List.filter libs_maybe_forced ~f:((<>) lib))
    @ link_deps_of_forced_lib mode ~dir lib)
;;

let link_deps_of_version_util (module Mode : Ocaml_mode.S) ~dir ~suppress_version_util ~libs exe =
  if not suppress_version_util && List.mem libs hg_version_required_by then
    [ Dep.path (relative ~dir
                  (hg_version_base ~base:(exe ^ Mode.exe) ^ ".o"))
    ]
  else
    []
;;

let link_deps_of_objs (module Mode : Ocaml_mode.S) objs =
  List.concat_map objs ~f:(fun (dir, base) ->
    List.map Mode.cmx_and_o ~f:(fun suf ->
      Dep.path (Path.relative ~dir (base ^ suf))))
;;

let build_info_rules ~dir ~exe ~suf ~sexp_dep =
  let base = build_info_base ~base:(exe ^ suf) in
  let sexp_file = relative ~dir (base ^ ".sexp") in
  let o = relative ~dir (base ^ ".o") in
  let o_rule =
    (* The .c is not a separate rule, so dependencies on *.c (like in
       external/lacaml/lib) do not depend spuriously on this C file for
       the inline benchmarks, creating a dependency cycle. *)
    simple_rule ~targets:[o]
      ~deps:[Dep.path generate_static_string_c_code_sh; Dep.path sexp_file]
      ~action:(
        bashf ~dir !"tmp=$(mktemp --tmpdir build_infoXXXXXX.c) && \
                     %{quote} __wrap_generated_build_info < %{quote} > \"$tmp\" && \
                     %{quote} \"$tmp\" && \
                     mv \"$(basename \"$tmp\" .c)\".o %{quote}"
          (reach_from ~dir generate_static_string_c_code_sh)
          (basename sexp_file)
          ocamlc_path
          (basename o))
  in
  let sexp_rule =
    Rule.create ~targets:[sexp_file] (
      Dep.all_unit [ sexp_dep; Dep.path build_info_sh ] *>>| fun () ->
      let application_specific_fields =
        Option.value build_info_app_fields ~default:String.Map.empty
        |> Map.add ~key:"use_new_sql" ~data:(Bool.sexp_of_t use_new_sqml)
      in
      let build_info =
        [%sexp
          { x_library_inlining = (x_library_inlining : bool);
            ocaml_version = (Compiler_selection.compiler_dir : string);
            executable_path = (suffixed ~dir exe suf : Path.t);
            build_system = "jenga";
            nodynlink = (nodynlink : bool);
            packing = (select_lib_packing : bool);
            application_specific_fields =
              (application_specific_fields : Sexp.t String.Map.t);
          }
        ]
      in
      let build_info =
        match build_info with
        | Sexp.Atom _ -> assert false
        | Sexp.List l -> String.concat ~sep:" " (List.map l ~f:Sexp.to_string)
      in
      bashf ~dir !"%{quote} %{quote} > %{quote}"
        (reach_from ~dir build_info_sh)
        build_info
        (basename sexp_file)
    )
  in
  [sexp_rule; o_rule]

(*----------------------------------------------------------------------
 top levels - utop
----------------------------------------------------------------------*)

(* UTop dependencies *)
module UTop = struct
  let includes = [
    "+compiler-libs";
    "+ocamldoc";
  ]

  let packs = ocamlpacks @ [
    "ocamlcommon";
    "ocamlbytecomp";
    "ocamloptcomp";
    "ocamlopttoplevel";
  ]

  let ppx () =
    let path = PPXset.create [PP.of_string "JANE"] |> PPXset.exe_path in
    assert
      (* this path is hard-coded in [lib/js_utop/src/main.ml]: *)
      (root_relative ".ppx/JANE/ppx.exe" = path);
    path

end

let utopdeps_rules ~dir ~libname =
  let base = "utop.bc.deps" in
  let c = relative ~dir (base ^ ".c") in
  let o = relative ~dir (base ^ ".o") in
  let o_rule =
    simple_rule ~targets:[o] ~deps:[Dep.path c]
      ~action:(
        Action.process ~dir ocamlc_path [basename c; "-o"; basename o;]
      )
  in
  let c_rule =
    Rule.create ~targets:[c] (
      LN.file_words (LN.suffixed ~dir libname ".libdeps") *>>= fun libs ->
      let libs = libs @ [libname] in
      Dep.path generate_static_string_c_code_sh *>>| fun () ->
      bashf ~dir "echo -n %S | %s generated_utop_deps > %s"
        (String.concat (List.map libs ~f:LN.to_string) ~sep:" ")
        (reach_from ~dir generate_static_string_c_code_sh)
        (basename c)
      )
  in
  [c_rule; o_rule]

let utop_rules dc ~dir ~libname =
  let {DC. ocamlflags; no_utop_alias; _ } = dc in
  let targets = [relative ~dir "utop"] in
  let hg_version = relative ~dir "utop.bc.hg_version.o" in
  let build_info = relative ~dir "utop.bc.build_info.o" in
  let utopdeps   = relative ~dir "utop.bc.deps.o"       in
  let utop_rule =
    Rule.create ~targets (
      LN.file_words (LN.suffixed ~dir libname ".libdeps") *>>= fun libs ->
      let libs = libs @ [libname] in
      libdeps_for dc.DC.libmap [LN.of_string "js_utop"] *>>= fun x_libs ->
      let libs = LN.remove_dups_preserve_order (libs @ x_libs) in
      Dep.all_unit [
        Dep.all_unit (
          link_deps_of_unforced_libs Ocaml_mode.native ~libs);
        Dep.path hg_version;
        Dep.path build_info;
        Dep.path utopdeps;
        Dep.path (UTop.ppx ());
        Dep.all_unit (LL.liblink_deps ~libs ~suffixes:[".cmi"]);
      ] *>>| fun () ->
      let standard_cmxas = List.map UTop.packs ~f:(fun name -> name ^ ".cmxa") in
      let lib_cmxas =
        List.concat_map libs ~f:(fun lib -> [
            "-I"; reach_from ~dir (LL.liblink_dir ~lib);
            LN.to_string lib ^ ".cmxa";
          ])
      in
      let includes =
        List.concat_map UTop.includes ~f:(fun dir -> ["-I"; dir])
      in
      let args = List.concat [
        use_compiler_flavor `Cxx;
        ["-ccopt"; quote "-Wl,-E"];
        ["-linkall"];
        ("-g" :: ocamlflags);
        includes;
        standard_cmxas;
        args_for_linker ["--wrap=generated_build_info"]; [basename hg_version];
        args_for_linker ["--wrap=generated_hg_version"]; [basename build_info];
        [basename utopdeps];
        lib_cmxas;
        ["-o"; "utop";];
      ]
      in
      Bash.action ~dir [bash1 ocamlopt_path args]
    )
  in
  List.concat [
    hg_version_rules ~dir ~exe:"utop.bc";
    build_info_rules ~dir ~exe:"utop" ~suf:".bc" ~sexp_dep:(return ());
    utopdeps_rules ~dir ~libname;
    [
      utop_rule;
      Rule.alias (Alias.utop ~dir) (
        if no_utop_alias then [] else
          [Dep.path (relative ~dir "utop")]
      );
    ]
  ]

(*----------------------------------------------------------------------
  Rules that check if libraries define tests or benchs
----------------------------------------------------------------------*)
let fgrep_rule ~dir ~filename ~macros ~impls =
  let target = relative ~dir filename in
  let sources = List.map impls ~f:(fun impl -> BN.suffixed ~dir impl ".ml") in
  simple_rule
    ~targets:[target]
    ~deps:(List.map sources ~f:Dep.path)
    ~action:(
      bashf ~dir !"\
 (cat %s | fgrep -w -f <(%s) || true) > %{quote}"
        (concat_quoted (List.map ~f:basename sources))
        (String.concat ~sep:"; " (List.map macros ~f:(sprintf !"echo %{quote}")))
        (basename target)
    )

let fgrep_inline_test_filename = "fgrep_inline_tests.out"
let inline_test_macros = ["TEST"; "TEST_UNIT"; "TEST_MODULE";
                          "let%test"; "let%test_unit"; "let%test_module"]

let fgrep_expect_test_filename = "fgrep_expect_tests.out"
let expect_test_macros = ["let%expect_test" ]

let fgrep_bench_filename = "fgrep_bench.out"
let bench_macros = ["BENCH"; "BENCH_FUN"; "BENCH_MODULE"; "BENCH_INDEXED";
                    "let%bench"; "let%bench_fun"; "let%bench_module"]

(*----------------------------------------------------------------------
 check_ldd_deps
----------------------------------------------------------------------*)

module Check_ldd_dependencies : sig

  val check :
    allowed:Ordered_set_lang.t
    -> target:Path.t -> (Path.t -> Action.t Dep.t) -> Rule.t list

end = struct

  (* This list contains dynamic libraries that we know are present on all prod boxes. It
     is useful because if you restrict yourself to this standard set of dynamic
     dependencies, your executable should run on all prod machines.
     So if you want to extend this list, check with the sysadmins whether the new library
     you want satisfies this condition. *)
  let standard = [
    "libc.so.6";
    "libcares.so.2";
    "libdl.so.2";
    "libgcc_s.so.1";
    "libgmp.so.3";
    "libjswrap.so.1";
    "libm.so.6";
    "libnsl.so.1";
    "libpcre.so.0";
    "libpthread.so.0";
    "librt.so.1";
    "libstdc++.so.6";
    "libtevent.so.0";
    "libtinfo.so.5";
    "libz.so.1";
    "linux-vdso.so.1";
  ]

  let dynamic_lib_deps_sh = bin_relative "dynamic-lib-deps.sh"

  let check ~allowed ~target mk_action =
    let allowed = Ordered_set_lang.eval_with_standard (Some allowed) ~standard in
    let is_unexpected =
      let set = String.Hash_set.of_list allowed in
      fun name -> not (Hash_set.mem set name)
    in
    let dir = dirname target in
    let unchecked = suffixed ~dir (basename target) ".unchecked-do-not-deploy" in
    [
      Rule.create ~targets:[unchecked] (mk_action unchecked);
      Rule.create ~targets:[target] (
        (* The unchecked .exe is a dependency for the checking-action AND the cp/rm *)
        Dep.path unchecked *>>= fun () ->
        Dep.action_stdout (
          Dep.all_unit [Dep.path dynamic_lib_deps_sh; Dep.path unchecked] *>>| fun () ->
          Action.process ~dir:Path.the_root
            (Path.to_string dynamic_lib_deps_sh)
            [Path.to_string unchecked]
        ) *>>| fun stdout ->
        let actual = words_of_string stdout in
        let unexpected = List.filter ~f:is_unexpected actual in
        match unexpected with
        | [] ->
          atomic_copy_action ~source:unchecked ~target
        | _ :: _ ->
          put "Removing executable with unexpected ldd deps: %s" (Path.to_string target);
          put "Allowed   : %s" (String.concat ~sep:" " allowed);
          put "Actual    : %s" (String.concat ~sep:" " actual);
          remove_target_and_fail ~target ~filename_for_error:"jbuild"
            "Unexpected dynamic dependencies: %s"
            (String.concat ~sep:" " unexpected)
      );
    ]

end


(*----------------------------------------------------------------------
 link_native
----------------------------------------------------------------------*)

let ocaml_plugin_handling dc ~dir name =
  let ocaml_plugin_o = BN.to_string name ^ ".ocaml_plugin.o" in
  match dc.DC.ocaml_plugin_libraries (BN.to_string name) with
  | None -> `Not_a_plugin
  | Some libs_for_plugins ->
    let libs_for_plugins =
      libs_for_plugins
      (* i.e. to remove "str" and friends *)
      |> List.filter ~f:(fun x -> not (List.mem ocamlpacks x))
      |> List.map ~f:LN.of_string
    in
    (* This linkall flag, together with the libs for plugins we add, means we link in the
       transitive implementations of the embedded library signatures, which is enough to
       guarantee that any code that types using these signatures will be loadable. *)
    `Plugin
      (libs_for_plugins,
       Dep.path (relative ~dir ocaml_plugin_o),
       "-linkall" ::
       ocaml_plugin_o ::
       args_for_linker ["--wrap=ocaml_plugin_archive"]
       @ args_for_linker ["--wrap=ocaml_plugin_archive_digest"])

let get_libs_for_exe ~link_libdeps_of ~libs_for_plugins ~force_link =
  Dep.both
    (Dep.all (List.map link_libdeps_of ~f:(fun (dir, libname) ->
       LN.file_words (LN.suffixed ~dir libname ".libdeps"))))
    (libs_transitive_closure libs_for_plugins)
  *>>| fun (normal_libss, libs_for_plugins) ->
  LN.remove_dups_preserve_order (List.concat normal_libss @ libs_for_plugins @
                                 Option.to_list force_link)
;;

let link (module Mode : Ocaml_mode.S) (dc : DC.t) ~dir
      ~link_flags
      ~ocamlpacks
      ~allowed_ldd_dependencies
      ~ocaml_plugin_handling
      ~link_libdeps_of
      ~suppress_build_info
      ~suppress_version_util
      ~build_libs_DEFAULT
      ~compute_objs
      ~exe
      ~force_link =
  let ocamlopt_path, ocamloptflags =
    match Mode.which with
    | `Byte -> ocamlc_path, dc.ocamlcflags
    | `Native -> ocamlopt_path, dc.ocamloptflags
  in
  let hg_version_o = relative ~dir (hg_version_base ~base:(exe ^ Mode.exe) ^ ".o") in
  let build_info_o = relative ~dir (build_info_base ~base:(exe ^ Mode.exe) ^ ".o") in

  let maybe_ldd_check_rules ~target mk_action =
    match allowed_ldd_dependencies with
    | None -> [Rule.create ~targets:[target] (mk_action target)] (* no check *)
    | Some allowed -> Check_ldd_dependencies.check ~allowed ~target mk_action
  in

  let libs_for_plugins, ocaml_plugin_deps, ocaml_plugin_flags =
    match ocaml_plugin_handling with
    | `Not_a_plugin -> [], [], []
    | `Plugin (libs_for_plugins, flags_dep, flags) ->
      libs_for_plugins, [flags_dep], flags
  in

  let exe_rules =
    let target = suffixed ~dir exe Mode.exe in
    maybe_ldd_check_rules ~target (fun target ->
      let exe_maybe_tmp = basename target in
      link_libdeps_of
      *>>= fun link_libdeps_of ->
      get_libs_for_exe ~link_libdeps_of ~libs_for_plugins ~force_link
      *>>= fun libs_maybe_forced ->
      compute_objs *>>= fun objs ->
      let build_info_dep, build_info_args =
        if not suppress_build_info && List.mem libs_maybe_forced build_info_required_by
        then [Dep.path build_info_o],
             args_for_linker ["--wrap=generated_build_info"] @ [ basename build_info_o ]
        else [], []
      in
      let version_util_args =
        if not suppress_version_util && List.mem libs_maybe_forced hg_version_required_by
        then args_for_linker ["--wrap=generated_hg_version"] @ [ basename hg_version_o ]
        else []
      in
      let deps =
        link_deps_of_libs (module Mode) ~dir ~libs_maybe_forced ~force_link
           ~build_libs_DEFAULT
        @ link_deps_of_version_util (module Mode) ~dir ~suppress_version_util
            ~libs:libs_maybe_forced exe
        @ link_deps_of_objs (module Mode) objs
        @ build_info_dep
        @ [Dep.path link_quietly]
        @ ocaml_plugin_deps
      in
      Dep.all_unit deps
      *>>| fun () ->
      let cmxa_for_packs = List.map ocamlpacks ~f:(fun name -> name ^ Mode.cmxa) in
      let sub_cmxs_in_correct_order =
        List.map objs ~f:(fun (obj_dir, base) ->
          Path.reach_from ~dir (Path.relative ~dir:obj_dir (base ^ Mode.cmx))) in
      let lib_cmxas =
        List.concat_map libs_maybe_forced ~f:(fun lib ->
          if Some lib = force_link then
            [ "-I"; "."; LN.to_string lib ^ ".linkall" ^ Mode.cmxa]
          else
            [ "-I"; reach_from ~dir (LL.liblink_dir ~lib); LN.to_string lib ^ Mode.cmxa ])
      in
      bash ~dir (
        concat_quoted (List.concat [
          [reach_from ~dir link_quietly;
           ocamlopt_path];
          dc.ocamlflags; ocamloptflags;
          link_flags;
          use_compiler_flavor `Cxx;
          (match Mode.which with
           | `Native -> []
           | `Byte -> ["-custom"]);
          cmxa_for_packs;
          build_info_args;
          version_util_args;
          ocaml_plugin_flags;
          lib_cmxas;
          sub_cmxs_in_correct_order;
          ["-o"; exe_maybe_tmp];
        ])
      )
    )
  in

  let js_of_ocaml_rules =
    match Mode.which, Compiler_selection.javascript with
    | `Byte, true ->
      let bytecode_exe = suffixed ~dir exe Mode.exe in
      let target_js = suffixed ~dir exe ".js" in
      [Js_of_ocaml.rule ~dir ~flags:dc.js_of_ocaml_flags ~src:bytecode_exe ~target:target_js]
    | `Byte, false
    | `Native, _   -> []
  in

  let sexp_dep =
    link_libdeps_of
    *>>= fun link_libdeps_of ->
    get_libs_for_exe ~link_libdeps_of ~libs_for_plugins ~force_link
    *>>= fun libs_maybe_forced ->
    compute_objs *>>= fun objs ->
    Dep.all_unit (List.concat [
      link_deps_of_libs (module Mode) ~dir ~libs_maybe_forced ~force_link
        ~build_libs_DEFAULT;
      link_deps_of_version_util (module Mode) ~dir ~suppress_version_util
        ~libs:libs_maybe_forced exe;
      link_deps_of_objs (module Mode) objs;
      [Dep.path build_info_sh];
      ocaml_plugin_deps;
    ])
  in
  List.concat [
    exe_rules;
    js_of_ocaml_rules;
    (if suppress_version_util
     then []
     else hg_version_rules ~dir ~exe:(exe ^ Mode.exe));
    (if suppress_build_info
     then []
     else build_info_rules ~dir ~exe ~suf:Mode.exe ~sexp_dep);
  ]

(*----------------------------------------------------------------------
 executable_rules
----------------------------------------------------------------------*)

module Executables_conf_interpret = struct

  include Executables_conf

  let disabled_warnings t =
    Compiler_selection.disabled_warnings @ t.extra_disabled_warnings

  let ocamlflags t =
    let ocaml_includes =
      List.concat_map t.extra_ocaml_includes ~f:(fun path -> ["-I"; expand_vars_root path])
    in
    let ocamlflags =
      ocaml_includes @ Top.default_ocamlflags ~disabled_warnings:(disabled_warnings t)
    in
    Ordered_set_lang.eval_with_standard t.flags ~standard:ocamlflags

  let ocamlcflags t =
    Ordered_set_lang.eval_with_standard t.ocamlc_flags ~standard:Top.ocamlcflags

  let ocamloptflags t =
    Ordered_set_lang.eval_with_standard t.ocamlopt_flags ~standard:Top.ocamloptflags

  let extend_dc t dc =
    let ocamlflags = ocamlflags t in
    let ocamlcflags = ocamlcflags t in
    let ocamloptflags = ocamloptflags t in
    let js_of_ocaml_flags = t.js_of_ocaml_flags in
    {dc with DC.
      ocamlflags;
      ocamlcflags;
      ocamloptflags;
      js_of_ocaml_flags
    }

  let libraries (t : t) = t.libraries

end

let bin_prefix =
  let wrapped = wrapped_bindirs in
  let prefixing = do_prefixing ~wrapped in
  if not prefixing
  then "bin-" (* not critical, but reduces diff from current packing rules *)
  else
    (* Must be a valid OCaml identifier prefix.
       We use "__" here as well because it's unlikely to occur in
       a legitimate library name  *)
    "bin__"

let fake_libname_of_exes names =
  match names with
  | [] -> failwith "executable declarations with no executables aren't allowed"
  | first_name :: _ -> LN.of_string (bin_prefix ^ first_name)

let objdeps ~dir name =
  BN.file_words (BN.suffixed ~dir name ".objdeps")
;;

let link_executable (module Mode : Ocaml_mode.S) (dc : DC.t) ~dir ~wrapped ~libname
      ~ocamlpacks ~link_flags ~projections_check ~allowed_ldd_dependencies name =
  let link_rules =
    link (module Mode) (dc : DC.t) ~dir
        ~link_flags
        ~ocamlpacks
        ~build_libs_DEFAULT:true
        ~allowed_ldd_dependencies
        ~ocaml_plugin_handling:(ocaml_plugin_handling dc ~dir name)
        ~suppress_build_info:false
        ~suppress_version_util:false
        ~link_libdeps_of:(return [dir, libname])
        ~compute_objs:
          (objdeps ~dir name
           *>>| fun bns ->
           List.map (bns @ [name]) ~f:(fun name ->
             dir, PN.to_string (PN.of_barename ~wrapped ~libname name)))
        ~exe:(BN.to_string name)
        ~force_link:None
  in
  let projections_check_rules =
    (* We only care about native code, since that's how we roll production executables.
       It shouldn't matter now, but if we start needing such checks on executable_conf
       items that define several executables, we could have a single rule that checks the
       dependencies of all of them at the same time. *)
    match projections_check, Mode.which with
    | None, _ | _, `Byte -> []
    | Some { Jbuild_types.Projections_check.allow; output_result_to }, `Native ->
      let exe = BN.suffixed ~dir name Mode.exe in
      let rule =
        match output_result_to with
        | Some file ->
          Fe.Projections_check.rule_for_testing
            ~target:(relative ~dir file)
            ~exe
            ~allowed_projections:allow
        | None ->
          (* This goes in a .runtest alias because jenga takes ~4s to run the [Dep.t] at
             the end of any build that runs it, which would slow down people working on
             such executable for no good reason, given that these is not a likely failure,
             and hydra will catch it anyway since it always runs .runtest. *)
          Rule.alias (Alias.runtest ~dir) [
            (Fe.Projections_check.error_msg_dep ~dir ~exe ~allowed_projections:allow
             *>>| Option.iter ~f:(fun error_msg ->
               let source = User_or_gen_config.source_file ~dir in
               failposf ~pos:(dummy_position source) "%s" error_msg ()));
          ]
      in
      [rule]
  in
  link_rules @ projections_check_rules
;;

let executables_rules dc ~dir e_conf =
  let { Executables_conf.
        projections_check;
        allowed_ldd_dependencies;
        names;
        preprocess = preprocess_spec;
        preprocessor_deps;
        link_flags;
        ocamlpacks = ocamlpacks_spec;
        ocamllex;
        ocamlyacc;
        modules = names_spec;
        review_help;
        _} = e_conf in
  let libname = fake_libname_of_exes names in
  let names = List.map names ~f:BN.of_string in
  let ocamllex_rules = List.map ocamllex ~f:(ocamllex_rule ~dir) in
  let ocamlyacc_rules = List.map ocamlyacc ~f:(ocamlyacc_rule ~dir) in
  let dc = Executables_conf_interpret.extend_dc e_conf dc in
  let wrapped = wrapped_bindirs in
  let for_executable = true in
  let can_setup_inline_runners = false in
  let names_set = BN.Set.of_list names in
  let ocamlpacks =
    Ordered_set_lang.eval_with_standard ocamlpacks_spec ~standard:ocamlpacks
  in
  let compile_rules =
    setup_ml_compile_rules
      dc ~dir ~libname ~wrapped ~for_executable ~can_setup_inline_runners
      ~preprocessor_deps ~preprocess_spec ~names_spec
      ~disallowed_module_dep:(fun x ->
        if Set.mem names_set x
        then Some (sprintf !"depending on an executable (%{BN}) doesn't make sense" x)
        else None)
  in
  let check_no_dead_code =
    let _, _, modules = eval_names_spec ~dc names_spec in
    let diff list set = List.filter list ~f:(fun elt -> not (Set.mem set elt)) in
    match diff modules names_set with
    | [] -> []
    | _ :: _ as modules_except_executables ->
      [
        (* We have to take into account both the files that are mentioned in
           implementations (obviously, ie objdeps) and mentioned only in mlis (with
           cmideps, because even though such files will not be linked in, they are
           still part of the build). *)
        Dep.both
          (Dep.List.concat_map names ~f:(objdeps ~dir))
          (Dep.List.concat_map names ~f:(cmideps ~dir))
        *>>| fun (objdeps, cmideps) ->
        let deps = BN.Set.of_list (objdeps @ cmideps) in
        match diff modules_except_executables deps with
        | [] -> ()
        | _ :: _ as dead_modules ->
          failposf ~pos:(dummy_position (User_or_gen_config.source_file ~dir))
            !"modules %s are not referenced by any executable"
            (String.concat ~sep:", " (List.map ~f:BN.to_string dead_modules))
            ()
      ]
  in
  let default_rule =
    Rule.default ~dir (
      check_no_dead_code @
      List.map names ~f:(fun name ->
        let prefixed_name = PN.of_barename ~wrapped ~libname name in
        if link_executables
        then Dep.path (BN.suffixed ~dir name ".exe")
        else Dep.path (PN.suffixed ~dir prefixed_name ".cmx")
      )
    )
  in
  let link_rules =
    List.concat_map names ~f:(fun name ->
      List.concat_map Ocaml_mode.all ~f:(fun ext ->
        link_executable ext dc ~dir ~wrapped ~libname ~link_flags ~ocamlpacks
          ~projections_check ~allowed_ldd_dependencies name))
  in
  let wrapped = wrapped_bindirs in
  let prefixing = do_prefixing ~wrapped in
  let review_help_rules =
    if review_help
    then
      List.concat_map names ~f:(fun name ->
        let name = BN.to_string name in
        [ Review_help.rule Ocaml_mode.native ~dir name
        ; Rule.default ~dir [ Dep.path (Review_help.help_filename ~dir name) ]
        ])
    else []
  in
  List.concat [
    (
      if prefixing
      then
        let _intfs, _impls, modules = eval_names_spec ~dc names_spec in
        renaming_rules ~dir ~libname ~modules
      else []
    );
    ocamllex_rules;
    ocamlyacc_rules;
    compile_rules;
    [default_rule];
    link_rules;
    review_help_rules;
  ]


(*----------------------------------------------------------------------
 embedding for ocaml plugin
----------------------------------------------------------------------*)

let time_limit = bin_relative "time_limit"

let ocaml_plugin_dir = root_relative "lib/ocaml_plugin"
let embedder = relative ~dir:ocaml_plugin_dir "bin/ocaml_embed_compiler.exe"

let builtin_cmis =
  List.map [ "pervasives.cmi"
           ; "camlinternalLazy.cmi"
           ; "camlinternalMod.cmi"
           ; "camlinternalOO.cmi"
           ; "camlinternalFormatBasics.cmi" (* added for 4.02 *)
           ; "lexing.cmi" (* pa_here (and by extension pa_fail) needs this *)
           ; "printf.cmi" (* added for 4.02 *)
           ; "digest.cmi" (* added for 4.02 *)
           ; "str.cmi" (* added for app/rules-checker *)
           ]
    ~f:(fun cmi -> ocaml_where ^/ cmi)

let embed_rules dc ~dir ~cflags conf =
  let wrapped = wrapped_bindirs in
  let {DC. xlibnames; libmap; _} = dc in
  let libname =
    match xlibnames with
    | [libname] -> libname
    | _ -> failwith "embed config requires exactly one executables or library config"
  in
  let {Embed_conf. names; libraries; cmis; pps=pp_names; code_style} = conf in
  let ppx_exe, camlp4_pps =
    let pps, flags = expand_pps pp_names in
    assert (List.is_empty flags);
    ppx_executable pps, camlp4_pps pps
  in
  let libraries = List.map libraries ~f:LN.of_string in
  List.concat_map names ~f:(fun prog ->
    let plugin_name = prog ^ ".ocaml_plugin" in
    let camlp4_spec, camlp4_dep_paths =
      let camlp4_pps = List.map camlp4_pps ~f:(fun pp ->
        LL.liblink_refname ~lib:(PP.to_libname pp) ~name:(PP.to_string pp ^ ".cmxs")
      ) in
      match camlp4_pps with
      | [] -> [], []
      | _::_ ->
        List.concat
          [ [ "-pp" ; reach_from ~dir camlp4o_path ]
          ; List.concat_map camlp4_pps ~f:(fun pp -> [ "-pa-cmxs"; reach_from ~dir pp ])],
        camlp4o_path :: camlp4_pps
    in
    let preprocessing_spec, preprocessing_dep_paths =
      let ppx_spec = [ "-ppx"; reach_from ~dir ppx_exe ] in
      let ppx_dep_paths = [ppx_exe] in
      match code_style with
      | No_preprocessing -> [], []
      | Camlp4 -> camlp4_spec, camlp4_dep_paths
      | Ppx -> ppx_spec, ppx_dep_paths
      | Bilingual -> camlp4_spec @ ppx_spec, camlp4_dep_paths @ ppx_dep_paths
    in
    let libraries = libraries @ [LN.of_string "ocaml_plugin"] in
    let gen_plugin_c =
      Rule.create
        ~targets:[relative ~dir (plugin_name ^ ".c")] (
        Dep.List.concat_map libraries ~f:(fun lib ->
          LL.liblink_interfaces ~lib *>>| fun deps -> lib :: deps
        ) *>>= fun libraries ->
        let libraries = LN.remove_dups_and_sort libraries in
        let local_cmis =
          List.map cmis ~f:(fun name ->
            let prefixed_name = PN.of_barename ~wrapped ~libname (BN.of_string name) in
            PN.suffixed ~dir prefixed_name ".cmi"
          )
        in
        Dep.List.concat_map libraries ~f:(fun lib ->
          LL.liblink_submodule_cmi_paths ~lib libmap
        ) *>>= fun library_cmis ->
        let cmis = local_cmis @ library_cmis in
        let dep_paths = cmis @ preprocessing_dep_paths @ [
          (*ocamlopt_path*)
          (*builtin_cmis*)
          time_limit;
          embedder;
        ]
        in
        Dep.all_unit (List.map ~f:Dep.path dep_paths) *>>| fun () ->
        (* Be careful here: there is a limit of MAX_ARG_STRLEN (130kB)  on any argument
           (note that this is distinct from ARG_MAX, which is bigger),
           so we can't use [bash -c], as the script could go over the limit,
           given how many cmis we can potentially have. *)
        Action.process ~dir
          (reach_from ~dir time_limit)
          (List.concat
            [ [ "300"
              ; reach_from ~dir embedder
              ; "-wrap-symbol"
              ]
            ; preprocessing_spec
            ; [ "-cc"
              ; ocamlopt_path
              ; "-ocamldep"
              ; ocamldep_path
              ]
            ; builtin_cmis
            ; List.map ~f:(reach_from ~dir) cmis
            ; [ "-o"; plugin_name ^ ".c" ]
            ])
      )
    in
    let cflags = List.filter cflags ~f:(fun flag -> flag <> "-pedantic") in
    let include_search_path = [] in
    List.concat [
      [gen_plugin_c];
      compile_c ~dir ~cflags ~include_search_path plugin_name;
    ]
  )

(*----------------------------------------------------------------------
 jane-script specific
----------------------------------------------------------------------*)

let jane_script_rules dc
      { Jane_script_conf. only_when_NODYNLINK_is; libraries = public_libraries; pps } =
  if only_when_NODYNLINK_is <> nodynlink then [] else begin
    let {DC. dir; _ } = dc in
    let public_libraries = List.map public_libraries ~f:LN.of_string in
    let pps, flags = expand_pps pps in
    assert (List.is_empty flags);
    let ppx_path = ppx_executable pps in
    let all_libraries =
      LN.remove_dups_preserve_order
        (List.concat_map ~f:libs_for_code_generated_by_pp pps @ public_libraries)
      |> libs_transitive_closure
    in
    let dep_for_libraries =
      all_libraries *>>= fun all_libraries ->
      (* Don't care about the cmx files. *)
      Dep.all_unit (LL.liblink_deps ~libs:all_libraries ~suffixes:[".cmi"; ".cmxs"])
    in
    let cfg_target = Path.relative ~dir "jane-script.cfg" in
    let cfg_rule =
      Rule.create ~targets:[cfg_target] (
        all_libraries *>>| fun all_libraries ->
        let sexp =
          let module Cfg = struct
            (* comments for the exact meaning of the fields are in
               app/jane-script/lib/static_data.mli *)
            type t = {
              compiler_dir_path : string;
              ppx_path          : string;
              stdlib_dir_path   : string;
              libs_dir_path     : string;
              public_libraries     : LN.t list;
              all_libraries        : LN.t list;
            } [@@deriving sexp_of]
          end in
          Cfg.sexp_of_t {
            compiler_dir_path = ocaml_bin;
            ppx_path = Path.reach_from ~dir ppx_path;
            stdlib_dir_path = ocaml_where;
            libs_dir_path = Path.reach_from ~dir liblinks_dir;
            public_libraries;
            all_libraries;
          }
        in
        Action.save (Sexp.to_string_hum sexp) ~target:cfg_target
      )
    in
    let jane_script_alias = Alias.create ~dir "jane-script" in
    [ Rule.default ~dir [ Dep.alias jane_script_alias ]
    ; Rule.alias jane_script_alias
        [ Dep.path cfg_target;
          dep_for_libraries;
          Dep.path ppx_path;
        ]
    ; cfg_rule
    ;
    ]
  end

(*----------------------------------------------------------------------
 inline_tests & benchmarks
----------------------------------------------------------------------*)

let inline_tests_script_rule ~dir ~libname =
  let run =
    if drop_test
    then "echo >&2 'Tests have been disabled'; exit 1"
    else sprintf !"exec ./inline_tests_runner.exe inline-test-runner %{quote} \"$@\""
           (LN.to_string libname)
  in
  write_string_rule ~chmod_x:() ~target:(relative ~dir "inline_tests_runner") (
"#!/bin/sh
# This file was generated by jenga
cd \"$(dirname \"$(readlink -f \"$0\")\")\"
" ^ run)
;;

let inline_bench_script_rule ~dir ~libname =
  let run =
    if drop_bench
    then "echo >&2 'Benches have been disabled'; exit 1"
    else sprintf
           !"BENCH_LIB=%{quote} exec ./inline_benchmarks_runner.exe -benchmarks-runner \"$@\""
           (LN.to_string libname)
  in
  write_string_rule ~chmod_x:() ~target:(relative ~dir "inline_benchmarks_runner") (
"#!/bin/sh
# This file was generated by jenga
cd \"$(dirname \"$(readlink -f \"$0\")\")\"
" ^ run)
;;

let run_inline_action ~dir ~user_deps filename =
  let f = relative ~dir in
  let sources = List.map ~f ([filename; filename ^ ".exe"] @ user_deps) in
  Dep.action
    (Dep.all_unit (List.map sources ~f:Dep.path)
     *>>| fun () ->
     let args =
       [ "300"
       ; "./" ^ filename
       ] @ if inline_test_color then [] else ["-no-color"]
     in
     Action.process ~dir (reach_from ~dir time_limit) args)

let non_empty_file ~dir ~filename =
  let path = relative ~dir filename in
  Dep.contents path *>>| fun s ->
  not (String.is_empty (String.strip s))

let has_inline_tests ~dir =
  non_empty_file ~dir ~filename:fgrep_inline_test_filename

let has_expect_tests ~dir =
  non_empty_file ~dir ~filename:fgrep_expect_test_filename

let has_tests ~dir =
  has_inline_tests ~dir
  *>>= function
  | true -> return true
  | false -> has_expect_tests ~dir

let has_benchmarks dc ~dir =
  let libmap = dc.DC.libmap in
  if Libmap.exists libmap (LN.of_string "inline_benchmarks")
  then non_empty_file ~dir ~filename:fgrep_bench_filename
  else return false

let inline_test_runner_dir, inline_test_runner, final_test_object =
  Config.inline_test_runner_dir, "summarize", "runner"
let expect_runner_dir, expect_runner =
  Config.expect_runner_dir, "evaluator"
let inline_tests_rules dc ~dir ~libname ~(user_config : Inline_tests.t) =
  let exe = "inline_tests_runner" in
  let if_expect_tests value =
    has_expect_tests ~dir
    *>>| function
    | true -> value
    | false -> []
  in
  List.concat [
    link Ocaml_mode.native dc ~dir
      ~suppress_build_info:true
      ~suppress_version_util:true
      ~build_libs_DEFAULT:false
      ~link_flags:[]
      ~ocamlpacks
      ~allowed_ldd_dependencies:None
      ~ocaml_plugin_handling:`Not_a_plugin
      ~link_libdeps_of:
        (Dep.List.concat [
           return [(dir, libname)];
           if_expect_tests [expect_runner_dir, fake_libname_of_exes [expect_runner]];
           return [inline_test_runner_dir,
                   fake_libname_of_exes [final_test_object; inline_test_runner]];
         ])
      ~force_link:(Some libname)
      ~compute_objs:
        (Dep.List.concat [
           return [inline_test_runner_dir, inline_test_runner];
           if_expect_tests [expect_runner_dir, expect_runner];
           return [inline_test_runner_dir, final_test_object];
         ])
      ~exe;
    [ inline_tests_script_rule ~dir ~libname;
      Rule.alias (alias_for_inline_runners ~dir) [
        has_tests ~dir *>>= function
        | false -> return ()
        | true ->
          let names =
            if link_executables
            then [exe; exe ^ ".exe"]
            else []
          in
          Dep.all_unit (List.map names ~f:(fun name -> Dep.path (relative ~dir name)))
      ];
    ];
    if user_config.in_runtest
    then
      [ Rule.alias (Alias.runtest ~dir) [
          has_tests ~dir *>>= function
          | false -> return ()
          | true -> run_inline_action ~dir ~user_deps:user_config.deps exe
      ]]
    else
      []
  ]

let bench_runner_dir = root_relative "lib/core_bench/runner"
let inline_bench_rules dc ~dir ~libname =
  let exe = "inline_benchmarks_runner" in
  List.concat [
    link Ocaml_mode.native dc ~dir
      ~suppress_build_info:false
      ~suppress_version_util:true
      ~build_libs_DEFAULT:false
      ~link_flags:[]
      ~ocamlpacks
      ~allowed_ldd_dependencies:None
      ~ocaml_plugin_handling:`Not_a_plugin
      ~link_libdeps_of:
        (return
           [ dir, libname
           ; bench_runner_dir, fake_libname_of_exes ["runner"]
           ])
      ~force_link:(Some libname)
      ~compute_objs:(return [(bench_runner_dir, "runner")])
      ~exe;
    [ inline_bench_script_rule ~dir ~libname;
      Rule.alias (alias_for_inline_runners ~dir) [
        has_benchmarks dc ~dir *>>= function
        | false -> return ()
        | true ->
          let names =
            if link_executables
            then [exe; exe ^ ".exe"]
            else []
          in
          Dep.all_unit (List.map names ~f:(fun name -> Dep.path (relative ~dir name)))
      ];
      Rule.alias (Alias.runbench ~dir) [
        has_benchmarks dc ~dir *>>= function
        | false -> return ()
        | true -> run_inline_action ~user_deps:[] ~dir exe
      ];
    ]
  ]

(*----------------------------------------------------------------------
 generate_dep_rules
----------------------------------------------------------------------*)

let ocaml_libraries j =
  begin match j with
  | `preprocessor x -> Preprocessor_conf.libraries x
  | `library x -> Library_conf.libraries x
  | `libraryX _ -> []
  | `executables x -> Executables_conf_interpret.libraries x
  | `embed _ -> []
  | `jane_script _ -> []
  | `compile_c _ -> []
  | `rule _ -> []
  | `alias _ -> []
  | `no_utop -> []
  | `switched_to_ppx_style -> []
  | `translate_from_camlp4 -> []
  | `requires_camlp4 -> []
  | `unified_tests _ -> []
  | `public_repo _ -> []
  end |> List.map ~f:LN.of_string

let xlibnames j =
  begin match j with
  | `preprocessor x -> [Preprocessor_conf.name x]
  | `library x -> [Library_conf.name x]
  | `libraryX x -> [LibraryX_conf.name x]
  | `executables { Executables_conf_interpret.names; _ } ->
    [LN.to_string (fake_libname_of_exes names)]
  | `embed _ -> []
  | `jane_script _ -> []
  | `compile_c _ -> []
  | `rule _ -> []
  | `alias _ -> []
  | `no_utop -> []
  | `switched_to_ppx_style -> []
  | `translate_from_camlp4 -> []
  | `requires_camlp4 -> []
  | `unified_tests _ -> []
  | `public_repo _ -> []
  end |> List.map ~f:LN.of_string

let extra_disabled_warnings j = match j with
  | `preprocessor x -> x.Preprocessor_conf.extra_disabled_warnings
  | `library x -> x.Library_conf.extra_disabled_warnings
  | `libraryX _ -> []
  | `executables x -> x.Executables_conf_interpret.extra_disabled_warnings
  | `embed _ -> []
  | `jane_script _ -> []
  | `compile_c _ -> []
  | `rule _ -> []
  | `alias _ -> []
  | `no_utop -> []
  | `switched_to_ppx_style -> []
  | `translate_from_camlp4 -> []
  | `requires_camlp4 -> []
  | `unified_tests _ -> []
  | `public_repo _ -> []

let pps_of_jbuild dc jbuild_item =
  let of_specs (xs : Preprocess_spec.t list) =
    List.concat_map xs ~f:(fun (kind,__names_spec) ->
      eval_preprocess_style_libs (PP_style.of_kind dc kind))
  in
  match jbuild_item with
  | `preprocessor x -> of_specs x.Preprocessor_conf.preprocess
  | `library x -> of_specs x.Library_conf.preprocess
  | `executables x -> of_specs x.Executables_conf.preprocess
  | `libraryX _
  | `embed _
  | `jane_script _
  | `compile_c _
  | `rule _
  | `alias _
  | `no_utop
  | `switched_to_ppx_style
  | `translate_from_camlp4
  | `requires_camlp4
  | `unified_tests _
  | `public_repo _
    -> []

let generate_dep_rules dc ~dir jbuilds =
  List.concat_map jbuilds ~f:(fun jbuild ->
    List.concat_map (xlibnames jbuild) ~f:(fun libname ->
      let pp_libs = pps_of_jbuild dc jbuild in
      let user_libraries = ocaml_libraries jbuild in
      let libs = pp_libs @ user_libraries in
      gen_libdeps ~dir ~libs libname
    )
  )

(*----------------------------------------------------------------------
 merlin rules
----------------------------------------------------------------------*)

let merlin_1step_libs dc ~dir =
  Dep.List.concat_map dc.DC.xlibnames ~f:(fun libname ->
    LN.file_words (LN.suffixed ~dir libname ".libdeps")
  ) *>>= fun libs ->
  let libs = LN.remove_dups_preserve_order libs in
  return libs

let merlin_rules dc ~dir (jbuilds : Jbuild_types.Jbuild.t list) =
  let target = relative ~dir ".merlin" in
  (* don't create .merlin files in all the directories when we don't need them *)
  if dc.DC.xlibnames = [] && dir <> Path.the_root then [] else [
    Rule.create ~targets:[target] (
      let preprocessor_directives =
        if
          List.exists jbuilds ~f:(function
            | `requires_camlp4 -> true
            | _ -> false
          )
        then [
          "EXT here";
          "EXT ounit";
          "EXT nonrec";
          "EXT custom_printf";
        ] else begin
          let approximate_pps, can_setup_inline_runners, flags =
            let approximate_pps = ref None in
            let can_setup_inline_runners = ref false in
            let use_default () =
              approximate_pps := Some pa_jane;
              can_setup_inline_runners := true;
            in
            let preprocess_spec ~can_setup_inline_runners:this_can_setup_inline_runners =
              can_setup_inline_runners := this_can_setup_inline_runners || !can_setup_inline_runners;
              function
              | [(`pps pps, name_spec)] when name_spec = Ordered_set_lang.standard ->
                let pps, flags = expand_pps pps in
                if List.is_empty flags then
                  (match !approximate_pps with
                  | None -> approximate_pps := Some pps
                  | Some old -> if old <> pps then use_default ())
                else use_default ()
              | _ -> use_default ()
            in
            List.iter jbuilds ~f:(fun jbuild_item ->
              match jbuild_item with
              | `library l -> preprocess_spec l.preprocess ~can_setup_inline_runners:true
              | `executables e -> preprocess_spec e.preprocess ~can_setup_inline_runners:false
              | `preprocessor p -> preprocess_spec p.preprocess ~can_setup_inline_runners:false
              | _ -> ());
            if Option.is_none !approximate_pps then use_default ();
            Option.value_exn !approximate_pps, !can_setup_inline_runners, []
          in
          let _relative_exe, args =
            get_ppx_command ~kind:None ~dir ~can_setup_inline_runners ~flags
              ~libname:(LN.of_string "fake_for_merlin") approximate_pps
          in
          let exe = Path.to_absolute_string (ppx_executable approximate_pps) in
          [ sprintf "FLG -ppx '%s -as-ppx %s'" exe (String.concat args ~sep:" ")
          ; "EXT nonrec" ]
        end
      in
      merlin_1step_libs dc ~dir *>>= fun libs ->
      let libmap = dc.DC.libmap in
      let find_library = Libmap.look_exn libmap in
      let dot_merlin_contents =
        String.concat ~sep:"\n" (
          ("FLG " ^ (String.concat ~sep:" " dc.DC.merlinflags))
          :: preprocessor_directives @ (
            (* When we use -open on the command line, we need to give it to merlin as
               well, otherwise it won't be able to type. In theory this should also be
               done if people add -open themselves to the list of flags in their jbuild,
               but in practice that doesn't happen. *)
            List.concat_map jbuilds ~f:(fun jbuild ->
              let wrapped = match jbuild with
                | `library conf -> Library_conf.wrapped conf
                | `executables _ -> wrapped_bindirs
                | _ -> false
              in
              if do_prefixing ~wrapped
              then List.map (xlibnames jbuild) ~f:(sprintf !"FLG -open %{LN.to_module}")
              else []
            )
          ) @
          (* We tell merlin to load cmi from the liblinks dir because that's what the
             compiler sees. But we tell merlin to load cmt from the source dir since they
             aren't installed, and merlin has some heuristic to try to figure out the
             right cmt to load even when they clash (when PACKING=true). *)
          List.concat_map libs ~f:(fun lib ->
            let src = reach_from ~dir (find_library lib) in
            let cmi = reach_from ~dir (LL.liblink_dir ~lib) in
            [
              sprintf "S %s" src;
              sprintf "CMT %s" src;
              sprintf "CMI %s" cmi;
            ]
          )
        )
      in
      let dependencies_dot_merlins =
        List.map libs ~f:(fun lib ->
          Dep.path (Path.relative ~dir:(find_library lib) ".merlin")
        )
      in
      Dep.all_unit dependencies_dot_merlins *>>| fun () ->
      write_string_action dot_merlin_contents ~target
    );
    Rule.alias (Alias.lib_artifacts ~dir) [Dep.path target];
    alias_dot_filename_hack ~dir ".merlin";
  ]

(*----------------------------------------------------------------------
 library_rules
----------------------------------------------------------------------*)

module Library_conf_interpret = struct

  include Library_conf

  let disabled_warnings t =
    Compiler_selection.disabled_warnings @ t.extra_disabled_warnings

  let ocaml_includes t =
    List.concat_map t.ocaml_includes ~f:(fun path -> ["-I"; expand_vars_root path])

  let o_names t = List.map t.o_names ~f:expand_vars_root

  let ocamlflags t =
    let ocamlflags =
      ocaml_includes t @ Top.default_ocamlflags ~disabled_warnings:(disabled_warnings t)
    in
    Ordered_set_lang.eval_with_standard t.flags ~standard:ocamlflags

  let ocamlcflags t =
    Ordered_set_lang.eval_with_standard t.ocamlc_flags ~standard:Top.ocamlcflags

  let ocamloptflags t =
    Ordered_set_lang.eval_with_standard t.ocamlopt_flags ~standard:Top.ocamloptflags

  let extend_dc t dc =
    let ocamlflags = ocamlflags t in
    let ocamlcflags = ocamlcflags t in
    let ocamloptflags = ocamloptflags t in
    {dc with DC.
      ocamlflags;
      ocamlcflags;
      ocamloptflags
    }

  let cflags t =
    match t.replace_cflags with _::_ as xs -> xs | [] ->
      let avoid_set = String.Hash_set.of_list t.avoid_cflags in
      List.filter (default_cflags @ t.extra_cflags)
        ~f:(fun flag -> not (Hash_set.mem avoid_set flag))

  let cxxflags t =
    let avoid_set = String.Hash_set.of_list t.avoid_cxxflags in
    List.filter (default_cxxflags @ t.extra_cxxflags)
      ~f:(fun flag -> not (Hash_set.mem avoid_set flag))

end

let library_rules dc ~dir ~cflags library_conf =
  let dc = Library_conf_interpret.extend_dc library_conf dc in
  let libname = LN.of_string (Library_conf.name library_conf) in
  let wrapped = Library_conf.wrapped library_conf in
  let prefixing = do_prefixing ~wrapped in
  assert (not wrapped ==> not prefixing);
  let preprocessor_deps = Library_conf.preprocessor_deps library_conf in
  let ocamllex_rules =
    List.map (Library_conf.ocamllex library_conf) ~f:(ocamllex_rule ~dir)
  in
  let ocamlyacc_rules =
    List.map (Library_conf.ocamlyacc library_conf) ~f:(ocamlyacc_rule ~dir)
  in
  let copy_config_from_core_rules =
    if Library_conf.copy_config_from_core library_conf
    then copy_config_from_core ~dir
    else []
  in
  let preprocess_spec = Library_conf.preprocess library_conf in
  let names_spec = Library_conf.modules library_conf in
  let () =
    let {Library_conf. wrapped; _ } = library_conf in
    if not wrapped then (
      let __intfs, impls, __modules = eval_names_spec ~dc names_spec in
      let ok =
        match impls with
        | [impl1] -> BN.is_lib impl1 ~libname
        | _ -> false
      in
      if not ok then (
        failwithf "Problem with unwrapped library `%s', in %s. Fails check: An unwrapped \
                   library must contain exactly one module, with the same name as the library"
          (LN.to_string libname) (Path.to_string dir) ()
      )
    )
  in
  let for_executable = false in
  let can_setup_inline_runners = true in
  let compile_rules =
    setup_ml_compile_rules
      dc ~dir ~libname ~wrapped ~for_executable ~can_setup_inline_runners
      ~preprocessor_deps ~preprocess_spec ~names_spec
  in
  let c_names = Library_conf.c_names library_conf in
  let o_names = Library_conf_interpret.o_names library_conf in
  let cxx_names = Library_conf_interpret.cxx_names library_conf in
  let cxxflags = Library_conf_interpret.cxxflags library_conf in
  let include_search_path = Library_conf_interpret.includes library_conf in
  let include_search_path = List.map include_search_path ~f:(expand_vars_root ~dir) in
  let cflags = List.map cflags ~f:(expand_vars_root ~dir) in
  let cxxflags = List.map cxxflags ~f:(expand_vars_root ~dir) in
  let cxx_suf =
    match Library_conf_interpret.cxx_suf library_conf with
    | None -> ".cpp"
    | Some suf -> "." ^ suf
  in
  let compile_c_rules =
    List.concat_map c_names ~f:(compile_c ~dir ~cflags ~include_search_path)
  in
  let compile_cxx_rules =
    List.concat_map cxx_names ~f:(compile_cxx ~dir ~cxxflags ~include_search_path ~cxx_suf)
  in
  let o_names = c_names @ cxx_names @
                o_names (*objects to link but not compile*)
  in
  let intfs, impls, modules = eval_names_spec ~dc names_spec in
  let stub_names, stub_rules =
    match Library_conf_interpret.self_build_stubs_archive library_conf with
    | Some name -> [name], []
    | None ->
      match o_names with
      | [] -> [], []
      | _::_ ->
        let target_a = stubs_archive_file (LN.to_string libname) in
        [LN.to_string libname], [static_archive_c ~dir ~o_names ~target:target_a]
  in
  let ocamllibflags =
    List.concat [
      link_time_args_for_c_compiler (
        List.map stub_names ~f:(sprintf "-l%s_stubs"));
      List.map (Library_conf_interpret.library_flags library_conf) ~f:expand_vars_root;
      link_time_args_for_c_compiler (
        List.map (Library_conf_interpret.cclibs library_conf) ~f:(sprintf "-l%s"));
      dc.DC.ocamllibflags;
    ]
  in
  let shared_rule =
    let dynlink_dep () =
      if List.mem dc.ocamloptflags "-nodynlink" then
        failwithf
          !"Trying to build a .cmxs for library %{LN} but '-nodynlink' is set."
          libname ();
      Async.Std.Deferred.unit
    in
    let deps_paths =
      let suffixes = [".cmxa"; ".a"] in
      List.map suffixes ~f:(LN.suffixed ~dir libname)
      @ List.map stub_names ~f:(fun name -> relative ~dir (stubs_archive_file name))
    in
    let deps = Dep.deferred dynlink_dep :: List.map deps_paths ~f:Dep.path in
    let target = LN.to_string libname ^ ".cmxs" in
    let action =
      (* -linkall is needed since we build the .cmxs from a .cmxa, otherwise the .cmxs
         will just be empty. The other possibility would be to create cmxs for each
         module, which would keep the benefit of not packing libraries, including less
         crap to load and run the toplevel of.

         The stubs need to be linked in statically into the cmxs as usual, but although
         the compiler does pass them to the C compiler, they can get dropped, presumably
         because the linker doesn't see any use of some symbols in the current library. We
         pass -whole-archive to ld to turn off this dead code elimination. *)
      Action.process ~dir
        ocamlopt_path
        (List.concat
           [ [ "-linkall"
             ; "-I"; "."
             ; "-ccopt"; quote "-Wl,-whole-archive"
             ]
           ; ccopts (List.map stub_names ~f:(sprintf "-l%s_stubs"))
           ; [ "-ccopt"; quote "-Wl,-no-whole-archive"
             ; LN.to_string libname ^ ".cmxa"
             ; "-o"; target
             ; "-shared"
             ] ])
    in
    simple_rule ~targets:[ Path.relative ~dir target ] ~deps ~action
  in
  let dc = {dc with DC. ocamllibflags } in
  let modules_file_rule =
    let has_bin_annot =
      List.mem dc.ocamlflags "-bin-annot"
      || List.mem dc.ocamloptflags "-bin-annot"
    in
    Lib_modules.rule ~dir ~libname
      { impls_and_intfs = modules ; impls; intfs;
        bin_annot = has_bin_annot; }
  in
  let pack_maybe_archive_rules =
    if not wrapped
    then ocaml_library_archive dc ~wrapped ~dir ~libname ~impls:[]
    else
    if select_lib_packing
    then
      ocaml_library_pack dc ~dir ~impls ~libname ~exists_libname_cmi:false
      @ ocaml_library_archive dc ~wrapped ~dir ~libname ~impls:[]
    else
      renaming_rules ~dir ~libname ~modules
      @ ocaml_library_archive dc ~wrapped ~dir ~libname ~impls
  in
  let default_targets =
    (if Library_conf_interpret.skip_from_default library_conf then []
     else
       let suffixes = [".cmi";".cmxa";".a"] in
       List.map suffixes ~f:(fun suf -> LN.suffixed ~dir libname suf)
    ) @
    List.map stub_names ~f:(fun name -> relative ~dir (stubs_archive_file name))
    @
    (if with_utop && not dc.no_utop_alias then
       [relative ~dir "utop"]
     else
       [])
  in
  let default_rules =
    [Rule.alias (Alias.lib_artifacts ~dir) (List.map default_targets ~f:Dep.path)]
  in
  List.concat [
    [gen_interface_deps_from_objinfo dc ~dir ~wrapped ~libname];
    Lib_clients.rules ~dir ~libname;
    [library_module_order ~dir ~impls ~intfs ~libname];
    [modules_file_rule];
    pack_maybe_archive_rules;
    compile_c_rules;
    compile_cxx_rules;
    copy_config_from_core_rules;
    ocamllex_rules;
    ocamlyacc_rules;
    [stub_names_rule ~dir ~libname ~stub_names];
    stub_rules;
    [shared_rule];
    compile_rules;
    default_rules;
    utop_rules dc ~dir ~libname;
    [fgrep_rule ~dir ~filename:fgrep_inline_test_filename ~macros:inline_test_macros ~impls];
    [fgrep_rule ~dir ~filename:fgrep_expect_test_filename ~macros:expect_test_macros ~impls];
    [fgrep_rule ~dir ~filename:fgrep_bench_filename ~macros:bench_macros ~impls];
    inline_tests_rules dc ~dir ~libname
      ~user_config:(Library_conf_interpret.inline_tests library_conf);
    inline_bench_rules dc ~dir ~libname;
  ]

(*----------------------------------------------------------------------
public_repo_rules
----------------------------------------------------------------------*)

module Public_release = Public_release.Make(struct
    module LN = LN
    module BN = BN
    module DC = DC
    module Libmap = Libmap
    module Lib_modules = Lib_modules
    module Standard_pp_sets = Standard_pp_sets
    module PP = PP
    module User_or_gen_config = User_or_gen_config
    let public_release_files = public_release_files_path ~repo:Path.the_root
    let deep_unignored_subdirs = deep_unignored_subdirs
    let libs_for_code_generated_by_pp = libs_for_code_generated_by_pp
    let ocaml_libraries = ocaml_libraries
    let pps_of_jbuild = pps_of_jbuild
    let expand_pps = expand_pps
    let remap_pa_names = remap_pa_names
  end)

(*----------------------------------------------------------------------
gen_build_rules
----------------------------------------------------------------------*)

let gen_build_rules dc ~dir jbuilds =
  let cflags =
    match (
      List.concat_map jbuilds ~f:(fun j ->
        match j with `library x -> [x] | _ -> []
      )
    ) with
    | [] -> default_cflags
    | conf::_ -> Library_conf_interpret.cflags conf
  in
  let ocamlflags =
    match (
      List.concat_map jbuilds ~f:(fun j ->
        match j with `library x -> [x] | _ -> []
      )
    ) with
    | [] -> dc.DC.ocamlflags
    | conf::_ -> Library_conf_interpret.ocamlflags conf
  in
  List.concat_map jbuilds ~f:(fun (j : Jbuild_types.Jbuild.t) ->
    match j with
    | `preprocessor conf -> preprocessor_rules dc ~dir conf
    | `library conf -> library_rules dc ~dir ~cflags conf
    | `libraryX conf -> libraryX_rules dc ~dir ~cflags conf
    | `executables conf -> executables_rules dc ~dir conf
    | `embed conf -> embed_rules dc ~dir ~cflags conf
    | `jane_script conf -> jane_script_rules dc conf
    | `compile_c conf -> user_configured_compile_c_rules ~dir conf
    | `rule conf ->
      let dc_for_rule_conf = {dc with DC. ocamlflags} in
      [rule_conf_to_rule dc_for_rule_conf ~dir ~cflags conf]
    | `alias conf ->
      let dc_for_rule_conf = {dc with DC. ocamlflags} in
      [alias_conf_to_rule dc_for_rule_conf ~dir ~cflags conf]
    | `no_utop -> []
    | `switched_to_ppx_style -> []
    | `translate_from_camlp4 -> []
    | `requires_camlp4 -> []
    | `unified_tests conf ->
      Js_unified_tests.rules ~dir
        { target = conf.target
        ; setup_script = Option.map conf.setup_script ~f:(expand_vars_root ~dir)
        ; deps = Dep_conf_interpret.list_to_depends ~dir conf.deps
        }
    | `public_repo conf -> Public_release.rules dc ~dir conf
  )

(*----------------------------------------------------------------------
 directory_rules
----------------------------------------------------------------------*)

let directory_rules dc ~dir jbuilds =
  List.concat [
    [
      Rule.alias (Alias.lib_artifacts ~dir) [];
      Rule.default ~dir [Dep.alias (Alias.lib_artifacts ~dir)];
    ];
    gen_build_rules dc ~dir jbuilds;
    generate_dep_rules dc ~dir jbuilds;
    merlin_rules dc ~dir jbuilds;
    Info_files.write ~dir;
    if Path.equal dir Public_release.repos then
      Public_release.Package_map.global_rules ~dir
    else
      []
  ]

(*----------------------------------------------------------------------
 libmap.sexp
----------------------------------------------------------------------*)

module Libmap_sexp : sig

  val rule : Rule.t
  val get : Libmap.t Dep.t

end = struct

  type t = (LN.t * Path.t) list
  [@@deriving sexp]

  let libmap_sexp_path = root_relative "libmap.sexp"

  let rule =
    Rule.create ~targets:[libmap_sexp_path] (
      deep_unignored_subdirs ~dir:Path.the_root *>>= fun dirs ->
      Dep.all (
        List.map dirs ~f:(fun dir ->
          User_or_gen_config.libnames ~dir *>>| fun libnames ->
          List.map libnames ~f:(fun name -> (name,dir))
        )
      ) *>>| fun xs ->
      let libmap = List.concat xs in
      let sexp = sexp_of_t libmap in
      write_string_action (Sexp.to_string_hum sexp) ~target:libmap_sexp_path
    )

  let load path =
    read_then_convert_string_via_reader
      ~path
      ~contents:Dep.contents_cutoff
      ~do_read:(fun reader ->
        let open Async.Std in
        Reader.read_sexp reader >>| function
        | `Ok sexp -> t_of_sexp sexp
        | `Eof -> failwith "Eof"
      )

  let get =
    load libmap_sexp_path *>>| Libmap.create_exn

end

let setup_liblinks_dir ~dir =
  Scheme.rules_dep (
    Libmap_sexp.get *>>| fun libmap ->
    [LL.api_rule ~dir libmap]
  )

let top_api_rule =
  Rule.alias (Alias.api ~dir:Path.the_root)
    [Dep.alias (Alias.api ~dir:liblinks_dir)]

let setup_liblinks ~dir =
  Scheme.rules_dep (
    Libmap_sexp.get *>>= fun libmap ->
    LL.rules ~dir libmap
  )

let setup_ppx_cache ~dir =
  Scheme.rules_dep (
    Libmap_sexp.get *>>| fun libmap ->
    generate_ppx_exe_rules libmap ~dir
  )

(*----------------------------------------------------------------------
  autogen: determine from rule targets (& ocamllex/yacc)
----------------------------------------------------------------------*)

let filter_drop ~suffix xs =
  List.filter_map xs ~f:(fun x ->
    String.chop_suffix x ~suffix
  )

(* duplicates knowledge here of what .ml/.mli are autogen via ocaml lex/yacc*)

let infer_autogen jbuilds =
  List.concat_map jbuilds ~f:(fun j ->
    match j with
    | `rule {Rule_conf. targets; _} ->
      targets

    | `library x ->
      List.concat_cartesian_product (Library_conf_interpret.ocamllex x) [".ml"]
      @ List.concat_cartesian_product (Library_conf_interpret.ocamlyacc x) [".ml"; ".mli"]
      @ (if Library_conf_interpret.copy_config_from_core x then ["config.h"; "config.mlh"] else [])

    | `executables x ->
      List.concat_cartesian_product x.Executables_conf_interpret.ocamllex [".ml"]
      @ List.concat_cartesian_product x.Executables_conf_interpret.ocamlyacc [".ml"; ".mli"]

    | `libraryX _ (* I suppose it could produce ml files, but well *)
    | `preprocessor _
    | `embed _
    | `jane_script _
    | `compile_c _
    | `alias _
    | `no_utop
    | `switched_to_ppx_style
    | `translate_from_camlp4
    | `requires_camlp4
    | `unified_tests _
    | `public_repo _
      -> []
  )

(*----------------------------------------------------------------------
 create_directory_context, setup_main
----------------------------------------------------------------------*)

let create_directory_context ~dir jbuilds =
  (* These dependencies could/should be run in parallel *)
  Centos.ocamllibflags *>>= fun ocamllibflags ->
  Dep.glob_listing (glob_ml ~dir) *>>= fun ml_paths ->
  Dep.glob_listing (glob_mli ~dir) *>>= fun mli_paths ->
  Libmap_sexp.get *>>= fun libmap ->
  List.iter jbuilds ~f:(fun jbuild ->
    List.iter (ocaml_libraries jbuild) ~f:(fun lib ->
      if not (Libmap.exists libmap lib) then begin
        let pos = dummy_position (User_or_gen_config.source_file ~dir) in
        failposf ~pos !"unknown library %{LN}" lib ()
      end));
  let merlinflags =
    let extra_disabled_warnings = List.concat_map jbuilds ~f:extra_disabled_warnings in
    let disabled_warnings = Compiler_selection.disabled_warnings @ extra_disabled_warnings in
    Top.default_merlinflags ~disabled_warnings
  in
  let ocamlflags =
    Top.default_ocamlflags ~disabled_warnings:Compiler_selection.disabled_warnings
  in
  let xlibnames = List.concat_map jbuilds ~f:xlibnames in
  let ocaml_plugin_libraries name =
    List.find_map jbuilds ~f:(fun j -> match j with
    | `embed { Embed_conf.names; libraries; _ } ->
      if List.mem names name then Some libraries else None
    | _ -> None
    )
  in
  let no_utop_alias =
    Path.is_descendant ~dir:ppx_dir dir ||
    List.exists jbuilds ~f:(fun j -> match j with | `no_utop -> true | _ -> false)
  in
  let autogen_raw = infer_autogen jbuilds in
  let generated_modules =
    List.filter autogen_raw ~f:(fun base ->
      String.is_suffix base ~suffix:".ml" || String.is_suffix base ~suffix:".mli")
  in
  let impls =
    (* Sort, so list order is stable when autogen files appear,
       and so action is unchanged *)
    List.map ~f:BN.of_string
      (remove_dups_and_sort
         (filter_drop ~suffix:".ml" (List.map ml_paths ~f:basename @ generated_modules)))
  in
  let intfs =
    List.map ~f:BN.of_string
      (remove_dups_and_sort
         (filter_drop ~suffix:".mli" (List.map mli_paths ~f:basename @ generated_modules)))
  in
  let impl_is_buildable = mem_of_list impls in
  let intf_is_buildable = mem_of_list intfs in
  (* select code_style uniformly for a directory *)
  let code_style =
    let default_code_style = Switched_to_ppx_style in
    match
      List.exists jbuilds ~f:(function | `switched_to_ppx_style -> true | _ -> false),
      List.exists jbuilds ~f:(function | `translate_from_camlp4 -> true | _ -> false),
      List.exists jbuilds ~f:(function | `requires_camlp4 -> true | _ -> false)
    with
    | true,true,_
    | true,_,true
    | _, true,true ->
      failwith "inconsistent code_style (camlp4/ppx) specified in jbuild"
    | true,_,_ -> Switched_to_ppx_style
    | _,true,_ -> Translate_from_camlp4
    | _,_,true -> Requires_camlp4
    | false,false,false -> default_code_style
  in
  let dc = {DC.
    code_style;
    dir;
    ocamllibflags;
    merlinflags;
    ocamlflags;
    ocamlcflags = Top.ocamlcflags;
    ocamloptflags = Top.ocamloptflags;
    js_of_ocaml_flags = [];
    xlibnames;
    ocaml_plugin_libraries;
    no_utop_alias;
    libmap;
    impls;
    intfs;
    impl_is_buildable;
    intf_is_buildable;
  }
  in
  return dc

let setup_main ~dir =
  Scheme.rules_dep (
    User_or_gen_config.load ~dir *>>= fun jbuilds ->
    create_directory_context ~dir jbuilds *>>| fun dc ->
    directory_rules dc ~dir jbuilds
  )

(*----------------------------------------------------------------------
 env
----------------------------------------------------------------------*)

let tmpdir = ".jenga.tmp"

let delete_and_recreate_tmpdir_action =
  (* We delete and recreate the tmpdir when jenga starts, and each time a polling build
   * restarts. ("*** jenga: rebuilding"). So we run this action in build_begin. *)
  bashf ~dir:Path.the_root "chmod -R +w %s &>/dev/null || true; rm -rf %s; mkdir -p %s" tmpdir tmpdir tmpdir

let putenv = (* setup external actions *)
  [
    (* /tmp partitions are small, which causes issues (for instance many simultaneous
       ocaml-plugin runs can go over the 1GB limit, especially if /tmp already contains
       stuff). So we stick the tmp directory on the local disk, which is much harder to
       fill, is easier to clean automatically, and makes admins happy. *)
    ("TMPDIR", Some (Path.to_absolute_string (relative ~dir:Path.the_root tmpdir)));
    (* Comparisons in the shell depend on the locale (eg [[ ! /j < "4.01" ]]) so let's use
       the same one for everyone *)
    ("LANG", Some "C");
    (* People often accidentally write unit tests that implicitly depend on the local
       timezone. This leads to builds which succeed in some offices and fail in others,
       which is confusing and annoying to deal with. *)
    ("TZ", Some "America/New_York");
  ]

let call_hg_showconfig_to_trigger_dirstate_change () =
  let open Async.Std in
  Sys.readdir (Path.to_absolute_string Path.the_root)
  >>= fun subdirs ->
  Deferred.List.iter ("." :: Array.to_list subdirs) ~f:(fun dir ->
    Sys.file_exists (Filename.concat dir ".hg")
    >>= function
    | `No | `Unknown -> Deferred.unit
    | `Yes ->
      (* We rely on this action not touching the dirstate file in the case it is unchanged.
         If it did, we could not unconditionally call this function from build_end without
         causing a continuously looping build *)
      run_action_now (bash ~dir:(Path.root_relative dir) "hg showconfig 2>&1 > /dev/null")
  )

let build_begin () =
  Async.Std.don't_wait_for (call_hg_showconfig_to_trigger_dirstate_change ());
  run_action_now (
    delete_and_recreate_tmpdir_action
  )

let build_end () =
  Async.Std.don't_wait_for (call_hg_showconfig_to_trigger_dirstate_change ());
  Async.Std.Deferred.unit

let deterministic_ranlib =
  (* Intercept calls made to "ranlib" from the ocaml compiler, and fix them to be
     deterministic by adding a 'D' modifier.
     - i.e. calling "ar -Ds" instead of "ar -s"
     We also now intercept calls to ar (from the ocaml compiler).
     And convert: "ar rc" -> "ar Drc" *)
  Path.to_absolute_string (bin_relative "deterministic-ranlib")

let command_lookup_path =
  `Replace [
    deterministic_ranlib;
    Path.to_absolute_string (bin_relative "cpp_quietly");
  ]

let artifacts ~dir =
  (* calculation of [artifacts] on a per directory basis *)
  Dep.action_stdout (
    Dep.source_files ~dir *>>= fun paths ->
    match paths with
    (* need special case when paths is empty, because "hg st -ni" would operate across
       the entire repo *)
    | [] -> return null_action
    | _ ->
      Dep.path (Path.root_relative ".hgignore") *>>| fun () ->
      (* In addition to normal noise from hg, we silence things like:
         $ /j/office/app/hg/prod/bin/hg st -ni -- OMakeroot README.md hg-checkexec-zAE_z8 jbuild-ignore jenga.conf
         hg-checkexec-zAE_z8: No such file or directory
      *)
      Action.process ~ignore_stderr:true ~dir
        hg_prog (["st";"-ni"; "--"] @ List.map paths ~f:basename)
  ) *>>| fun s ->
  (* The deletion of all ignored files not created by jenga is annoying. Short of having a
     better solution, let's special case one really annoying case. *)
  List.filter_map (lines_of_string s) ~f:(fun basename ->
    if String.is_suffix ~suffix:".t.err" basename
    || String.is_suffix ~suffix:".ml.corrected" basename
    then None
    else Some (relative ~dir basename))

let rec under segment dir =
  (* Return true if [segment] is found anywhere in the path [dir].
     [dir] must be a descendant of [Path.the_root].
  *)
  if (dir = Path.the_root) then false else
    Path.basename dir = segment || under segment (Path.dirname dir)

let scheme ~dir =
  (* Construct the rule scheme for given [dir] *)
  Scheme.exclude (fun path ->
    (* We [exclude] the following basenames, which are consulted at scheme setup time, to
       prevent cyclic dependencies. *)
    List.mem [
      Fe.dot_fe_sexp_basename;
      "jbuild";
      "jbuild-ignore";
    ] (Path.basename path)
  ) begin
    (* Never build or call [artifacts] within any subtree of an .hg or .projections dir *)
    if under ".hg" dir then Scheme.no_rules else
    if under ".fe" dir then Scheme.no_rules else
      Scheme.dep (
        is_ignored dir *>>| function
        | true ->
          (* Empty aliases that hydra can ask for .DEFAULT or .runtest even in ignored
             places. *)
          empty_recursive_aliases ~dir
        | false ->
          (* Special scheme for the root directory *)
          let common_rules_except_for_liblinks =
            Scheme.all [
              setup_manifest ~dir;
              setup_recursive_aliases ~dir;
              Scheme.rules (Makefile.extract ~dir);
            ]
          in
          if dir = Path.the_root then
            Scheme.all [
              Scheme.rules [
                hg_version_out_rule;
                Libmap_sexp.rule;
                Lib_clients.Cache.rule;
                alias_dot_filename_hack ~dir Lib_clients.Cache.file;
                top_api_rule;
              ];
              Fe.setup_projections_targets;
              common_rules_except_for_liblinks;
            ]
            (* Special scheme for the lib-links directory *)
          else if dirname dir = liblinks_dir then
            setup_liblinks ~dir
          else if dir = liblinks_dir then
            setup_liblinks_dir ~dir
            (* Special scheme for the ppx_cache directory *)
          else if dirname dir = ppx_cache_dir then
            setup_ppx_cache ~dir
            (* Scheme for all other directories *)
          else
            Scheme.all [
              common_rules_except_for_liblinks;
              setup_main ~dir;
            ]
      )
  end

let ocaml_bin_file = ".omake-ocaml-bin"
let ocaml_bin_file_path = root_relative ocaml_bin_file
let ocaml_bin_file_rule = write_string_rule ocaml_bin ~target:ocaml_bin_file_path

let scheme ~dir =
  (* Wrapper to force build of [ocaml_bin_file] at scheme setup time *)
  Scheme.all [
    Scheme.rules (if not (dir = Path.the_root) then [] else [
      ocaml_bin_file_rule;
      alias_dot_filename_hack ~dir ocaml_bin_file;
      public_release_files_rule;
    ]);
    Scheme.dep (
      Dep.path ocaml_bin_file_path *>>| fun () ->
      scheme ~dir)
  ]

let env = Env.create
  ~putenv
  ~command_lookup_path
  ~build_begin
  ~build_end
  ~artifacts
  scheme

let check_compiler_exists () =
  let open Async.Std in
  Sys.is_directory_exn (Compiler_selection.compiler_dir ^/ "bin") ~follow_symlinks:true
  >>| function
  | true -> ()
  | false ->
    Core.Std.Printf.eprintf "\n\nThe following compiler is not installed:\n  %s\n\n"
        Compiler_selection.compiler_dir;
    failwith "compiler not available"

let setup () =
  let open Async.Std in
  check_compiler_exists ()
  >>| fun () ->
  env
