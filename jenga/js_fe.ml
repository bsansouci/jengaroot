open Import

module Make(Hg : sig
    val all_the_repos : Path.t list Dep.t
    val manifest_dirs : repo:Path.t -> Path.t list Dep.t
  end) = struct

  module Projection = struct
    type t = {
      repo : Path.t;
      name : string;
    } [@@deriving fields]
    let create ~repo ~name = { repo; name; }
  end

  let dot_fe_sexp_basename = ".fe.sexp"
  let fe_prog = Path.absolute "/j/office/app/fe/prod/bin/fe"
  let fe_dir ~repo = relative ~dir:repo ".fe"
  let fe_obligations ~repo = relative ~dir:(fe_dir ~repo) "obligations-repo.sexp"
  let fe_obligations_global = relative ~dir:(fe_dir ~repo:Path.the_root) "obligations-global.sexp"

  let rec safe_dir_exists ~dir =
    if (dir = Path.the_root)
    then return true
    else
      safe_dir_exists ~dir:(dirname dir) *>>= function
      | false -> return false
      | true ->
        Dep.subdirs ~dir:(dirname dir) *>>| fun subs ->
        List.mem subs dir

  let safe_file_exists path =
    safe_dir_exists ~dir:(Path.dirname path) *>>= function
    | false -> Dep.return false
    | true ->
      Dep.file_existence path *>>= fun () ->
      Dep.file_exists path

  let fe_sexp_deps ~repo =
    Hg.manifest_dirs ~repo *>>= fun dirs ->
    Dep.all_unit (
      List.map dirs ~f:(fun dir ->
        let file = relative ~dir dot_fe_sexp_basename in
        safe_file_exists file *>>= function
        | true -> Dep.path file
        | false -> return ()
      ))

  let is_an_fe_repo ~repo =
    Dep.file_exists (fe_dir ~repo)
    *>>= function
    | false -> return false
    | true -> Dep.file_exists (fe_obligations ~repo)

  let list_projections =
    Hg.all_the_repos *>>= fun repos ->
    Dep.List.concat_map repos ~f:(fun repo ->
      is_an_fe_repo ~repo
      *>>= function
      | false -> return []
      | true ->
        Dep.action_stdout (
          Dep.all_unit [
            Dep.path fe_prog;
            Dep.path (fe_obligations ~repo);
            Dep.path fe_obligations_global;
          ] *>>| fun () ->
          bashf ~dir:repo
            !"%{quote} obligations list-projections"
            (Path.to_absolute_string fe_prog)
        ) *>>| fun s ->
        List.map (String.split_lines s) ~f:(fun name -> Projection.create ~repo ~name)
    )

  let projection_files_action projection ~follow_up:(dep, follow_up_command) =
    let {Projection.repo;name} = projection in
    Dep.all_unit [
      Dep.path fe_prog;
      Dep.path (Path.relative ~dir:repo ".hg/dirstate");
      Dep.path (fe_obligations ~repo);
      Dep.path fe_obligations_global;
      fe_sexp_deps ~repo;
      dep;
    ] *>>| fun () ->
    bashf ~dir:repo
      !"%{quote} obligations projection %{quote} | %s"
      (Path.to_absolute_string fe_prog) name follow_up_command

  let no_dep = return ()
  let projection_files projection =
    Dep.action_stdout (
      projection_files_action projection ~follow_up:(no_dep, "cat")
    ) *>>| fun s ->
    List.map (String.split_lines s) ~f:(relative ~dir:projection.repo)

  let rule_for_projection_files projection ~target =
    Rule.create ~targets:[target]
      (projection_files_action projection
         ~follow_up:(no_dep,
                     sprintf !"cat > %{quote}"
                       (Path.reach_from ~dir:projection.repo target)))

  (*----------------------------------------------------------------------
   builds driven by projections
  ----------------------------------------------------------------------*)

  let parse_infer_targets_output ~repo string =
    let words = words_of_string string in
    List.map words ~f:(fun string ->
      match String.rsplit2 string ~on:'/' with
      | None ->
        failwiths "parse_infer_targets_output failed to convert"
          string [%sexp_of: string]
      | Some (dir,s) ->
        let alias_name = (* loose leading "." from .DEFAULT *)
          match String.chop_prefix s ~prefix:"." with None -> s | Some s -> s
        in
        Alias.create ~dir:(relative ~dir:repo dir) alias_name
    )

  let projection_rule ~proj ~with_qtest =
    let {Projection.repo;name} = proj in
    let infer_targets_for_files = relative ~dir:Config.bin_dir "infer-targets-for-files" in
    let suffix = if with_qtest then "" else "-except-qtest" in
    let projection_name = name ^ "-projection" ^ suffix in
    let alias_name = Alias.create ~dir:Path.the_root projection_name in
    Rule.alias alias_name [
      Dep.action_stdout (
        projection_files_action proj ~follow_up:(
          Dep.path infer_targets_for_files,
          sprintf !"%{quote} %{quote}"
            (reach_from ~dir:repo infer_targets_for_files)
            (Bool.to_string with_qtest)))
      *>>= fun string ->
      let aliases = parse_infer_targets_output ~repo string in
      let deps = List.map aliases ~f:Dep.alias in
      Dep.all_unit deps
    ]

  let setup_projections_targets =
    Scheme.rules_dep (
      list_projections *>>| fun projs ->
      List.concat_map projs ~f:(fun proj -> [
        projection_rule ~proj ~with_qtest:false;
        projection_rule ~proj ~with_qtest:true;
      ]))

  module Projections_check = struct

    let projections names =
      list_projections *>>| fun the_projection_list ->
      let repo_by_name =
        String.Table.of_alist_multi
          (List.map the_projection_list ~f:(fun { name; repo } -> name, repo))
      in
      List.map names ~f:(fun pname ->
        match Hashtbl.find repo_by_name pname with
        | None | Some [] -> Or_error.error "unknown projection" pname [%sexp_of: string]
        | Some [repo] -> Ok { Projection.repo; name = pname }
        | Some (_ :: _ :: _ as repos) ->
          Or_error.error "duplicate projection" (pname, `Defined_in repos)
            [%sexp_of: string * [ `Defined_in of Path.t list ]])
      |> Or_error.combine_errors
    ;;

    let files_in_projections ~projections =
      Dep.List.concat_map projections ~f:projection_files
      *>>| Path.Set.of_list
    ;;

    let dependencies_and_targets ~of_:target =
      (* Technically, we miss dependencies in here: we miss all the dependencies of the
         schemes, and all the dependencies only used to build actions. Concretely, these
         dependencies are jbuild, jbuild-ignore, a bunch dependencies of running the
         jengaroot (like .libdeps file, .ml.d files, .cmi.deps files, globs, reflections,
         Dep.action_stdout, Dep.content, etc.)
         The only things that actually matter in there are jbuild, which is not too bad
         since one would have to go out of their way (in the .fe.sexp) to not review
         them. *)
      Reflect.reachable ~keep:(fun _ -> true) [target] *>>| fun trips ->
      let make_set ~f =
        let set = ref Path.Set.empty in
        List.iter trips ~f:(fun trip ->
          List.iter (f trip) ~f:(fun path ->
            set := Set.add !set path));
        !set
      in
      Set.to_list (make_set ~f:(fun trip -> trip.deps)),
      make_set ~f:(fun trip -> trip.targets)
    ;;

    let tracked_files =
      Hg.all_the_repos
      *>>= fun all_the_repos ->
      (* Better not miss repositories in here, because if we say a file is not tracked, then
         we won't require that it is in the right projection. *)
      Dep.List.concat_map all_the_repos ~f:(fun repo ->
        Dep.action_stdout (
          Dep.path (Path.relative ~dir:repo ".hg/dirstate") *>>| fun () ->
          (* Here we include all files except -i. This test is not particularly useful when
             the repository is not clean, but just in case something goes wrong, let's
             approximate in the direction of consider that more things should be
             reviewed. *)
          Action.process ~dir:repo "hg" ["st"; "-mardcun"])
        *>>| fun x ->
        List.map (String.split_lines x) ~f:(relative ~dir:repo)
      ) *>>| Path.Set.of_list
    ;;

    let internal_check ~dependencies_and_targets ~tracked_files ~files_in_projections =
      let (dependencies, targets) = dependencies_and_targets in
      With_return.with_return (fun r ->
        Ok (
          List.partition_map dependencies ~f:(fun x ->
            if not (Set.mem tracked_files x)
            then begin
              if (* the common case, files both generated and untracked  *)
                Set.mem targets x
                (* file untracked because not in the tree, ok since it can't be reviewed *)
              || not (Path.is_descendant ~dir:Path.the_root x)
                (* things under .hg are the same as files outside the tree. The only
                   such dependency we should have is on */.hg/dirstate* *)
              || (Path.basename (Path.dirname x)) = ".hg"
              then `Fst (sprintf !"# not tracked %{Path}" x)
              else
                (* This check is here to make sure we don't wrongly exclude dependencies.
                   Examples of problems this could catch:
                   - instead of getting the tracked files by running [hg status] in all
                   subrepos, we used to run [hg status] only in the repository that
                   contained the listed projections. So if jane was not listed in the
                   jbuild, all its files looked untracked and we would not complain.
                   - [Hg.all_the_repos] misses scaffold repositories
                   - the format of hg status changes (for instance -n stopped working) *)
                let msg =
                  sprintf !"jengaroot bug: unexpected dependency on %{Path}, \
                            because it is not generated by jenga, not tracked \
                            by hg, but it is below the jengaroot." x
                in
                r.return (Or_error.error_string msg)
            end else if Set.mem files_in_projections x
            then `Fst (sprintf !"# in proj %{Path}" x)
            else `Snd x)))
    ;;

    let () =
      let dependencies_and_targets =
        (List.map ~f:Path.root_relative [ "a/a.ml"],
         Path.Set.empty)
      in
      let tracked_files = Path.Set.empty in
      let files_in_projections = Path.Set.empty in
      match internal_check ~dependencies_and_targets ~tracked_files ~files_in_projections with
      | Ok _ -> assert false
      | Error e -> assert (String.(Error.to_string_hum e =
                             "jengaroot bug: unexpected dependency on a/a.ml, because it \
                              is not generated by jenga, not tracked by hg, but it is \
                              below the jengaroot."))
    ;;

    let check ~exe ~allowed_projections =
      Dep.both
        (dependencies_and_targets ~of_:exe)
        (Dep.both
           tracked_files
           (projections allowed_projections
            *>>= function
            | Error _ as e -> return e
            | Ok projections ->
              files_in_projections ~projections
              *>>| fun x -> Ok x))
      *>>| fun (dependencies_and_targets, (tracked_files, rhs_or_error)) ->
      Or_error.bind rhs_or_error (fun files_in_projections ->
        internal_check ~dependencies_and_targets ~tracked_files ~files_in_projections
      )
    ;;

    let rule_for_testing ~target ~exe ~allowed_projections =
      Rule.create ~targets:[target]
        (check ~exe ~allowed_projections
         *>>| fun res ->
         let str =
           match res with
           | Error e -> Error.to_string_hum e
           | Ok (why_ok, why_error) ->
             String.concat ~sep:"\n"
               (List.map why_error ~f:Path.to_string @ why_ok)
         in
         Action.save str ~target
        )
    ;;

    let error_msg_dep ~dir ~exe ~allowed_projections =
      check ~exe ~allowed_projections *>>| function
      | Error e -> Some (Error.to_string_hum e)
      | Ok (_why_ok, []) -> None
      | Ok (_why_ok, (_ :: _ as paths)) ->
        let n = 5 in
        let first_paths = List.map (List.take paths n) ~f:Path.to_string in
        let first_paths =
          if List.length paths > n then first_paths @ ["..."] else first_paths
        in
        Some (sprintf "Some dependencies of %s are not in the projections %s: %s"
                (Path.reach_from ~dir exe)
                (String.concat ~sep:"," allowed_projections)
                (String.concat ~sep:", " first_paths))
    ;;
  end
end
