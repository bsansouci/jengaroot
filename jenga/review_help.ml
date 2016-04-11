open Import

let help_filename ~dir name = relative ~dir (name ^ "-help-for-review.org")

let command_inspector =
  Path.relative ~dir:Path.the_root "app/command-inspector/bin/command_inspector.exe"

let rule (module Mode : Ocaml_mode.S) ~dir name =
  let target = help_filename ~dir name in
  Rule.create ~targets:[ target; ] begin
    let exe = suffixed ~dir name Mode.exe in
    Dep.all_unit [Dep.path exe; Dep.path command_inspector]
    *>>| fun () ->
    bashf ~dir !"%{quote} %{quote} > %{quote}"
      (Path.reach_from ~dir command_inspector)
      (Path.reach_from ~dir exe)
      (Path.reach_from ~dir target)
  end
