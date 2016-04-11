include Core.Std
include (Jenga_lib.Api
         : module type of struct include Jenga_lib.Api end
           with module Path := Jenga_lib.Api.Path
           with module Action := Jenga_lib.Api.Action)

module Path = struct
  include Jenga_lib.Api.Path
  let precise_dirname path =
    let dir = dirname path in
    if dir = path
    then `Root_or_repo_root
    else `Ok dir
end

let relative = Path.relative
let reach_from = Path.reach_from
let root_relative = Path.root_relative
let basename = Path.basename
let dirname = Path.dirname
let suffixed ~dir name suf = relative ~dir (name ^ suf)

let return = Dep.return
let ( *>>= ) = Dep.bind
let ( *>>| ) = Dep.map

let quote = Shell.escape

let concat_quoted l = String.concat ~sep:" " (List.map ~f:quote l)

let bash ?ignore_stderr ~dir command_string =
  Jenga_lib.Api.Action.process ?ignore_stderr ~dir ~prog:"bash" ~args:[
    "-e"; "-u"; "-o"; "pipefail";
    "-c"; command_string;
  ] ()

let bashf ?ignore_stderr ~dir fmt =
  ksprintf (fun str -> bash ?ignore_stderr ~dir str) fmt

let null_action = bash ~dir:Path.the_root "true"

let words_of_string string =
  let string = String.tr string ~target:'\n' ~replacement:' ' in
  let words = String.split string ~on:' ' in
  let words = List.filter words ~f:(function | "" -> false | _ -> true) in
  words

module Action = struct
  include Jenga_lib.Api.Action

  let process ?env ?ignore_stderr ~dir prog args =
    let prog, args =
      match env with
      | None -> prog, args
      | Some env ->
        assert (String.mem prog '=' = false);
        let env = List.map env ~f:(fun (key, data) -> sprintf "%s=%s" key data) in
        "/usr/bin/env", List.concat [ env; prog :: args; ]
    in
    process ?ignore_stderr ~dir ~prog ~args ()
  ;;

end
