open! Core.Std

let put     fmt = ksprintf (fun s -> Printf.printf "%s\n%!"                  s) fmt
let message fmt = ksprintf (fun s -> Printf.printf "!!JengaRoot.ml : %s\n%!" s) fmt

let getenv ~of_string ~to_string varname ~default =
  let value =
    match Sys.getenv varname with
    | None -> default
    | Some s -> of_string s
  in
  let is_default = (to_string default = to_string value) in
  if not is_default then (
    put "%s = %s" varname (to_string value);
  );
  value

let getenv_bool =
  getenv
    ~of_string:(function | "false" | "0" -> false | _ -> true)
    ~to_string:Bool.to_string

let getenv_args =
  getenv
    ~of_string:(String.split ~on:' ')
    ~to_string:(String.concat ~sep:" ")

let getenv_enumeration ?fallback varname ~choices ~default =
  assert (Map.mem choices default);
  let err() = failwithf "getenv_enumeration: %s" varname () in
  let override =
    match Sys.getenv varname with
    | None | Some "" -> default
    | Some v -> v
  in
  let choices_string = String.concat ~sep:" " (Map.keys choices) in
  if override <> default then (
    put "%s = %s # choices: %s" varname override choices_string
  );
  match Map.find choices override with
  | Some value -> value
  | None ->
    match fallback with
    | Some f -> f override
    | None ->
      (* user error *)
      message "%s should not be %s but one of: %s." varname override choices_string;
      message "If you are running Jenga from Emacs, do C-c C-o %s RET <valid-value> RET,"
        varname;
      message "and then C-c C-c to restart your build.";
      err()
