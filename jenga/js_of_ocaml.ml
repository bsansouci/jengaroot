open Import

let base = Path.relative ~dir:Path.the_root "external/js_of_ocaml"
let compiler = Path.relative ~dir:base "compiler/js_of_ocaml"
let runtime =
  List.map ~f:(fun f -> Path.relative ~dir:base ("runtime/" ^ f))
    [ "bigstring.js"
    ; "bin_prot.js"
    ; "core_kernel.js"
    ; "runtime.js"
    ; "weak.js"
    ; "nat.js"
    ; "strftime.js" ]

let rule ~dir ~flags ~src ~target =
  Rule.create ~targets:[target]
    (Dep.all_unit
       [ Dep.path compiler
       ; Dep.all_unit (List.map ~f:Dep.path runtime)
       ; Dep.path src ]
     *>>| fun () ->
     let flags =
       [ "--no-runtime"
       ; "-o"; reach_from ~dir target ]
       @ flags
     in
     let args =
       flags
       @ List.map ~f:(fun x -> reach_from ~dir x) runtime
       @ [ reach_from ~dir src ] in
     Action.process ~dir (reach_from ~dir compiler) args
    )
