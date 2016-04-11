(* Generated code should depend on the environment in scope as little as possible.
   E.g. rather than [foo = []] do [match foo with [] ->], to eliminate the use of [=].  It
   is especially important to not use polymorphic comparisons, since we are moving more
   and more to code that doesn't have them in scope. *)


open StdLabels
open Ppx_core.Std
open Asttypes
open Parsetree
open Ast_builder.Default
open Ppx_type_conv.Std

[@@@metaloc loc]

module List = struct
  include List
  (* import from core *)
  let init n ~f =
    if n < 0 then invalid_arg "List.init";
    let rec loop i accum =
      assert (i >= 0);
      if i = 0 then accum
      else loop (i-1) (f (i-1)::accum)
    in
    loop n []
  ;;
end

module Create = struct
  let lambda loc xs body =
    List.fold_right xs ~init:body ~f:(fun (label, p) e -> pexp_fun ~loc label None p e)
  ;;

  let lambda_sig loc arg_tys body_ty =
    List.fold_right arg_tys ~init:body_ty ~f:(fun arg_ty acc ->
      ptyp_arrow ~loc "" arg_ty acc)
  ;;

  let lambda_sig' loc arg_tys body_ty =
    List.fold_right arg_tys ~init:body_ty ~f:(fun (label, arg_ty) acc ->
      ptyp_arrow ~loc label arg_ty acc)
  ;;
end

module Variant_definition = struct
  type t = {
    name : string;
    body_ty : core_type;
    arg_tys : core_type list;
    kind : [ `Normal | `Polymorphic | `Type ]
  }

  let to_constructor_type t loc =
    match t.kind with
    | `Normal | `Polymorphic -> Some (
      Create.lambda_sig loc t.arg_tys t.body_ty
    )
    | `Type -> None
end

module Inspect = struct
  let row_field body_ty rf : Variant_definition.t =
    match rf with
    | Rtag (name, _, true, _) | Rtag (name, _, _, []) ->
      { name
      ; arg_tys = []
      ; kind = `Polymorphic
      ; body_ty
      }
    | Rtag (name, _, false, tp :: _) ->
      { name
      ; arg_tys = [tp]
      ; kind = `Polymorphic
      ; body_ty
      }
    | Rinherit ty ->
      match ty.ptyp_desc with
      | Ptyp_constr ({ txt = Lident name; _ }, []) ->
        { name
        ; arg_tys = []
        ; kind = `Type
        ; body_ty
        }
      | Ptyp_constr ({ txt = Lident name; _ }, [tp]) ->
        { name
        ; arg_tys = [tp]
        ; kind = `Type
        ; body_ty
        }
      | _ ->
        Location.raise_errorf ~loc:ty.ptyp_loc "ppx_variants_conv: unknown type"

  let constructor body_ty cd : Variant_definition.t =
    if cd.pcd_res <> None then
      Location.raise_errorf ~loc:cd.pcd_loc "GADTs are not supported by variantslib";
    { name = cd.pcd_name.txt
    ; body_ty
    ; arg_tys = cd.pcd_args
    ; kind = `Normal
    }

  let variants ty _loc body_ty =
    match ty with
    | `Polymorphic row_fields -> List.map row_fields ~f:(row_field body_ty)
    | `Normal      cds        -> List.map cds        ~f:(constructor body_ty)
end

let raise_unsupported loc =
  Location.raise_errorf ~loc
    "Unsupported use of variants (you can only use it on variant types)."

let variants_module = function
  | "t" -> "Variants"
  | type_name -> "Variants_of_" ^ type_name
;;

module Gen_sig = struct
  let apply_type loc ~ty_name ~tps =
    ptyp_constr ~loc (Located.lident ~loc ty_name) tps

  let label_arg _loc name ty =
    (String.lowercase name, ty)
  ;;

  let variant_arg loc f v =
    let variant =
      match Variant_definition.to_constructor_type v loc with
      | None -> None
      | Some constructor_type -> (
          Some [%type: [%t constructor_type] Variantslib.Variant.t]
        )
    in
    label_arg loc v.Variant_definition.name (f ~variant)
  ;;

  let v_fold_fun ~ty_name ~tps loc ty =
    let variant_type = apply_type loc ~ty_name ~tps in
    let variants = Inspect.variants ty loc variant_type in
    let f = variant_arg loc (fun ~variant ->
      match variant with
      | Some variant ->
          [%type: 'acc__ -> [%t variant] -> 'acc__ ]
      | None -> [%type:  'acc__ -> 'acc__ ]
    )
    in
    let types = List.map variants ~f in
    let init_ty = label_arg loc "init" [%type:  'acc__ ] in
    let t = Create.lambda_sig' loc (init_ty :: types) [%type: 'acc__ ] in
    psig_value ~loc (value_description ~loc ~name:(Located.mk ~loc "fold") ~type_:t
                       ~prim:[])
  ;;

  let v_iter_fun ~ty_name ~tps loc ty =
    let variant_type = apply_type loc ~ty_name ~tps in
    let variants = Inspect.variants ty loc variant_type in
    let f = variant_arg loc (fun ~variant ->
      match variant with
      | Some variant ->
          [%type:  [%t variant] -> unit]
      | None -> [%type:  unit -> unit ]
    )
    in
    let types = List.map variants ~f in
    let t = Create.lambda_sig' loc
      types [%type:  unit ] in
    psig_value ~loc (value_description ~loc ~name:(Located.mk ~loc "iter") ~type_:t
                       ~prim:[])
  ;;

  let v_map_fun ~ty_name ~tps loc ty =
    let module V = Variant_definition in
    let variant_type = apply_type loc ~ty_name ~tps in
    let variants = Inspect.variants ty loc variant_type in
    let result_type = [%type:  'result__ ] in
    let f v =
      let variant =
        match V.to_constructor_type v loc with
        | None -> [%type:  unit -> [%t result_type] ]
        | Some constructor_type ->
            Create.lambda_sig loc
              ([%type: [%t constructor_type] Variantslib.Variant.t ] ::
                  v.V.arg_tys) result_type
      in
      label_arg loc v.V.name variant
    in
    let types = List.map variants ~f in
    let t = Create.lambda_sig' loc
      (("", variant_type) :: types) result_type in
    psig_value ~loc (value_description ~loc ~name:(Located.mk ~loc "map") ~type_:t
                       ~prim:[])
  ;;

  let v_descriptions ~ty_name:_ ~tps:_ loc _ =
    psig_value ~loc (value_description ~loc ~name:(Located.mk ~loc "descriptions")
                       ~type_:[%type: (string * int) list] ~prim:[])

  let variant ~ty_name ~tps loc ty =
    let variant_type = apply_type loc ~ty_name ~tps in
    let variants = Inspect.variants ty loc variant_type in
    let conv_variant (res_constructors,res_variants) v =
      let module V = Variant_definition in
      let constructor_type = V.to_constructor_type v loc in
      match constructor_type with
      | None -> (res_constructors,res_variants)
      | Some constructor_type ->
          let name = String.lowercase v.V.name in
          ( psig_value ~loc (value_description ~loc ~name:(Located.mk ~loc name)
                               ~type_:constructor_type ~prim:[])
            :: res_constructors
          , psig_value ~loc (value_description ~loc ~name:(Located.mk ~loc name)
                               ~type_:[%type: [%t constructor_type] Variantslib.Variant.t]
                               ~prim:[])
            :: res_variants
          )
    in
    let constructors, variants =
      List.fold_left variants ~init:([], []) ~f:conv_variant
    in
    let fold = v_fold_fun ~ty_name ~tps loc ty in
    let iter = v_iter_fun ~ty_name ~tps loc ty in
    let map = v_map_fun ~ty_name ~tps loc ty in
    let descriptions = v_descriptions ~ty_name ~tps loc ty in
    constructors @
    [ psig_module ~loc
        (module_declaration
           ~loc
           ~name:(Located.mk ~loc (variants_module ty_name))
           ~type_:(pmty_signature ~loc
                     (variants @ [ fold
                                 ; iter
                                 ; map
                                 ; descriptions
                                 ])))
    ]
  ;;

  let variants_of_td td =
    let loc = td.ptype_loc in
    let tps = List.map td.ptype_params ~f:fst in
    let ty_name = td.ptype_name.txt in
    match td.ptype_kind with
    | Ptype_variant cds -> variant ~ty_name ~tps loc (`Normal cds)
    | Ptype_record _ | Ptype_open -> raise_unsupported loc
    | Ptype_abstract ->
      match td.ptype_manifest with
      | Some { ptyp_desc = Ptyp_variant (rf, _, _); _ } ->
        variant ~ty_name ~tps loc (`Polymorphic rf)
      | _ -> raise_unsupported loc

  let generate ~loc ~path:_ (rec_flag, tds) =
    if rec_flag = Nonrecursive then
      Location.raise_errorf ~loc
        "nonrec is not compatible with the `ppx_variants_conv' preprocessor";
    match tds with
    | [td] -> variants_of_td td
    | _ -> Location.raise_errorf ~loc "ppx_variants_conv: not supported"
end

module Gen_str = struct

  let variant_name_to_string v =
    match String.lowercase v with
    | "try" -> "try_"
    | s -> s

  let variants loc ty variant_name =
    let module V = Variant_definition in
    let conv_variant (rank, res_constructors, res_variants) v =
      match v.V.kind with
      | `Type -> (rank + 1, res_constructors, res_variants)
      | (`Normal | `Polymorphic) as kind ->
          let uncapitalized = variant_name_to_string v.V.name in
          let constructor =
            let vars = List.init (List.length v.V.arg_tys) ~f:(fun i ->
              "v" ^ string_of_int i)
            in
            let constructed_value =
              let arg =
                match vars with
                | [] -> None
                | [v] -> Some (evar ~loc v)
                | _ -> Some (pexp_tuple ~loc (List.map vars ~f:(evar ~loc)))
              in
              match kind with
              | `Normal      -> pexp_construct ~loc (Located.lident ~loc v.V.name) arg
              | `Polymorphic -> pexp_variant   ~loc                  v.V.name  arg
            in
            pstr_value ~loc Nonrecursive
              [ value_binding ~loc ~pat:(pvar ~loc uncapitalized)
                  ~expr:(eabstract ~loc (List.map vars ~f:(pvar ~loc)) constructed_value)
              ]
          in
          let variant =
            [%stri
              let [%p pvar ~loc uncapitalized] =
                { Variantslib.Variant.
                  name        = [%e estring ~loc v.V.name     ]
                ; rank        = [%e eint    ~loc rank         ]
                ; constructor = [%e evar    ~loc uncapitalized]
                }
            ]
          in
          ( rank + 1
          , constructor :: res_constructors
          , variant     :: res_variants
          )
    in
    let variants = Inspect.variants ty loc variant_name in
    List.fold_left variants ~init:(0, [], []) ~f:conv_variant
  ;;

  let label_arg ?label loc name =
    let l =
      match label with
      | None    -> name
      | Some n  -> n
    in
    (l, pvar ~loc name)
  ;;

  let label_arg_fun loc name =
    label_arg ~label:name loc (name ^ "_fun__")
  ;;

  let v_fold_fun loc ty body_ty  =
    let module V = Variant_definition in
    let variants = Inspect.variants ty loc body_ty in
    let variant_fold acc_expr variant =
      let variant_name = variant_name_to_string variant.V.name in
      match variant.V.kind with
      | `Type ->
        [%expr  [%e evar ~loc @@ variant_name ^ "_fun__" ] [%e acc_expr] ]
      | `Polymorphic | `Normal ->
        [%expr  [%e evar ~loc @@ variant_name ^ "_fun__" ] [%e acc_expr]
                  [%e evar ~loc variant_name]
        ]
    in
    let body =
      List.fold_left variants ~init:[%expr  init__ ] ~f:variant_fold
    in
    let patterns =
      List.map variants ~f:(fun variant ->
        label_arg_fun loc (variant_name_to_string variant.V.name))
    in
    let init = label_arg ~label:"init" loc "init__" in
    let lambda = Create.lambda loc (init :: patterns) body in
    [%stri let fold = [%e lambda] ]
  ;;

  let v_descriptions loc ty body_ty  =
    let module V = Variant_definition in
    let variants = Inspect.variants ty loc body_ty in
    let f v =
      [%expr
        ( [%e estring ~loc v.V.name]
        , [%e eint ~loc @@ List.length v.V.arg_tys]
        )
      ]
    in
    let variant_names = List.map ~f variants in
    let expr_of_list loc = (* for some reason I can't put this at the top level *)
      let rec loop =
        function
          | [] -> [%expr  [] ]
          | x :: xs -> [%expr [%e x] :: [%e loop xs] ]
      in
      loop
    in
    [%stri let descriptions = [%e expr_of_list loc variant_names] ]
  ;;

  let v_map_fun loc ty body_ty =
    let module V = Variant_definition in
    let variants = Inspect.variants ty loc body_ty in
    let variant_match_case variant =
      match variant.V.kind with
      | `Type ->
        case ~guard:None
          ~lhs:(ppat_type ~loc (Located.lident ~loc variant.V.name))
          ~rhs:(evar ~loc @@ variant.V.name ^ "_fun__")
      | (`Polymorphic | `Normal) as kind ->
          let vars =
            List.init (List.length variant.V.arg_tys) ~f:(fun i ->
              "v" ^ string_of_int i)
          in
          let pattern =
            let arg =
              match vars with
              | [] -> None
              | [v] -> Some (pvar ~loc v)
              | _ -> Some (ppat_tuple ~loc @@ List.map vars ~f:(pvar ~loc))
            in
            match kind with
            | `Polymorphic -> ppat_variant   ~loc                  variant.V.name  arg
            | `Normal      -> ppat_construct ~loc (Located.lident ~loc variant.V.name) arg
          in
          let uncapitalized = variant_name_to_string variant.V.name in
          let value =
            List.fold_left vars
              ~init:(eapply ~loc (evar ~loc @@ uncapitalized ^ "_fun__") [evar ~loc uncapitalized])
              ~f:(fun acc_expr var -> eapply ~loc acc_expr [evar ~loc var])
          in
          case ~guard:None ~lhs:pattern ~rhs:value
    in
    let body = pexp_match ~loc [%expr t__] (List.map variants ~f:variant_match_case) in
    let patterns =
      List.map variants ~f:(fun variant ->
        label_arg_fun loc (variant_name_to_string variant.V.name))
    in
    let lambda = Create.lambda loc (("", [%pat? t__]) :: patterns) body in
    [%stri let map = [%e lambda] ]
  ;;

  let v_iter_fun loc ty body_ty  =
    let module V = Variant_definition in
    let variants = Inspect.variants ty loc body_ty in
    let names = List.map variants ~f:(fun v -> variant_name_to_string v.V.name) in
    let variant_iter variant =
      let variant_name = variant_name_to_string variant.V.name in
      let apply_to =
        match variant.V.kind with
        | `Type -> [%expr  () ]
        | `Polymorphic | `Normal -> evar ~loc variant_name
      in
      [%expr ([%e evar ~loc @@ variant_name ^ "_fun__"] [%e apply_to] : unit) ]
    in
    let body =
      match variants with
      | [] -> assert false
      | hd :: tl ->
        List.fold_left tl
          ~init:(variant_iter hd)
          ~f:(fun acc n -> pexp_sequence ~loc acc (variant_iter n))
    in
    let patterns = List.map names ~f:(label_arg_fun loc) in
    let lambda = Create.lambda loc patterns body in
    [%stri let iter = [%e lambda] ]
  ;;

  let variant ~variant_name ~tps loc ty =
    let body_ty =
      Create.lambda_sig loc tps @@ ptyp_constr ~loc (Located.lident ~loc variant_name) []
    in
    let _num_variants, constructors, variants = variants loc ty body_ty in
    let fold = v_fold_fun loc ty body_ty in
    let iter = v_iter_fun loc ty body_ty in
    let map = v_map_fun loc ty body_ty in
    let descriptions = v_descriptions loc ty body_ty in
    constructors @
    [ pstr_module ~loc
        (module_binding
           ~loc
           ~name:(Located.mk ~loc (variants_module variant_name))
           ~expr:(pmod_structure ~loc
                    (variants @ [ fold
                                ; iter
                                ; map
                                ; descriptions
                                ])))
    ]
  ;;

  let variants_of_td td =
    let loc = td.ptype_loc in
    let tps = List.map td.ptype_params ~f:fst in
    let variant_name = td.ptype_name.txt in
    match td.ptype_kind with
    | Ptype_variant cds -> variant ~variant_name ~tps loc (`Normal cds)
    | Ptype_record _ | Ptype_open -> raise_unsupported loc
    | Ptype_abstract ->
      match td.ptype_manifest with
      | Some { ptyp_desc = Ptyp_variant (rf, _, _); _ } ->
        variant ~variant_name ~tps loc (`Polymorphic rf)
      | _ -> raise_unsupported loc

  let generate ~loc ~path:_ (rec_flag, tds) =
    if rec_flag = Nonrecursive then
      Location.raise_errorf ~loc
        "nonrec is not compatible with the `ppx_variants_conv' preprocessor";
    match tds with
    | [td] -> variants_of_td td
    | _ -> Location.raise_errorf ~loc "ppx_variants_conv: not supported"
end

let () =
  Type_conv.add "variants"
    ~str_type_decl:(Type_conv.Generator.make_noarg Gen_str.generate)
    ~sig_type_decl:(Type_conv.Generator.make_noarg Gen_sig.generate)
  |> Type_conv.ignore;
;;
