open StdLabels
open Ppx_core.Std
open Ppx_type_conv.Std
open Asttypes
open Parsetree
open Ast_builder.Default

[@@@metaloc loc]


module Gen = struct
  let let_in loc list_lid_expr body =
    List.fold_right list_lid_expr ~init:body ~f:(fun (lid, expr) body ->
      [%expr let [%p pvar ~loc lid] = [%e expr] in [%e body] ])
end

module List = struct
  include List
  let init ~f n =
    let rec aux acc index =
      if index < 0 then acc else
        let acc = f index :: acc in
        aux acc (pred index)
    in
    aux [] (pred n)

  let fold_righti list ~f ~init =
    let length = length list in
    let (acc, _) =
      fold_right
        ~f:(fun el (acc, index) -> let acc = f index el acc in (acc, pred index))
        ~init:(init, pred length)
        list
    in
    acc

  let mapi ~f list =
    let rev, _ =
      fold_left
        ~f:(fun (acc, index) el -> f index el :: acc, succ index)
        ~init:([], 0)
        list
    in
    List.rev rev
end

module Field_case = struct
  type t =
    { label        : string
    ; ctyp         : core_type
    ; index        : int
    ; mutable_flag : Asttypes.mutable_flag
    }
end

module Variant_case = struct
  type t =
    { label       : string
    ; ctyp        : core_type option
    ; poly        : bool
    ; arity       : int
    ; index       : int
    ; arity_index : int
    }

  let patt ~loc t arg =
    let label = t.label in
    if t.poly then
      ppat_variant ~loc label arg
    else
      ppat_construct ~loc (Located.lident ~loc label) arg

  let expr ~loc t arg =
    let label = t.label in
    if t.poly then
      pexp_variant ~loc label arg
    else
      pexp_construct ~loc (Located.lident ~loc label) arg

  let ocaml_repr ~loc { label ; poly ; arity_index ; _ } =
    if poly
    then [%expr Typerep_lib.Std.Typerep_obj.repr_of_poly_variant
                  [%e pexp_variant ~loc label None] ]
    else eint ~loc arity_index
end

module Branches = struct
  let fields fields =
    let mapi index ld : Field_case.t =
      { label        = ld.pld_name.txt
      ; ctyp         = ld.pld_type
      ; index        = index
      ; mutable_flag = ld.pld_mutable
      }
    in
    List.mapi fields ~f:mapi

  let row_fields rfs =
      (* duplicates like [ `A | `B | `A ] cause warnings in the generated code (duplicated
         patterns), so we don't have to deal with them. *)
      let no_arg = let r = ref (-1) in fun () -> incr r; !r in
      let with_arg = let r = ref (-1) in fun () -> incr r; !r in
      let mapi index rf : Variant_case.t =
        match rf with
        | Rtag (label, _, true, _) | Rtag (label, _, _, []) ->
          { label;
            ctyp = None;
            poly = true;
            arity = 0;
            index;
            arity_index = no_arg ();
          }
        | Rtag (label, _, false, ctyp :: _) ->
          { label;
            ctyp = Some ctyp;
            poly = true;
            arity = 1;
            index;
            arity_index = with_arg ();
          }
        | Rinherit ty ->
          Location.raise_errorf ~loc:ty.ptyp_loc
            "ppx_typerep_conv: unknown type"
      in
      List.mapi rfs ~f:mapi

  let constructors cds =
    let no_arg = let r = ref (-1) in fun () -> incr r; !r in
    let with_arg = let r = ref (-1) in fun () -> incr r; !r in
    let mapi index cd : Variant_case.t =
      if cd.pcd_res <> None then
        Location.raise_errorf ~loc:cd.pcd_loc
          "ppx_typerep_conv: GADTs not supported";
      let label = cd.pcd_name.txt in
      let loc = cd.pcd_loc in
      match cd.pcd_args with
      | [] ->
        { label;
          ctyp = None;
          poly = false;
          arity = 0;
          index;
          arity_index = no_arg ();
        }
      | args ->
        let arity = List.length args in
        let ctyp = ptyp_tuple ~loc args in
        { label;
          ctyp = Some ctyp;
          poly = false;
          arity;
          index;
          arity_index = with_arg ();
        }
    in
    List.mapi cds ~f:mapi
end

module Typerep_signature = struct
  let sig_of_typerep_of_t td =
    combinator_type_of_type_declaration td ~f:(fun ~loc ty ->
      [%type:  [%t ty] Typerep_lib.Std.Typerep.t])

  let sig_of_typename_of_t td =
    combinator_type_of_type_declaration td ~f:(fun ~loc ty ->
      [%type:  [%t ty] Typerep_lib.Std.Typename.t])

  let sig_of_one_def td =
    let typerep_of  = sig_of_typerep_of_t  td in
    let typename_of = sig_of_typename_of_t td in
    let loc = td.ptype_loc in
    let type_name = td.ptype_name.txt in
    [ psig_value ~loc (value_description ~loc
                         ~name:(Located.mk ~loc @@ "typerep_of_"  ^ type_name)
                         ~type_:typerep_of
                         ~prim:[])
    ; psig_value ~loc (value_description ~loc
                         ~name:(Located.mk ~loc @@ "typename_of_" ^ type_name)
                         ~type_:typename_of
                         ~prim:[])
    ]

  let sig_generator ~loc:_ ~path:_ (_rec_flag, tds) =
    List.map tds ~f:sig_of_one_def
    |> List.flatten

  let gen = Type_conv.Generator.make Type_conv.Args.empty sig_generator
end

module Typerep_implementation = struct

  module Util : sig

    val typename_field : loc:Location.t -> type_name:string option -> expression

    val arg_of_param : string -> string

    val params_names : params:(core_type * variance) list -> string list
    val params_patts : loc:Location.t -> params_names:string list -> pattern list

    val type_name_module_definition : loc:Location.t -> path:string ->
      type_name:string -> params_names:string list -> structure

    val with_named :
      loc:Location.t -> type_name:string -> params_names:string list -> expression -> expression

    val typerep_of_t_coerce : type_declaration -> core_type option

    val typerep_abstract : loc:Location.t -> path:string ->
      type_name:string -> params_names:string list -> structure_item

    module Record : sig

      val field_n_ident : fields:(Field_case.t list) -> int -> string

      val fields :
        loc:Location.t
        -> typerep_of_type:(core_type -> expression)
        -> fields:Field_case.t list
        -> (int * string * expression) list

      val create : loc:Location.t -> fields:Field_case.t list -> expression

      val has_double_array_tag : loc:Location.t -> fields:Field_case.t list -> expression
    end

    module Variant : sig

      val tag_n_ident : variants:(Variant_case.t list) -> int -> string

      val tags :
        loc:Location.t
        -> typerep_of_type:(core_type -> expression)
        -> variants:Variant_case.t list
        -> (int * expression) list

      val value : loc:Location.t -> variants:Variant_case.t list -> expression

      val polymorphic : loc:Location.t -> variants:Variant_case.t list -> expression
    end

  end = struct

    let str_item_type_and_name ~loc ~path ~params_names ~type_name =
      let params =
        List.map params_names
          ~f:(fun name -> (ptyp_var ~loc name, Invariant))
      in
      let td =
        let manifest =
          ptyp_constr ~loc (Located.lident ~loc type_name)
            (List.map params_names ~f:(ptyp_var ~loc))
        in
        nonrec_type_declaration ~loc ~name:(Located.mk ~loc "t")
          ~params
          ~manifest:(Some manifest)
          ~kind:Ptype_abstract
          ~cstrs:[]
          ~private_:Public
      in
      let name_def =
        let full_type_name = Printf.sprintf "%s.%s" path type_name in
        [%stri let name = [%e estring ~loc full_type_name] ]
      in
      pmod_structure ~loc
        [ pstr_type ~loc [td]
        ; name_def
        ]

    let arg_of_param name = "_of_" ^ name
    let name_of_t ~type_name = "name_of_" ^ type_name

    let typename_field ~loc ~type_name =
      match type_name with
      | None ->
        [%expr Typerep_lib.Std.Typename.create () ]
      | Some type_name ->
        [%expr Typerep_lib.Std.Typerep.Named.typename_of_t
                 [%e evar ~loc @@  name_of_t ~type_name]
        ]

    let params_names ~params =
      List.map params ~f:(fun x -> (get_type_param_name x).txt)

    let params_patts ~loc ~params_names =
      List.map params_names ~f:(fun s -> pvar ~loc @@ arg_of_param s)

    let type_name_module_name ~type_name = "Typename_of_" ^ type_name

    let with_named ~loc ~type_name ~params_names expr =
      let name_t =
        eapply ~loc (pexp_ident ~loc
                  @@ Located.lident ~loc
                  @@ type_name_module_name ~type_name ^ ".named")
          (List.map params_names ~f:(fun name -> evar ~loc @@ arg_of_param name))
      in
      let name_of_t = name_of_t ~type_name in
      let args = [%expr ( [%e evar ~loc name_of_t], Some (lazy [%e expr]) ) ] in
      [%expr  let [%p pvar ~loc name_of_t] = [%e name_t] in
              Typerep_lib.Std.Typerep.Named [%e args] ]

    let typerep_of_t_coerce td =
      match td.ptype_params with
      | [] -> None
      | params ->
        let t =
          combinator_type_of_type_declaration td ~f:(fun ~loc ty ->
            [%type: [%t ty] Typerep_lib.Std.Typerep.t])
        in
        Some (ptyp_poly ~loc:td.ptype_loc
                (List.map params ~f:(fun x -> (get_type_param_name x).txt))
                t)

    let type_name_module_definition ~loc ~path ~type_name ~params_names =
      let name = type_name_module_name ~type_name in
      let type_arity = List.length params_names in
      let make =
        pmod_ident ~loc @@ Located.lident ~loc @@
        "Typerep_lib.Std.Make_typename.Make" ^ string_of_int type_arity
      in
      let type_name_struct =
        str_item_type_and_name ~loc ~path ~params_names ~type_name
      in
      let type_name_module = pmod_apply ~loc make type_name_struct in
      let module_def =
        pstr_module ~loc @@ module_binding ~loc ~name:(Located.mk ~loc name)
                              ~expr:type_name_module
      in
      let typename_of_t =
        let lid = "typename_of_" ^ type_name in
        pstr_value ~loc Nonrecursive [ value_binding ~loc ~pat:(pvar ~loc lid)
                                         ~expr:(evar ~loc (name ^ ".typename_of_t"))
                                     ]
      in
      [ module_def
      ; typename_of_t
      ]

    let typerep_abstract ~loc ~path ~type_name ~params_names =
      let type_name_struct =
        str_item_type_and_name ~loc ~path ~params_names ~type_name
      in
      let type_arity = List.length params_names in
      let make =
        pmod_ident ~loc @@ Located.lident ~loc @@
        "Typerep_lib.Std.Type_abstract.Make" ^ string_of_int type_arity
      in
      pstr_include ~loc @@ include_infos ~loc @@ pmod_apply ~loc make type_name_struct

    let field_or_tag_n_ident prefix ~list n =
      if n < 0 || n > List.length list then assert false;
      prefix ^ string_of_int n

    module Record = struct
      let field_n_ident ~fields:list = field_or_tag_n_ident "field" ~list

      let fields ~loc ~typerep_of_type ~fields =
          let map { Field_case.ctyp ; label ; index; mutable_flag } =
            let rep = typerep_of_type ctyp in
            let is_mutable =
              match mutable_flag with
              | Mutable   -> true
              | Immutable -> false
            in
            index, label, [%expr
             Typerep_lib.Std.Typerep.Field.internal_use_only
               { Typerep_lib.Std.Typerep.Field_internal.
                 label      = [%e estring ~loc label];
                 index      = [%e eint    ~loc index];
                 is_mutable = [%e ebool   ~loc is_mutable];
                 rep        = [%e rep];
                 tyid       = Typerep_lib.Std.Typename.create ();
                 get        = (fun t -> [%e pexp_field ~loc (evar ~loc "t")
                                              (Located.lident ~loc label)]);
               }
            ]
          in
          List.map ~f:map fields

      let has_double_array_tag ~loc ~fields =
          let fields_binding =
            let map { Field_case.label ; _ } =
              (* The value must be a float else this segfaults.  This is tested by the
                 unit tests in case this property changes. *)
              (Located.lident ~loc label,
               [%expr Typerep_lib.Std.Typerep_obj.double_array_value ])
            in
            List.map ~f:map fields
          in
          [%expr Typerep_lib.Std.Typerep_obj.has_double_array_tag
                   [%e pexp_record ~loc fields_binding None ] ]

      let create ~loc ~fields =
        let record =
          (* Calling [get] on the fields from left to right matters, so that iteration
             goes left to right too. *)
          let fields_binding =
            let map { Field_case.label ; _ } = (Located.lident ~loc label, evar ~loc label) in
            List.map ~f:map fields
          in
          let record = pexp_record ~loc fields_binding None in
          let foldi index' { Field_case.label ; index; _ } acc =
            if index <> index' then assert false;
            let rhs = [%expr get [%e evar ~loc @@ field_n_ident ~fields index] ] in
            [%expr let [%p pvar ~loc label] = [%e rhs] in [%e acc] ]
          in
          List.fold_righti fields ~f:foldi ~init:record
        in
        [%expr fun { Typerep_lib.Std.Typerep.Record_internal.get = get } -> [%e record] ]
    end

    module Variant = struct
      (* tag_0, tag_1, etc. *)
      let tag_n_ident ~variants:list = field_or_tag_n_ident "tag" ~list

      let polymorphic ~loc ~variants =
          let polymorphic =
            match variants with
            | []      -> true
            | hd :: _ -> hd.Variant_case.poly
          in
          [%expr  [%e ebool ~loc polymorphic] ]

      let tags ~loc ~typerep_of_type ~variants =
        let create ({ Variant_case.arity ; _ } as variant) =
          if arity = 0
          then
            [%expr Typerep_lib.Std.Typerep.Tag_internal.Const
                     [%e Variant_case.expr ~loc variant None] ]
          else
            let arg_tuple i = "v" ^ string_of_int i in
            let patt, expr =
              let patt =
                let f i = pvar ~loc @@ arg_tuple i in
                ppat_tuple ~loc (List.init arity ~f)
              in
              let expr =
                let f i = evar ~loc @@ arg_tuple i in
                let args = pexp_tuple ~loc (List.init arity ~f) in
                Variant_case.expr ~loc variant (Some args)
              in
              patt, expr
            in
            [%expr  Typerep_lib.Std.Typerep.Tag_internal.Args
                      (fun [%p patt] -> [%e expr]) ]
        in
        let mapi index' ({ Variant_case.ctyp ; label ; arity ; index ; _ } as variant) =
          if index <> index' then assert false;
          let rep, tyid =
            match ctyp with
            | Some ctyp ->
              typerep_of_type ctyp, [%expr Typerep_lib.Std.Typename.create () ]
            | None ->
              [%expr typerep_of_tuple0], [%expr typename_of_tuple0]
          in
          index, [%expr
            Typerep_lib.Std.Typerep.Tag.internal_use_only
              { Typerep_lib.Std.Typerep.Tag_internal.
                label       = [%e estring ~loc label];
                rep         = [%e rep];
                arity       = [%e eint ~loc arity];
                index       = [%e eint ~loc index];
                ocaml_repr  = [%e Variant_case.ocaml_repr ~loc variant];
                tyid        = [%e tyid];
                create      = [%e create variant];
              }
          ]
        in
        List.mapi ~f:mapi variants

      let value ~loc ~variants =
          let match_cases =
            let arg_tuple i = "v" ^ string_of_int i in
            let mapi index' ({ Variant_case.arity ; index ; _ } as variant) =
              if index <> index' then assert false;
              let patt, value =
                if arity = 0 then
                  (Variant_case.patt ~loc variant None, [%expr  value_tuple0])
                else
                  let patt =
                    let f i = pvar ~loc @@ arg_tuple i in
                    let args = ppat_tuple ~loc (List.init arity ~f) in
                    Variant_case.patt ~loc variant (Some args)
                  in
                  let expr =
                    let f i = evar ~loc @@ arg_tuple i in
                    pexp_tuple ~loc (List.init arity ~f)
                  in
                  patt, expr
              in
              let tag = evar ~loc @@ tag_n_ident ~variants index in
              let prod = [%expr Typerep_lib.Std.Typerep.Variant_internal.Value
                ([%e tag], [%e value]) ]
              in
              case ~guard:None ~lhs:patt ~rhs:prod
            in
            List.mapi ~f:mapi variants
          in
          pexp_function ~loc match_cases
    end
  end

  let ldot path s : Longident.t =
    match path with
    | None -> Lident s
    | Some p -> Ldot (p, s)

  let mk_abst_call loc tn path =
    pexp_ident ~loc @@ Located.mk ~loc @@ ldot path ("typerep_of_" ^ tn)

  (* Conversion of type paths *)
  let typerep_of_path_fun loc (id : Longident.t) =
    match id with
    | Lident tn    -> mk_abst_call loc tn None
    | Ldot (p, tn) -> mk_abst_call loc tn (Some p)
    | Lapply _     -> Location.raise_errorf ~loc "ppx_typerep_conv: invalid identifier"

  let rec typerep_of_type ty =
    let loc = ty.ptyp_loc in
    match ty.ptyp_desc with
    | Ptyp_constr (id, params) ->
      eapply ~loc (typerep_of_path_fun id.loc id.txt)
        (List.map params ~f:typerep_of_type)
    | Ptyp_var parm -> evar ~loc @@ Util.arg_of_param parm
    | Ptyp_variant (row_fields, _, _) ->
      typerep_of_variant loc ~type_name:None (Branches.row_fields row_fields)
    | Ptyp_tuple tuple -> typerep_of_tuple loc tuple
    | _ ->
      Location.raise_errorf ~loc "ppx_typerep: unknown type"

  and typerep_of_tuple loc tuple =
    let typereps = List.map tuple ~f:typerep_of_type in
    match typereps with | [typerep] -> typerep | _ ->
    let typerep_of_tuple =
      let len = List.length typereps in
      if len < 2 || len > 5
      then
        Location.raise_errorf ~loc
          "ppx_type_conv: unsupported tuple arity %d. must be in {2,3,4,5}" len
      else
        evar ~loc @@ "typerep_of_tuple" ^ string_of_int len
    in
    eapply ~loc typerep_of_tuple typereps


  and typerep_of_record loc ~type_name lds =
    let fields = Branches.fields lds in
    let field_ident i = Util.Record.field_n_ident ~fields i in
    let indexed_fields = Util.Record.fields ~loc ~typerep_of_type ~fields in
    let fields_array =
      let fields =
        List.map ~f:(fun (index,_,_) ->
          [%expr Typerep_lib.Std.Typerep.Record_internal.Field
                   [%e evar ~loc @@ field_ident index] ]
        ) indexed_fields
      in
      pexp_array ~loc fields
    in
    let bindings = [
      "typename", Util.typename_field ~loc ~type_name:(Some type_name);
      "has_double_array_tag", Util.Record.has_double_array_tag ~loc ~fields;
      "fields", fields_array;
      "create", Util.Record.create ~loc ~fields;
    ] in
    let fields_binding =
      let map (name, _) =
        (Located.lident ~loc ("Typerep_lib.Std.Typerep.Record_internal." ^ name),
         evar ~loc name)
      in
      List.map ~f:map bindings
    in
    let record =
      let fields =
        [%expr  Typerep_lib.Std.Typerep.Record.internal_use_only
                  [%e pexp_record ~loc fields_binding None] ]
      in
      [%expr Typerep_lib.Std.Typerep.Record [%e fields] ]
    in
    let record = Gen.let_in loc bindings record in
    let record = List.fold_right indexed_fields ~f:(fun (index, _, expr) acc ->
      [%expr let [%p pvar ~loc @@ field_ident index] = [%e expr] in [%e acc] ]
    ) ~init:record
    in
    record

  and typerep_of_variant loc ~type_name variants =
    let tags = Util.Variant.tags ~loc ~typerep_of_type ~variants in
    let tag_ident i = Util.Variant.tag_n_ident ~variants i in
    let tags_array =
      let tags =
        List.map ~f:(fun (index,_) ->
          [%expr Typerep_lib.Std.Typerep.Variant_internal.Tag
                   [%e evar ~loc @@ tag_ident index] ]
        ) tags
      in
      pexp_array ~loc tags
    in
    let bindings = [
      "typename", Util.typename_field ~loc ~type_name;
      "tags", tags_array;
      "polymorphic", Util.Variant.polymorphic ~loc ~variants;
      "value", Util.Variant.value ~loc ~variants;
    ] in
    let tags_binding =
      let map (name, _) =
        (Located.lident ~loc ("Typerep_lib.Std.Typerep.Variant_internal." ^ name),
         evar ~loc name)
      in
      List.map ~f:map bindings
    in
    let variant =
      let tags =
        [%expr Typerep_lib.Std.Typerep.Variant.internal_use_only
                 [%e pexp_record ~loc tags_binding None]
        ]
      in
      [%expr Typerep_lib.Std.Typerep.Variant [%e tags] ]
    in
    let variant = Gen.let_in loc bindings variant in
    let variant = List.fold_right tags ~f:(fun (index, expr) acc ->
      [%expr let [%p pvar ~loc @@ tag_ident index] = [%e expr] in [%e acc] ]
    ) ~init:variant
    in
    variant

  let impl_of_one_def ~loc:_ ~path td =
    let loc = td.ptype_loc in
    let type_name = td.ptype_name.txt in
    let body =
      match td.ptype_kind with
      | Ptype_variant cds ->
        typerep_of_variant loc ~type_name:(Some type_name) (Branches.constructors cds)
      | Ptype_record lds ->
        typerep_of_record loc ~type_name lds
      | Ptype_open ->
        Location.raise_errorf ~loc "ppx_typerep_conv: open types are not supported"
      | Ptype_abstract ->
        match td.ptype_manifest with
        | None ->
          Location.raise_errorf ~loc
            "typerep cannot be applied on abstract types, except \
             like 'type t [@@deriving typerep ~abstract]'"
        | Some ty ->
          match ty.ptyp_desc with
          | Ptyp_variant (row_fields, _, _) ->
            typerep_of_variant loc ~type_name:(Some type_name)
              (Branches.row_fields row_fields)
          | _ ->
            typerep_of_type ty
    in
    let params = td.ptype_params in
    let params_names = Util.params_names ~params in
    let params_patts = Util.params_patts ~loc ~params_names in
    let body = Util.with_named ~loc ~type_name ~params_names body in
    let arguments = List.map2 params_names params_patts ~f:(fun name patt ->
      (* Add type annotations to parameters, at least to avoid the unused type warning. *)
      let loc = patt.ppat_loc in
      [%pat?  ([%p patt] : [%t ptyp_constr ~loc (Located.lident ~loc name) []]
                             Typerep_lib.Std.Typerep.t) ])
    in
    let body = eabstract ~loc arguments body in
    let body = List.fold_right params_names ~init:body ~f:(fun name acc ->
      pexp_newtype ~loc name acc)
    in
    let bnd = pvar ~loc @@ "typerep_of_" ^ type_name in
    let bnd =
      match Util.typerep_of_t_coerce td with
      | Some coerce -> ppat_constraint ~loc bnd coerce
      | None -> bnd
    in
    let binding = value_binding ~loc ~pat:bnd ~expr:body in
    Util.type_name_module_definition ~loc ~path ~type_name ~params_names, binding

  module List = struct
    include List
    (* to avoid gensym diffs with camlp4 *)
    let map_right_to_left xs ~f = rev xs |> map ~f |> rev
  end

  let with_typerep ~loc ~path (rec_flag, tds) =
    let rec_flag = really_recursive rec_flag tds in
    let prelude, bindings =
      List.split (List.map_right_to_left tds
                    ~f:(impl_of_one_def ~loc ~path))
    in
    List.flatten prelude @ [ pstr_value ~loc rec_flag bindings ]

  let with_typerep_abstract ~loc:_ ~path (_rec_flag, tds) =
    List.map tds ~f:(fun td ->
      let loc = td.ptype_loc in
      let type_name = td.ptype_name.txt in
      let params = td.ptype_params in
      let params_names = Util.params_names ~params in
      Util.typerep_abstract ~loc ~path ~type_name ~params_names)

  let gen =
    Type_conv.Generator.make
      Type_conv.Args.(empty
                      +> flag "abstract")
      (fun ~loc ~path x abstract ->
         if abstract
         then with_typerep_abstract ~loc ~path x
         else with_typerep          ~loc ~path x
      )

  let typerep_of_extension ~loc:_ ~path:_ ctyp = typerep_of_type ctyp
end

let () =
  Type_conv.add "typerep"
    ~sig_type_decl:Typerep_signature.gen
    ~str_type_decl:Typerep_implementation.gen
  |> Type_conv.ignore;
  Type_conv.add "typerep_of"
    ~extension:Typerep_implementation.typerep_of_extension
  |> Type_conv.ignore;
;;
