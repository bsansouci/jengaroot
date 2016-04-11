open Parsetree
class ['ctx] t =
  object (self)
    method attribute: 'ctx -> attribute -> attribute =
      fun ctx  ->
        fun (a,b)  ->
          let a = (self#loc self#string) ctx a in
          let b = self#payload ctx b in (a, b)
    method attributes: 'ctx -> attributes -> attributes =
      self#list self#attribute
    method bool: 'ctx -> bool -> bool =
      fun _ctx  -> fun x  -> match x with | false  -> false | true  -> true
    method case: 'ctx -> case -> case =
      fun ctx  ->
        fun { pc_lhs; pc_guard; pc_rhs }  ->
          let pc_lhs = self#pattern ctx pc_lhs in
          let pc_guard = (self#option self#expression) ctx pc_guard in
          let pc_rhs = self#expression ctx pc_rhs in
          { pc_lhs; pc_guard; pc_rhs }
    method char: 'ctx -> char -> char = fun _ctx  -> fun x  -> x
    method class_declaration: 'ctx -> class_declaration -> class_declaration
      = self#class_infos self#class_expr
    method class_description: 'ctx -> class_description -> class_description
      = self#class_infos self#class_type
    method class_expr: 'ctx -> class_expr -> class_expr =
      fun ctx  ->
        fun { pcl_desc; pcl_loc; pcl_attributes }  ->
          let pcl_desc = self#class_expr_desc ctx pcl_desc in
          let pcl_loc = self#location ctx pcl_loc in
          let pcl_attributes = self#attributes ctx pcl_attributes in
          { pcl_desc; pcl_loc; pcl_attributes }
    method class_expr_desc: 'ctx -> class_expr_desc -> class_expr_desc =
      fun ctx  ->
        fun x  ->
          match x with
          | Pcl_constr (a,b) ->
              let a = (self#loc self#longident) ctx a in
              let b = (self#list self#core_type) ctx b in Pcl_constr (a, b)
          | Pcl_structure (a) ->
              let a = self#class_structure ctx a in Pcl_structure (a)
          | Pcl_fun (a,b,c,d) ->
              let a = self#label ctx a in
              let b = (self#option self#expression) ctx b in
              let c = self#pattern ctx c in
              let d = self#class_expr ctx d in Pcl_fun (a, b, c, d)
          | Pcl_apply (a,b) ->
              let a = self#class_expr ctx a in
              let b =
                (self#list
                   (fun ctx  ->
                      fun (a,b)  ->
                        let a = self#label ctx a in
                        let b = self#expression ctx b in (a, b))) ctx b in
              Pcl_apply (a, b)
          | Pcl_let (a,b,c) ->
              let a = self#rec_flag ctx a in
              let b = (self#list self#value_binding) ctx b in
              let c = self#class_expr ctx c in Pcl_let (a, b, c)
          | Pcl_constraint (a,b) ->
              let a = self#class_expr ctx a in
              let b = self#class_type ctx b in Pcl_constraint (a, b)
          | Pcl_extension (a) ->
              let a = self#extension ctx a in Pcl_extension (a)
    method class_field: 'ctx -> class_field -> class_field =
      fun ctx  ->
        fun { pcf_desc; pcf_loc; pcf_attributes }  ->
          let pcf_desc = self#class_field_desc ctx pcf_desc in
          let pcf_loc = self#location ctx pcf_loc in
          let pcf_attributes = self#attributes ctx pcf_attributes in
          { pcf_desc; pcf_loc; pcf_attributes }
    method class_field_desc: 'ctx -> class_field_desc -> class_field_desc =
      fun ctx  ->
        fun x  ->
          match x with
          | Pcf_inherit (a,b,c) ->
              let a = self#override_flag ctx a in
              let b = self#class_expr ctx b in
              let c = (self#option self#string) ctx c in
              Pcf_inherit (a, b, c)
          | Pcf_val (a) ->
              let a =
                (fun ctx  ->
                   fun (a,b,c)  ->
                     let a = (self#loc self#string) ctx a in
                     let b = self#mutable_flag ctx b in
                     let c = self#class_field_kind ctx c in (a, b, c)) ctx a in
              Pcf_val (a)
          | Pcf_method (a) ->
              let a =
                (fun ctx  ->
                   fun (a,b,c)  ->
                     let a = (self#loc self#string) ctx a in
                     let b = self#private_flag ctx b in
                     let c = self#class_field_kind ctx c in (a, b, c)) ctx a in
              Pcf_method (a)
          | Pcf_constraint (a) ->
              let a =
                (fun ctx  ->
                   fun (a,b)  ->
                     let a = self#core_type ctx a in
                     let b = self#core_type ctx b in (a, b)) ctx a in
              Pcf_constraint (a)
          | Pcf_initializer (a) ->
              let a = self#expression ctx a in Pcf_initializer (a)
          | Pcf_attribute (a) ->
              let a = self#attribute ctx a in Pcf_attribute (a)
          | Pcf_extension (a) ->
              let a = self#extension ctx a in Pcf_extension (a)
    method class_field_kind: 'ctx -> class_field_kind -> class_field_kind =
      fun ctx  ->
        fun x  ->
          match x with
          | Cfk_virtual (a) ->
              let a = self#core_type ctx a in Cfk_virtual (a)
          | Cfk_concrete (a,b) ->
              let a = self#override_flag ctx a in
              let b = self#expression ctx b in Cfk_concrete (a, b)
    method class_infos:
      'a . ('ctx -> 'a -> 'a) -> 'ctx -> 'a class_infos -> 'a class_infos =
      fun map_a  ->
        fun ctx  ->
          fun
            { pci_virt; pci_params; pci_name; pci_expr; pci_loc;
              pci_attributes }
             ->
            let pci_virt = self#virtual_flag ctx pci_virt in
            let pci_params =
              (self#list
                 (fun ctx  ->
                    fun (a,b)  ->
                      let a = self#core_type ctx a in
                      let b = self#variance ctx b in (a, b))) ctx pci_params in
            let pci_name = (self#loc self#string) ctx pci_name in
            let pci_expr = map_a ctx pci_expr in
            let pci_loc = self#location ctx pci_loc in
            let pci_attributes = self#attributes ctx pci_attributes in
            {
              pci_virt;
              pci_params;
              pci_name;
              pci_expr;
              pci_loc;
              pci_attributes
            }
    method class_signature: 'ctx -> class_signature -> class_signature =
      fun ctx  ->
        fun { pcsig_self; pcsig_fields }  ->
          let pcsig_self = self#core_type ctx pcsig_self in
          let pcsig_fields =
            (self#list self#class_type_field) ctx pcsig_fields in
          { pcsig_self; pcsig_fields }
    method class_structure: 'ctx -> class_structure -> class_structure =
      fun ctx  ->
        fun { pcstr_self; pcstr_fields }  ->
          let pcstr_self = self#pattern ctx pcstr_self in
          let pcstr_fields = (self#list self#class_field) ctx pcstr_fields in
          { pcstr_self; pcstr_fields }
    method class_type: 'ctx -> class_type -> class_type =
      fun ctx  ->
        fun { pcty_desc; pcty_loc; pcty_attributes }  ->
          let pcty_desc = self#class_type_desc ctx pcty_desc in
          let pcty_loc = self#location ctx pcty_loc in
          let pcty_attributes = self#attributes ctx pcty_attributes in
          { pcty_desc; pcty_loc; pcty_attributes }
    method class_type_declaration:
      'ctx -> class_type_declaration -> class_type_declaration =
      self#class_infos self#class_type
    method class_type_desc: 'ctx -> class_type_desc -> class_type_desc =
      fun ctx  ->
        fun x  ->
          match x with
          | Pcty_constr (a,b) ->
              let a = (self#loc self#longident) ctx a in
              let b = (self#list self#core_type) ctx b in Pcty_constr (a, b)
          | Pcty_signature (a) ->
              let a = self#class_signature ctx a in Pcty_signature (a)
          | Pcty_arrow (a,b,c) ->
              let a = self#label ctx a in
              let b = self#core_type ctx b in
              let c = self#class_type ctx c in Pcty_arrow (a, b, c)
          | Pcty_extension (a) ->
              let a = self#extension ctx a in Pcty_extension (a)
    method class_type_field: 'ctx -> class_type_field -> class_type_field =
      fun ctx  ->
        fun { pctf_desc; pctf_loc; pctf_attributes }  ->
          let pctf_desc = self#class_type_field_desc ctx pctf_desc in
          let pctf_loc = self#location ctx pctf_loc in
          let pctf_attributes = self#attributes ctx pctf_attributes in
          { pctf_desc; pctf_loc; pctf_attributes }
    method class_type_field_desc:
      'ctx -> class_type_field_desc -> class_type_field_desc =
      fun ctx  ->
        fun x  ->
          match x with
          | Pctf_inherit (a) ->
              let a = self#class_type ctx a in Pctf_inherit (a)
          | Pctf_val (a) ->
              let a =
                (fun ctx  ->
                   fun (a,b,c,d)  ->
                     let a = self#string ctx a in
                     let b = self#mutable_flag ctx b in
                     let c = self#virtual_flag ctx c in
                     let d = self#core_type ctx d in (a, b, c, d)) ctx a in
              Pctf_val (a)
          | Pctf_method (a) ->
              let a =
                (fun ctx  ->
                   fun (a,b,c,d)  ->
                     let a = self#string ctx a in
                     let b = self#private_flag ctx b in
                     let c = self#virtual_flag ctx c in
                     let d = self#core_type ctx d in (a, b, c, d)) ctx a in
              Pctf_method (a)
          | Pctf_constraint (a) ->
              let a =
                (fun ctx  ->
                   fun (a,b)  ->
                     let a = self#core_type ctx a in
                     let b = self#core_type ctx b in (a, b)) ctx a in
              Pctf_constraint (a)
          | Pctf_attribute (a) ->
              let a = self#attribute ctx a in Pctf_attribute (a)
          | Pctf_extension (a) ->
              let a = self#extension ctx a in Pctf_extension (a)
    method closed_flag: 'ctx -> Asttypes.closed_flag -> Asttypes.closed_flag
      =
      fun _ctx  -> fun x  -> match x with | Closed  -> Closed | Open  -> Open
    method constant: 'ctx -> Asttypes.constant -> Asttypes.constant =
      fun ctx  ->
        fun x  ->
          match x with
          | Const_int (a) -> let a = self#int ctx a in Const_int (a)
          | Const_char (a) -> let a = self#char ctx a in Const_char (a)
          | Const_string (a,b) ->
              let a = self#string ctx a in
              let b = (self#option self#string) ctx b in Const_string (a, b)
          | Const_float (a) -> let a = self#string ctx a in Const_float (a)
          | Const_int32 (a) -> let a = self#int32 ctx a in Const_int32 (a)
          | Const_int64 (a) -> let a = self#int64 ctx a in Const_int64 (a)
          | Const_nativeint (a) ->
              let a = self#nativeint ctx a in Const_nativeint (a)
    method constructor_declaration:
      'ctx -> constructor_declaration -> constructor_declaration =
      fun ctx  ->
        fun { pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes }  ->
          let pcd_name = (self#loc self#string) ctx pcd_name in
          let pcd_args = (self#list self#core_type) ctx pcd_args in
          let pcd_res = (self#option self#core_type) ctx pcd_res in
          let pcd_loc = self#location ctx pcd_loc in
          let pcd_attributes = self#attributes ctx pcd_attributes in
          { pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes }
    method core_type: 'ctx -> core_type -> core_type =
      fun ctx  ->
        fun { ptyp_desc; ptyp_loc; ptyp_attributes }  ->
          let ptyp_desc = self#core_type_desc ctx ptyp_desc in
          let ptyp_loc = self#location ctx ptyp_loc in
          let ptyp_attributes = self#attributes ctx ptyp_attributes in
          { ptyp_desc; ptyp_loc; ptyp_attributes }
    method core_type_desc: 'ctx -> core_type_desc -> core_type_desc =
      fun ctx  ->
        fun x  ->
          match x with
          | Ptyp_any  -> Ptyp_any
          | Ptyp_var (a) -> let a = self#string ctx a in Ptyp_var (a)
          | Ptyp_arrow (a,b,c) ->
              let a = self#label ctx a in
              let b = self#core_type ctx b in
              let c = self#core_type ctx c in Ptyp_arrow (a, b, c)
          | Ptyp_tuple (a) ->
              let a = (self#list self#core_type) ctx a in Ptyp_tuple (a)
          | Ptyp_constr (a,b) ->
              let a = (self#loc self#longident) ctx a in
              let b = (self#list self#core_type) ctx b in Ptyp_constr (a, b)
          | Ptyp_object (a,b) ->
              let a =
                (self#list
                   (fun ctx  ->
                      fun (a,b,c)  ->
                        let a = self#string ctx a in
                        let b = self#attributes ctx b in
                        let c = self#core_type ctx c in (a, b, c))) ctx a in
              let b = self#closed_flag ctx b in Ptyp_object (a, b)
          | Ptyp_class (a,b) ->
              let a = (self#loc self#longident) ctx a in
              let b = (self#list self#core_type) ctx b in Ptyp_class (a, b)
          | Ptyp_alias (a,b) ->
              let a = self#core_type ctx a in
              let b = self#string ctx b in Ptyp_alias (a, b)
          | Ptyp_variant (a,b,c) ->
              let a = (self#list self#row_field) ctx a in
              let b = self#closed_flag ctx b in
              let c = (self#option (self#list self#label)) ctx c in
              Ptyp_variant (a, b, c)
          | Ptyp_poly (a,b) ->
              let a = (self#list self#string) ctx a in
              let b = self#core_type ctx b in Ptyp_poly (a, b)
          | Ptyp_package (a) ->
              let a = self#package_type ctx a in Ptyp_package (a)
          | Ptyp_extension (a) ->
              let a = self#extension ctx a in Ptyp_extension (a)
    method direction_flag:
      'ctx -> Asttypes.direction_flag -> Asttypes.direction_flag =
      fun _ctx  -> fun x  -> match x with | Upto  -> Upto | Downto  -> Downto
    method directive_argument:
      'ctx -> directive_argument -> directive_argument =
      fun ctx  ->
        fun x  ->
          match x with
          | Pdir_none  -> Pdir_none
          | Pdir_string (a) -> let a = self#string ctx a in Pdir_string (a)
          | Pdir_int (a) -> let a = self#int ctx a in Pdir_int (a)
          | Pdir_ident (a) -> let a = self#longident ctx a in Pdir_ident (a)
          | Pdir_bool (a) -> let a = self#bool ctx a in Pdir_bool (a)
    method expression: 'ctx -> expression -> expression =
      fun ctx  ->
        fun { pexp_desc; pexp_loc; pexp_attributes }  ->
          let pexp_desc = self#expression_desc ctx pexp_desc in
          let pexp_loc = self#location ctx pexp_loc in
          let pexp_attributes = self#attributes ctx pexp_attributes in
          { pexp_desc; pexp_loc; pexp_attributes }
    method expression_desc: 'ctx -> expression_desc -> expression_desc =
      fun ctx  ->
        fun x  ->
          match x with
          | Pexp_ident (a) ->
              let a = (self#loc self#longident) ctx a in Pexp_ident (a)
          | Pexp_constant (a) ->
              let a = self#constant ctx a in Pexp_constant (a)
          | Pexp_let (a,b,c) ->
              let a = self#rec_flag ctx a in
              let b = (self#list self#value_binding) ctx b in
              let c = self#expression ctx c in Pexp_let (a, b, c)
          | Pexp_function (a) ->
              let a = (self#list self#case) ctx a in Pexp_function (a)
          | Pexp_fun (a,b,c,d) ->
              let a = self#label ctx a in
              let b = (self#option self#expression) ctx b in
              let c = self#pattern ctx c in
              let d = self#expression ctx d in Pexp_fun (a, b, c, d)
          | Pexp_apply (a,b) ->
              let a = self#expression ctx a in
              let b =
                (self#list
                   (fun ctx  ->
                      fun (a,b)  ->
                        let a = self#label ctx a in
                        let b = self#expression ctx b in (a, b))) ctx b in
              Pexp_apply (a, b)
          | Pexp_match (a,b) ->
              let a = self#expression ctx a in
              let b = (self#list self#case) ctx b in Pexp_match (a, b)
          | Pexp_try (a,b) ->
              let a = self#expression ctx a in
              let b = (self#list self#case) ctx b in Pexp_try (a, b)
          | Pexp_tuple (a) ->
              let a = (self#list self#expression) ctx a in Pexp_tuple (a)
          | Pexp_construct (a,b) ->
              let a = (self#loc self#longident) ctx a in
              let b = (self#option self#expression) ctx b in
              Pexp_construct (a, b)
          | Pexp_variant (a,b) ->
              let a = self#label ctx a in
              let b = (self#option self#expression) ctx b in
              Pexp_variant (a, b)
          | Pexp_record (a,b) ->
              let a =
                (self#list
                   (fun ctx  ->
                      fun (a,b)  ->
                        let a = (self#loc self#longident) ctx a in
                        let b = self#expression ctx b in (a, b))) ctx a in
              let b = (self#option self#expression) ctx b in
              Pexp_record (a, b)
          | Pexp_field (a,b) ->
              let a = self#expression ctx a in
              let b = (self#loc self#longident) ctx b in Pexp_field (a, b)
          | Pexp_setfield (a,b,c) ->
              let a = self#expression ctx a in
              let b = (self#loc self#longident) ctx b in
              let c = self#expression ctx c in Pexp_setfield (a, b, c)
          | Pexp_array (a) ->
              let a = (self#list self#expression) ctx a in Pexp_array (a)
          | Pexp_ifthenelse (a,b,c) ->
              let a = self#expression ctx a in
              let b = self#expression ctx b in
              let c = (self#option self#expression) ctx c in
              Pexp_ifthenelse (a, b, c)
          | Pexp_sequence (a,b) ->
              let a = self#expression ctx a in
              let b = self#expression ctx b in Pexp_sequence (a, b)
          | Pexp_while (a,b) ->
              let a = self#expression ctx a in
              let b = self#expression ctx b in Pexp_while (a, b)
          | Pexp_for (a,b,c,d,e) ->
              let a = self#pattern ctx a in
              let b = self#expression ctx b in
              let c = self#expression ctx c in
              let d = self#direction_flag ctx d in
              let e = self#expression ctx e in Pexp_for (a, b, c, d, e)
          | Pexp_constraint (a,b) ->
              let a = self#expression ctx a in
              let b = self#core_type ctx b in Pexp_constraint (a, b)
          | Pexp_coerce (a,b,c) ->
              let a = self#expression ctx a in
              let b = (self#option self#core_type) ctx b in
              let c = self#core_type ctx c in Pexp_coerce (a, b, c)
          | Pexp_send (a,b) ->
              let a = self#expression ctx a in
              let b = self#string ctx b in Pexp_send (a, b)
          | Pexp_new (a) ->
              let a = (self#loc self#longident) ctx a in Pexp_new (a)
          | Pexp_setinstvar (a,b) ->
              let a = (self#loc self#string) ctx a in
              let b = self#expression ctx b in Pexp_setinstvar (a, b)
          | Pexp_override (a) ->
              let a =
                (self#list
                   (fun ctx  ->
                      fun (a,b)  ->
                        let a = (self#loc self#string) ctx a in
                        let b = self#expression ctx b in (a, b))) ctx a in
              Pexp_override (a)
          | Pexp_letmodule (a,b,c) ->
              let a = (self#loc self#string) ctx a in
              let b = self#module_expr ctx b in
              let c = self#expression ctx c in Pexp_letmodule (a, b, c)
          | Pexp_assert (a) ->
              let a = self#expression ctx a in Pexp_assert (a)
          | Pexp_lazy (a) -> let a = self#expression ctx a in Pexp_lazy (a)
          | Pexp_poly (a,b) ->
              let a = self#expression ctx a in
              let b = (self#option self#core_type) ctx b in Pexp_poly (a, b)
          | Pexp_object (a) ->
              let a = self#class_structure ctx a in Pexp_object (a)
          | Pexp_newtype (a,b) ->
              let a = self#string ctx a in
              let b = self#expression ctx b in Pexp_newtype (a, b)
          | Pexp_pack (a) -> let a = self#module_expr ctx a in Pexp_pack (a)
          | Pexp_open (a,b,c) ->
              let a = self#override_flag ctx a in
              let b = (self#loc self#longident) ctx b in
              let c = self#expression ctx c in Pexp_open (a, b, c)
          | Pexp_extension (a) ->
              let a = self#extension ctx a in Pexp_extension (a)
    method extension: 'ctx -> extension -> extension =
      fun ctx  ->
        fun (a,b)  ->
          let a = (self#loc self#string) ctx a in
          let b = self#payload ctx b in (a, b)
    method extension_constructor:
      'ctx -> extension_constructor -> extension_constructor =
      fun ctx  ->
        fun { pext_name; pext_kind; pext_loc; pext_attributes }  ->
          let pext_name = (self#loc self#string) ctx pext_name in
          let pext_kind = self#extension_constructor_kind ctx pext_kind in
          let pext_loc = self#location ctx pext_loc in
          let pext_attributes = self#attributes ctx pext_attributes in
          { pext_name; pext_kind; pext_loc; pext_attributes }
    method extension_constructor_kind:
      'ctx -> extension_constructor_kind -> extension_constructor_kind =
      fun ctx  ->
        fun x  ->
          match x with
          | Pext_decl (a,b) ->
              let a = (self#list self#core_type) ctx a in
              let b = (self#option self#core_type) ctx b in Pext_decl (a, b)
          | Pext_rebind (a) ->
              let a = (self#loc self#longident) ctx a in Pext_rebind (a)
    method include_declaration:
      'ctx -> include_declaration -> include_declaration =
      self#include_infos self#module_expr
    method include_description:
      'ctx -> include_description -> include_description =
      self#include_infos self#module_type
    method include_infos:
      'a . ('ctx -> 'a -> 'a) -> 'ctx -> 'a include_infos -> 'a include_infos
      =
      fun map_a  ->
        fun ctx  ->
          fun { pincl_mod; pincl_loc; pincl_attributes }  ->
            let pincl_mod = map_a ctx pincl_mod in
            let pincl_loc = self#location ctx pincl_loc in
            let pincl_attributes = self#attributes ctx pincl_attributes in
            { pincl_mod; pincl_loc; pincl_attributes }
    method int: 'ctx -> int -> int = fun _ctx  -> fun x  -> x
    method int32: 'ctx -> int32 -> int32 = fun _ctx  -> fun x  -> x
    method int64: 'ctx -> int64 -> int64 = fun _ctx  -> fun x  -> x
    method label: 'ctx -> Asttypes.label -> Asttypes.label = self#string
    method label_declaration: 'ctx -> label_declaration -> label_declaration
      =
      fun ctx  ->
        fun { pld_name; pld_mutable; pld_type; pld_loc; pld_attributes }  ->
          let pld_name = (self#loc self#string) ctx pld_name in
          let pld_mutable = self#mutable_flag ctx pld_mutable in
          let pld_type = self#core_type ctx pld_type in
          let pld_loc = self#location ctx pld_loc in
          let pld_attributes = self#attributes ctx pld_attributes in
          { pld_name; pld_mutable; pld_type; pld_loc; pld_attributes }
    method lexing_position: 'ctx -> Lexing.position -> Lexing.position =
      fun ctx  ->
        fun { pos_fname; pos_lnum; pos_bol; pos_cnum }  ->
          let pos_fname = self#string ctx pos_fname in
          let pos_lnum = self#int ctx pos_lnum in
          let pos_bol = self#int ctx pos_bol in
          let pos_cnum = self#int ctx pos_cnum in
          { pos_fname; pos_lnum; pos_bol; pos_cnum }
    method list: 'a . ('ctx -> 'a -> 'a) -> 'ctx -> 'a list -> 'a list =
      fun map_a  ->
        fun ctx  ->
          fun x  ->
            match x with
            | [] -> []
            | a::b ->
                let a = map_a ctx a in
                let b = (self#list map_a) ctx b in a :: b
    method loc:
      'a . ('ctx -> 'a -> 'a) -> 'ctx -> 'a Asttypes.loc -> 'a Asttypes.loc =
      fun map_a  ->
        fun ctx  ->
          fun { txt; loc }  ->
            let txt = map_a ctx txt in
            let loc = self#location ctx loc in { txt; loc }
    method location: 'ctx -> Location.t -> Location.t =
      fun ctx  ->
        fun { loc_start; loc_end; loc_ghost }  ->
          let loc_start = self#lexing_position ctx loc_start in
          let loc_end = self#lexing_position ctx loc_end in
          let loc_ghost = self#bool ctx loc_ghost in
          { loc_start; loc_end; loc_ghost }
    method longident: 'ctx -> Longident.t -> Longident.t =
      fun ctx  ->
        fun x  ->
          match x with
          | Lident (a) -> let a = self#string ctx a in Lident (a)
          | Ldot (a,b) ->
              let a = self#longident ctx a in
              let b = self#string ctx b in Ldot (a, b)
          | Lapply (a,b) ->
              let a = self#longident ctx a in
              let b = self#longident ctx b in Lapply (a, b)
    method module_binding: 'ctx -> module_binding -> module_binding =
      fun ctx  ->
        fun { pmb_name; pmb_expr; pmb_attributes; pmb_loc }  ->
          let pmb_name = (self#loc self#string) ctx pmb_name in
          let pmb_expr = self#module_expr ctx pmb_expr in
          let pmb_attributes = self#attributes ctx pmb_attributes in
          let pmb_loc = self#location ctx pmb_loc in
          { pmb_name; pmb_expr; pmb_attributes; pmb_loc }
    method module_declaration:
      'ctx -> module_declaration -> module_declaration =
      fun ctx  ->
        fun { pmd_name; pmd_type; pmd_attributes; pmd_loc }  ->
          let pmd_name = (self#loc self#string) ctx pmd_name in
          let pmd_type = self#module_type ctx pmd_type in
          let pmd_attributes = self#attributes ctx pmd_attributes in
          let pmd_loc = self#location ctx pmd_loc in
          { pmd_name; pmd_type; pmd_attributes; pmd_loc }
    method module_expr: 'ctx -> module_expr -> module_expr =
      fun ctx  ->
        fun { pmod_desc; pmod_loc; pmod_attributes }  ->
          let pmod_desc = self#module_expr_desc ctx pmod_desc in
          let pmod_loc = self#location ctx pmod_loc in
          let pmod_attributes = self#attributes ctx pmod_attributes in
          { pmod_desc; pmod_loc; pmod_attributes }
    method module_expr_desc: 'ctx -> module_expr_desc -> module_expr_desc =
      fun ctx  ->
        fun x  ->
          match x with
          | Pmod_ident (a) ->
              let a = (self#loc self#longident) ctx a in Pmod_ident (a)
          | Pmod_structure (a) ->
              let a = self#structure ctx a in Pmod_structure (a)
          | Pmod_functor (a,b,c) ->
              let a = (self#loc self#string) ctx a in
              let b = (self#option self#module_type) ctx b in
              let c = self#module_expr ctx c in Pmod_functor (a, b, c)
          | Pmod_apply (a,b) ->
              let a = self#module_expr ctx a in
              let b = self#module_expr ctx b in Pmod_apply (a, b)
          | Pmod_constraint (a,b) ->
              let a = self#module_expr ctx a in
              let b = self#module_type ctx b in Pmod_constraint (a, b)
          | Pmod_unpack (a) ->
              let a = self#expression ctx a in Pmod_unpack (a)
          | Pmod_extension (a) ->
              let a = self#extension ctx a in Pmod_extension (a)
    method module_type: 'ctx -> module_type -> module_type =
      fun ctx  ->
        fun { pmty_desc; pmty_loc; pmty_attributes }  ->
          let pmty_desc = self#module_type_desc ctx pmty_desc in
          let pmty_loc = self#location ctx pmty_loc in
          let pmty_attributes = self#attributes ctx pmty_attributes in
          { pmty_desc; pmty_loc; pmty_attributes }
    method module_type_declaration:
      'ctx -> module_type_declaration -> module_type_declaration =
      fun ctx  ->
        fun { pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc }  ->
          let pmtd_name = (self#loc self#string) ctx pmtd_name in
          let pmtd_type = (self#option self#module_type) ctx pmtd_type in
          let pmtd_attributes = self#attributes ctx pmtd_attributes in
          let pmtd_loc = self#location ctx pmtd_loc in
          { pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc }
    method module_type_desc: 'ctx -> module_type_desc -> module_type_desc =
      fun ctx  ->
        fun x  ->
          match x with
          | Pmty_ident (a) ->
              let a = (self#loc self#longident) ctx a in Pmty_ident (a)
          | Pmty_signature (a) ->
              let a = self#signature ctx a in Pmty_signature (a)
          | Pmty_functor (a,b,c) ->
              let a = (self#loc self#string) ctx a in
              let b = (self#option self#module_type) ctx b in
              let c = self#module_type ctx c in Pmty_functor (a, b, c)
          | Pmty_with (a,b) ->
              let a = self#module_type ctx a in
              let b = (self#list self#with_constraint) ctx b in
              Pmty_with (a, b)
          | Pmty_typeof (a) ->
              let a = self#module_expr ctx a in Pmty_typeof (a)
          | Pmty_extension (a) ->
              let a = self#extension ctx a in Pmty_extension (a)
          | Pmty_alias (a) ->
              let a = (self#loc self#longident) ctx a in Pmty_alias (a)
    method mutable_flag:
      'ctx -> Asttypes.mutable_flag -> Asttypes.mutable_flag =
      fun _ctx  ->
        fun x  ->
          match x with | Immutable  -> Immutable | Mutable  -> Mutable
    method nativeint: 'ctx -> nativeint -> nativeint =
      fun _ctx  -> fun x  -> x
    method open_description: 'ctx -> open_description -> open_description =
      fun ctx  ->
        fun { popen_lid; popen_override; popen_loc; popen_attributes }  ->
          let popen_lid = (self#loc self#longident) ctx popen_lid in
          let popen_override = self#override_flag ctx popen_override in
          let popen_loc = self#location ctx popen_loc in
          let popen_attributes = self#attributes ctx popen_attributes in
          { popen_lid; popen_override; popen_loc; popen_attributes }
    method option: 'a . ('ctx -> 'a -> 'a) -> 'ctx -> 'a option -> 'a option
      =
      fun map_a  ->
        fun ctx  ->
          fun x  ->
            match x with
            | None  -> None
            | Some (a) -> let a = map_a ctx a in Some (a)
    method override_flag:
      'ctx -> Asttypes.override_flag -> Asttypes.override_flag =
      fun _ctx  ->
        fun x  -> match x with | Override  -> Override | Fresh  -> Fresh
    method package_type: 'ctx -> package_type -> package_type =
      fun ctx  ->
        fun (a,b)  ->
          let a = (self#loc self#longident) ctx a in
          let b =
            (self#list
               (fun ctx  ->
                  fun (a,b)  ->
                    let a = (self#loc self#longident) ctx a in
                    let b = self#core_type ctx b in (a, b))) ctx b in
          (a, b)
    method pattern: 'ctx -> pattern -> pattern =
      fun ctx  ->
        fun { ppat_desc; ppat_loc; ppat_attributes }  ->
          let ppat_desc = self#pattern_desc ctx ppat_desc in
          let ppat_loc = self#location ctx ppat_loc in
          let ppat_attributes = self#attributes ctx ppat_attributes in
          { ppat_desc; ppat_loc; ppat_attributes }
    method pattern_desc: 'ctx -> pattern_desc -> pattern_desc =
      fun ctx  ->
        fun x  ->
          match x with
          | Ppat_any  -> Ppat_any
          | Ppat_var (a) ->
              let a = (self#loc self#string) ctx a in Ppat_var (a)
          | Ppat_alias (a,b) ->
              let a = self#pattern ctx a in
              let b = (self#loc self#string) ctx b in Ppat_alias (a, b)
          | Ppat_constant (a) ->
              let a = self#constant ctx a in Ppat_constant (a)
          | Ppat_interval (a,b) ->
              let a = self#constant ctx a in
              let b = self#constant ctx b in Ppat_interval (a, b)
          | Ppat_tuple (a) ->
              let a = (self#list self#pattern) ctx a in Ppat_tuple (a)
          | Ppat_construct (a,b) ->
              let a = (self#loc self#longident) ctx a in
              let b = (self#option self#pattern) ctx b in
              Ppat_construct (a, b)
          | Ppat_variant (a,b) ->
              let a = self#label ctx a in
              let b = (self#option self#pattern) ctx b in Ppat_variant (a, b)
          | Ppat_record (a,b) ->
              let a =
                (self#list
                   (fun ctx  ->
                      fun (a,b)  ->
                        let a = (self#loc self#longident) ctx a in
                        let b = self#pattern ctx b in (a, b))) ctx a in
              let b = self#closed_flag ctx b in Ppat_record (a, b)
          | Ppat_array (a) ->
              let a = (self#list self#pattern) ctx a in Ppat_array (a)
          | Ppat_or (a,b) ->
              let a = self#pattern ctx a in
              let b = self#pattern ctx b in Ppat_or (a, b)
          | Ppat_constraint (a,b) ->
              let a = self#pattern ctx a in
              let b = self#core_type ctx b in Ppat_constraint (a, b)
          | Ppat_type (a) ->
              let a = (self#loc self#longident) ctx a in Ppat_type (a)
          | Ppat_lazy (a) -> let a = self#pattern ctx a in Ppat_lazy (a)
          | Ppat_unpack (a) ->
              let a = (self#loc self#string) ctx a in Ppat_unpack (a)
          | Ppat_exception (a) ->
              let a = self#pattern ctx a in Ppat_exception (a)
          | Ppat_extension (a) ->
              let a = self#extension ctx a in Ppat_extension (a)
    method payload: 'ctx -> payload -> payload =
      fun ctx  ->
        fun x  ->
          match x with
          | PStr (a) -> let a = self#structure ctx a in PStr (a)
          | PTyp (a) -> let a = self#core_type ctx a in PTyp (a)
          | PPat (a,b) ->
              let a = self#pattern ctx a in
              let b = (self#option self#expression) ctx b in PPat (a, b)
    method private_flag:
      'ctx -> Asttypes.private_flag -> Asttypes.private_flag =
      fun _ctx  ->
        fun x  -> match x with | Private  -> Private | Public  -> Public
    method rec_flag: 'ctx -> Asttypes.rec_flag -> Asttypes.rec_flag =
      fun _ctx  ->
        fun x  ->
          match x with
          | Nonrecursive  -> Nonrecursive
          | Recursive  -> Recursive
    method row_field: 'ctx -> row_field -> row_field =
      fun ctx  ->
        fun x  ->
          match x with
          | Rtag (a,b,c,d) ->
              let a = self#label ctx a in
              let b = self#attributes ctx b in
              let c = self#bool ctx c in
              let d = (self#list self#core_type) ctx d in Rtag (a, b, c, d)
          | Rinherit (a) -> let a = self#core_type ctx a in Rinherit (a)
    method signature: 'ctx -> signature -> signature =
      self#list self#signature_item
    method signature_item: 'ctx -> signature_item -> signature_item =
      fun ctx  ->
        fun { psig_desc; psig_loc }  ->
          let psig_desc = self#signature_item_desc ctx psig_desc in
          let psig_loc = self#location ctx psig_loc in
          { psig_desc; psig_loc }
    method signature_item_desc:
      'ctx -> signature_item_desc -> signature_item_desc =
      fun ctx  ->
        fun x  ->
          match x with
          | Psig_value (a) ->
              let a = self#value_description ctx a in Psig_value (a)
          | Psig_type (a) ->
              let a = (self#list self#type_declaration) ctx a in
              Psig_type (a)
          | Psig_typext (a) ->
              let a = self#type_extension ctx a in Psig_typext (a)
          | Psig_exception (a) ->
              let a = self#extension_constructor ctx a in Psig_exception (a)
          | Psig_module (a) ->
              let a = self#module_declaration ctx a in Psig_module (a)
          | Psig_recmodule (a) ->
              let a = (self#list self#module_declaration) ctx a in
              Psig_recmodule (a)
          | Psig_modtype (a) ->
              let a = self#module_type_declaration ctx a in Psig_modtype (a)
          | Psig_open (a) ->
              let a = self#open_description ctx a in Psig_open (a)
          | Psig_include (a) ->
              let a = self#include_description ctx a in Psig_include (a)
          | Psig_class (a) ->
              let a = (self#list self#class_description) ctx a in
              Psig_class (a)
          | Psig_class_type (a) ->
              let a = (self#list self#class_type_declaration) ctx a in
              Psig_class_type (a)
          | Psig_attribute (a) ->
              let a = self#attribute ctx a in Psig_attribute (a)
          | Psig_extension (a,b) ->
              let a = self#extension ctx a in
              let b = self#attributes ctx b in Psig_extension (a, b)
    method string: 'ctx -> string -> string = fun _ctx  -> fun x  -> x
    method structure: 'ctx -> structure -> structure =
      self#list self#structure_item
    method structure_item: 'ctx -> structure_item -> structure_item =
      fun ctx  ->
        fun { pstr_desc; pstr_loc }  ->
          let pstr_desc = self#structure_item_desc ctx pstr_desc in
          let pstr_loc = self#location ctx pstr_loc in
          { pstr_desc; pstr_loc }
    method structure_item_desc:
      'ctx -> structure_item_desc -> structure_item_desc =
      fun ctx  ->
        fun x  ->
          match x with
          | Pstr_eval (a,b) ->
              let a = self#expression ctx a in
              let b = self#attributes ctx b in Pstr_eval (a, b)
          | Pstr_value (a,b) ->
              let a = self#rec_flag ctx a in
              let b = (self#list self#value_binding) ctx b in
              Pstr_value (a, b)
          | Pstr_primitive (a) ->
              let a = self#value_description ctx a in Pstr_primitive (a)
          | Pstr_type (a) ->
              let a = (self#list self#type_declaration) ctx a in
              Pstr_type (a)
          | Pstr_typext (a) ->
              let a = self#type_extension ctx a in Pstr_typext (a)
          | Pstr_exception (a) ->
              let a = self#extension_constructor ctx a in Pstr_exception (a)
          | Pstr_module (a) ->
              let a = self#module_binding ctx a in Pstr_module (a)
          | Pstr_recmodule (a) ->
              let a = (self#list self#module_binding) ctx a in
              Pstr_recmodule (a)
          | Pstr_modtype (a) ->
              let a = self#module_type_declaration ctx a in Pstr_modtype (a)
          | Pstr_open (a) ->
              let a = self#open_description ctx a in Pstr_open (a)
          | Pstr_class (a) ->
              let a = (self#list self#class_declaration) ctx a in
              Pstr_class (a)
          | Pstr_class_type (a) ->
              let a = (self#list self#class_type_declaration) ctx a in
              Pstr_class_type (a)
          | Pstr_include (a) ->
              let a = self#include_declaration ctx a in Pstr_include (a)
          | Pstr_attribute (a) ->
              let a = self#attribute ctx a in Pstr_attribute (a)
          | Pstr_extension (a,b) ->
              let a = self#extension ctx a in
              let b = self#attributes ctx b in Pstr_extension (a, b)
    method toplevel_phrase: 'ctx -> toplevel_phrase -> toplevel_phrase =
      fun ctx  ->
        fun x  ->
          match x with
          | Ptop_def (a) -> let a = self#structure ctx a in Ptop_def (a)
          | Ptop_dir (a,b) ->
              let a = self#string ctx a in
              let b = self#directive_argument ctx b in Ptop_dir (a, b)
    method type_declaration: 'ctx -> type_declaration -> type_declaration =
      fun ctx  ->
        fun
          { ptype_name; ptype_params; ptype_cstrs; ptype_kind; ptype_private;
            ptype_manifest; ptype_attributes; ptype_loc }
           ->
          let ptype_name = (self#loc self#string) ctx ptype_name in
          let ptype_params =
            (self#list
               (fun ctx  ->
                  fun (a,b)  ->
                    let a = self#core_type ctx a in
                    let b = self#variance ctx b in (a, b))) ctx ptype_params in
          let ptype_cstrs =
            (self#list
               (fun ctx  ->
                  fun (a,b,c)  ->
                    let a = self#core_type ctx a in
                    let b = self#core_type ctx b in
                    let c = self#location ctx c in (a, b, c))) ctx
              ptype_cstrs in
          let ptype_kind = self#type_kind ctx ptype_kind in
          let ptype_private = self#private_flag ctx ptype_private in
          let ptype_manifest =
            (self#option self#core_type) ctx ptype_manifest in
          let ptype_attributes = self#attributes ctx ptype_attributes in
          let ptype_loc = self#location ctx ptype_loc in
          {
            ptype_name;
            ptype_params;
            ptype_cstrs;
            ptype_kind;
            ptype_private;
            ptype_manifest;
            ptype_attributes;
            ptype_loc
          }
    method type_extension: 'ctx -> type_extension -> type_extension =
      fun ctx  ->
        fun
          { ptyext_path; ptyext_params; ptyext_constructors; ptyext_private;
            ptyext_attributes }
           ->
          let ptyext_path = (self#loc self#longident) ctx ptyext_path in
          let ptyext_params =
            (self#list
               (fun ctx  ->
                  fun (a,b)  ->
                    let a = self#core_type ctx a in
                    let b = self#variance ctx b in (a, b))) ctx ptyext_params in
          let ptyext_constructors =
            (self#list self#extension_constructor) ctx ptyext_constructors in
          let ptyext_private = self#private_flag ctx ptyext_private in
          let ptyext_attributes = self#attributes ctx ptyext_attributes in
          {
            ptyext_path;
            ptyext_params;
            ptyext_constructors;
            ptyext_private;
            ptyext_attributes
          }
    method type_kind: 'ctx -> type_kind -> type_kind =
      fun ctx  ->
        fun x  ->
          match x with
          | Ptype_abstract  -> Ptype_abstract
          | Ptype_variant (a) ->
              let a = (self#list self#constructor_declaration) ctx a in
              Ptype_variant (a)
          | Ptype_record (a) ->
              let a = (self#list self#label_declaration) ctx a in
              Ptype_record (a)
          | Ptype_open  -> Ptype_open
    method value_binding: 'ctx -> value_binding -> value_binding =
      fun ctx  ->
        fun { pvb_pat; pvb_expr; pvb_attributes; pvb_loc }  ->
          let pvb_pat = self#pattern ctx pvb_pat in
          let pvb_expr = self#expression ctx pvb_expr in
          let pvb_attributes = self#attributes ctx pvb_attributes in
          let pvb_loc = self#location ctx pvb_loc in
          { pvb_pat; pvb_expr; pvb_attributes; pvb_loc }
    method value_description: 'ctx -> value_description -> value_description
      =
      fun ctx  ->
        fun { pval_name; pval_type; pval_prim; pval_attributes; pval_loc } 
          ->
          let pval_name = (self#loc self#string) ctx pval_name in
          let pval_type = self#core_type ctx pval_type in
          let pval_prim = (self#list self#string) ctx pval_prim in
          let pval_attributes = self#attributes ctx pval_attributes in
          let pval_loc = self#location ctx pval_loc in
          { pval_name; pval_type; pval_prim; pval_attributes; pval_loc }
    method variance: 'ctx -> Asttypes.variance -> Asttypes.variance =
      fun _ctx  ->
        fun x  ->
          match x with
          | Covariant  -> Covariant
          | Contravariant  -> Contravariant
          | Invariant  -> Invariant
    method virtual_flag:
      'ctx -> Asttypes.virtual_flag -> Asttypes.virtual_flag =
      fun _ctx  ->
        fun x  -> match x with | Virtual  -> Virtual | Concrete  -> Concrete
    method with_constraint: 'ctx -> with_constraint -> with_constraint =
      fun ctx  ->
        fun x  ->
          match x with
          | Pwith_type (a,b) ->
              let a = (self#loc self#longident) ctx a in
              let b = self#type_declaration ctx b in Pwith_type (a, b)
          | Pwith_module (a,b) ->
              let a = (self#loc self#longident) ctx a in
              let b = (self#loc self#longident) ctx b in Pwith_module (a, b)
          | Pwith_typesubst (a) ->
              let a = self#type_declaration ctx a in Pwith_typesubst (a)
          | Pwith_modsubst (a,b) ->
              let a = (self#loc self#string) ctx a in
              let b = (self#loc self#longident) ctx b in
              Pwith_modsubst (a, b)
  end
