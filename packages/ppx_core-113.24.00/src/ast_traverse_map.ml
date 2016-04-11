open Parsetree
class t =
  object (self)
    method attribute: attribute -> attribute =
      fun (a,b)  ->
        let a = (self#loc self#string) a in let b = self#payload b in (a, b)
    method attributes: attributes -> attributes = self#list self#attribute
    method bool: bool -> bool =
      fun x  -> match x with | false  -> false | true  -> true
    method case: case -> case =
      fun { pc_lhs; pc_guard; pc_rhs }  ->
        let pc_lhs = self#pattern pc_lhs in
        let pc_guard = (self#option self#expression) pc_guard in
        let pc_rhs = self#expression pc_rhs in { pc_lhs; pc_guard; pc_rhs }
    method char: char -> char = fun x  -> x
    method class_declaration: class_declaration -> class_declaration =
      self#class_infos self#class_expr
    method class_description: class_description -> class_description =
      self#class_infos self#class_type
    method class_expr: class_expr -> class_expr =
      fun { pcl_desc; pcl_loc; pcl_attributes }  ->
        let pcl_desc = self#class_expr_desc pcl_desc in
        let pcl_loc = self#location pcl_loc in
        let pcl_attributes = self#attributes pcl_attributes in
        { pcl_desc; pcl_loc; pcl_attributes }
    method class_expr_desc: class_expr_desc -> class_expr_desc =
      fun x  ->
        match x with
        | Pcl_constr (a,b) ->
            let a = (self#loc self#longident) a in
            let b = (self#list self#core_type) b in Pcl_constr (a, b)
        | Pcl_structure (a) ->
            let a = self#class_structure a in Pcl_structure (a)
        | Pcl_fun (a,b,c,d) ->
            let a = self#label a in
            let b = (self#option self#expression) b in
            let c = self#pattern c in
            let d = self#class_expr d in Pcl_fun (a, b, c, d)
        | Pcl_apply (a,b) ->
            let a = self#class_expr a in
            let b =
              (self#list
                 (fun (a,b)  ->
                    let a = self#label a in
                    let b = self#expression b in (a, b))) b in
            Pcl_apply (a, b)
        | Pcl_let (a,b,c) ->
            let a = self#rec_flag a in
            let b = (self#list self#value_binding) b in
            let c = self#class_expr c in Pcl_let (a, b, c)
        | Pcl_constraint (a,b) ->
            let a = self#class_expr a in
            let b = self#class_type b in Pcl_constraint (a, b)
        | Pcl_extension (a) -> let a = self#extension a in Pcl_extension (a)
    method class_field: class_field -> class_field =
      fun { pcf_desc; pcf_loc; pcf_attributes }  ->
        let pcf_desc = self#class_field_desc pcf_desc in
        let pcf_loc = self#location pcf_loc in
        let pcf_attributes = self#attributes pcf_attributes in
        { pcf_desc; pcf_loc; pcf_attributes }
    method class_field_desc: class_field_desc -> class_field_desc =
      fun x  ->
        match x with
        | Pcf_inherit (a,b,c) ->
            let a = self#override_flag a in
            let b = self#class_expr b in
            let c = (self#option self#string) c in Pcf_inherit (a, b, c)
        | Pcf_val (a) ->
            let a =
              (fun (a,b,c)  ->
                 let a = (self#loc self#string) a in
                 let b = self#mutable_flag b in
                 let c = self#class_field_kind c in (a, b, c)) a in
            Pcf_val (a)
        | Pcf_method (a) ->
            let a =
              (fun (a,b,c)  ->
                 let a = (self#loc self#string) a in
                 let b = self#private_flag b in
                 let c = self#class_field_kind c in (a, b, c)) a in
            Pcf_method (a)
        | Pcf_constraint (a) ->
            let a =
              (fun (a,b)  ->
                 let a = self#core_type a in
                 let b = self#core_type b in (a, b)) a in
            Pcf_constraint (a)
        | Pcf_initializer (a) ->
            let a = self#expression a in Pcf_initializer (a)
        | Pcf_attribute (a) -> let a = self#attribute a in Pcf_attribute (a)
        | Pcf_extension (a) -> let a = self#extension a in Pcf_extension (a)
    method class_field_kind: class_field_kind -> class_field_kind =
      fun x  ->
        match x with
        | Cfk_virtual (a) -> let a = self#core_type a in Cfk_virtual (a)
        | Cfk_concrete (a,b) ->
            let a = self#override_flag a in
            let b = self#expression b in Cfk_concrete (a, b)
    method class_infos: 'a . ('a -> 'a) -> 'a class_infos -> 'a class_infos =
      fun map_a  ->
        fun
          { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes
            }
           ->
          let pci_virt = self#virtual_flag pci_virt in
          let pci_params =
            (self#list
               (fun (a,b)  ->
                  let a = self#core_type a in
                  let b = self#variance b in (a, b))) pci_params in
          let pci_name = (self#loc self#string) pci_name in
          let pci_expr = map_a pci_expr in
          let pci_loc = self#location pci_loc in
          let pci_attributes = self#attributes pci_attributes in
          { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes
          }
    method class_signature: class_signature -> class_signature =
      fun { pcsig_self; pcsig_fields }  ->
        let pcsig_self = self#core_type pcsig_self in
        let pcsig_fields = (self#list self#class_type_field) pcsig_fields in
        { pcsig_self; pcsig_fields }
    method class_structure: class_structure -> class_structure =
      fun { pcstr_self; pcstr_fields }  ->
        let pcstr_self = self#pattern pcstr_self in
        let pcstr_fields = (self#list self#class_field) pcstr_fields in
        { pcstr_self; pcstr_fields }
    method class_type: class_type -> class_type =
      fun { pcty_desc; pcty_loc; pcty_attributes }  ->
        let pcty_desc = self#class_type_desc pcty_desc in
        let pcty_loc = self#location pcty_loc in
        let pcty_attributes = self#attributes pcty_attributes in
        { pcty_desc; pcty_loc; pcty_attributes }
    method class_type_declaration:
      class_type_declaration -> class_type_declaration =
      self#class_infos self#class_type
    method class_type_desc: class_type_desc -> class_type_desc =
      fun x  ->
        match x with
        | Pcty_constr (a,b) ->
            let a = (self#loc self#longident) a in
            let b = (self#list self#core_type) b in Pcty_constr (a, b)
        | Pcty_signature (a) ->
            let a = self#class_signature a in Pcty_signature (a)
        | Pcty_arrow (a,b,c) ->
            let a = self#label a in
            let b = self#core_type b in
            let c = self#class_type c in Pcty_arrow (a, b, c)
        | Pcty_extension (a) ->
            let a = self#extension a in Pcty_extension (a)
    method class_type_field: class_type_field -> class_type_field =
      fun { pctf_desc; pctf_loc; pctf_attributes }  ->
        let pctf_desc = self#class_type_field_desc pctf_desc in
        let pctf_loc = self#location pctf_loc in
        let pctf_attributes = self#attributes pctf_attributes in
        { pctf_desc; pctf_loc; pctf_attributes }
    method class_type_field_desc:
      class_type_field_desc -> class_type_field_desc =
      fun x  ->
        match x with
        | Pctf_inherit (a) -> let a = self#class_type a in Pctf_inherit (a)
        | Pctf_val (a) ->
            let a =
              (fun (a,b,c,d)  ->
                 let a = self#string a in
                 let b = self#mutable_flag b in
                 let c = self#virtual_flag c in
                 let d = self#core_type d in (a, b, c, d)) a in
            Pctf_val (a)
        | Pctf_method (a) ->
            let a =
              (fun (a,b,c,d)  ->
                 let a = self#string a in
                 let b = self#private_flag b in
                 let c = self#virtual_flag c in
                 let d = self#core_type d in (a, b, c, d)) a in
            Pctf_method (a)
        | Pctf_constraint (a) ->
            let a =
              (fun (a,b)  ->
                 let a = self#core_type a in
                 let b = self#core_type b in (a, b)) a in
            Pctf_constraint (a)
        | Pctf_attribute (a) ->
            let a = self#attribute a in Pctf_attribute (a)
        | Pctf_extension (a) ->
            let a = self#extension a in Pctf_extension (a)
    method closed_flag: Asttypes.closed_flag -> Asttypes.closed_flag =
      fun x  -> match x with | Closed  -> Closed | Open  -> Open
    method constant: Asttypes.constant -> Asttypes.constant =
      fun x  ->
        match x with
        | Const_int (a) -> let a = self#int a in Const_int (a)
        | Const_char (a) -> let a = self#char a in Const_char (a)
        | Const_string (a,b) ->
            let a = self#string a in
            let b = (self#option self#string) b in Const_string (a, b)
        | Const_float (a) -> let a = self#string a in Const_float (a)
        | Const_int32 (a) -> let a = self#int32 a in Const_int32 (a)
        | Const_int64 (a) -> let a = self#int64 a in Const_int64 (a)
        | Const_nativeint (a) ->
            let a = self#nativeint a in Const_nativeint (a)
    method constructor_declaration:
      constructor_declaration -> constructor_declaration =
      fun { pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes }  ->
        let pcd_name = (self#loc self#string) pcd_name in
        let pcd_args = (self#list self#core_type) pcd_args in
        let pcd_res = (self#option self#core_type) pcd_res in
        let pcd_loc = self#location pcd_loc in
        let pcd_attributes = self#attributes pcd_attributes in
        { pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes }
    method core_type: core_type -> core_type =
      fun { ptyp_desc; ptyp_loc; ptyp_attributes }  ->
        let ptyp_desc = self#core_type_desc ptyp_desc in
        let ptyp_loc = self#location ptyp_loc in
        let ptyp_attributes = self#attributes ptyp_attributes in
        { ptyp_desc; ptyp_loc; ptyp_attributes }
    method core_type_desc: core_type_desc -> core_type_desc =
      fun x  ->
        match x with
        | Ptyp_any  -> Ptyp_any
        | Ptyp_var (a) -> let a = self#string a in Ptyp_var (a)
        | Ptyp_arrow (a,b,c) ->
            let a = self#label a in
            let b = self#core_type b in
            let c = self#core_type c in Ptyp_arrow (a, b, c)
        | Ptyp_tuple (a) ->
            let a = (self#list self#core_type) a in Ptyp_tuple (a)
        | Ptyp_constr (a,b) ->
            let a = (self#loc self#longident) a in
            let b = (self#list self#core_type) b in Ptyp_constr (a, b)
        | Ptyp_object (a,b) ->
            let a =
              (self#list
                 (fun (a,b,c)  ->
                    let a = self#string a in
                    let b = self#attributes b in
                    let c = self#core_type c in (a, b, c))) a in
            let b = self#closed_flag b in Ptyp_object (a, b)
        | Ptyp_class (a,b) ->
            let a = (self#loc self#longident) a in
            let b = (self#list self#core_type) b in Ptyp_class (a, b)
        | Ptyp_alias (a,b) ->
            let a = self#core_type a in
            let b = self#string b in Ptyp_alias (a, b)
        | Ptyp_variant (a,b,c) ->
            let a = (self#list self#row_field) a in
            let b = self#closed_flag b in
            let c = (self#option (self#list self#label)) c in
            Ptyp_variant (a, b, c)
        | Ptyp_poly (a,b) ->
            let a = (self#list self#string) a in
            let b = self#core_type b in Ptyp_poly (a, b)
        | Ptyp_package (a) -> let a = self#package_type a in Ptyp_package (a)
        | Ptyp_extension (a) ->
            let a = self#extension a in Ptyp_extension (a)
    method direction_flag: Asttypes.direction_flag -> Asttypes.direction_flag
      = fun x  -> match x with | Upto  -> Upto | Downto  -> Downto
    method directive_argument: directive_argument -> directive_argument =
      fun x  ->
        match x with
        | Pdir_none  -> Pdir_none
        | Pdir_string (a) -> let a = self#string a in Pdir_string (a)
        | Pdir_int (a) -> let a = self#int a in Pdir_int (a)
        | Pdir_ident (a) -> let a = self#longident a in Pdir_ident (a)
        | Pdir_bool (a) -> let a = self#bool a in Pdir_bool (a)
    method expression: expression -> expression =
      fun { pexp_desc; pexp_loc; pexp_attributes }  ->
        let pexp_desc = self#expression_desc pexp_desc in
        let pexp_loc = self#location pexp_loc in
        let pexp_attributes = self#attributes pexp_attributes in
        { pexp_desc; pexp_loc; pexp_attributes }
    method expression_desc: expression_desc -> expression_desc =
      fun x  ->
        match x with
        | Pexp_ident (a) ->
            let a = (self#loc self#longident) a in Pexp_ident (a)
        | Pexp_constant (a) -> let a = self#constant a in Pexp_constant (a)
        | Pexp_let (a,b,c) ->
            let a = self#rec_flag a in
            let b = (self#list self#value_binding) b in
            let c = self#expression c in Pexp_let (a, b, c)
        | Pexp_function (a) ->
            let a = (self#list self#case) a in Pexp_function (a)
        | Pexp_fun (a,b,c,d) ->
            let a = self#label a in
            let b = (self#option self#expression) b in
            let c = self#pattern c in
            let d = self#expression d in Pexp_fun (a, b, c, d)
        | Pexp_apply (a,b) ->
            let a = self#expression a in
            let b =
              (self#list
                 (fun (a,b)  ->
                    let a = self#label a in
                    let b = self#expression b in (a, b))) b in
            Pexp_apply (a, b)
        | Pexp_match (a,b) ->
            let a = self#expression a in
            let b = (self#list self#case) b in Pexp_match (a, b)
        | Pexp_try (a,b) ->
            let a = self#expression a in
            let b = (self#list self#case) b in Pexp_try (a, b)
        | Pexp_tuple (a) ->
            let a = (self#list self#expression) a in Pexp_tuple (a)
        | Pexp_construct (a,b) ->
            let a = (self#loc self#longident) a in
            let b = (self#option self#expression) b in Pexp_construct (a, b)
        | Pexp_variant (a,b) ->
            let a = self#label a in
            let b = (self#option self#expression) b in Pexp_variant (a, b)
        | Pexp_record (a,b) ->
            let a =
              (self#list
                 (fun (a,b)  ->
                    let a = (self#loc self#longident) a in
                    let b = self#expression b in (a, b))) a in
            let b = (self#option self#expression) b in Pexp_record (a, b)
        | Pexp_field (a,b) ->
            let a = self#expression a in
            let b = (self#loc self#longident) b in Pexp_field (a, b)
        | Pexp_setfield (a,b,c) ->
            let a = self#expression a in
            let b = (self#loc self#longident) b in
            let c = self#expression c in Pexp_setfield (a, b, c)
        | Pexp_array (a) ->
            let a = (self#list self#expression) a in Pexp_array (a)
        | Pexp_ifthenelse (a,b,c) ->
            let a = self#expression a in
            let b = self#expression b in
            let c = (self#option self#expression) c in
            Pexp_ifthenelse (a, b, c)
        | Pexp_sequence (a,b) ->
            let a = self#expression a in
            let b = self#expression b in Pexp_sequence (a, b)
        | Pexp_while (a,b) ->
            let a = self#expression a in
            let b = self#expression b in Pexp_while (a, b)
        | Pexp_for (a,b,c,d,e) ->
            let a = self#pattern a in
            let b = self#expression b in
            let c = self#expression c in
            let d = self#direction_flag d in
            let e = self#expression e in Pexp_for (a, b, c, d, e)
        | Pexp_constraint (a,b) ->
            let a = self#expression a in
            let b = self#core_type b in Pexp_constraint (a, b)
        | Pexp_coerce (a,b,c) ->
            let a = self#expression a in
            let b = (self#option self#core_type) b in
            let c = self#core_type c in Pexp_coerce (a, b, c)
        | Pexp_send (a,b) ->
            let a = self#expression a in
            let b = self#string b in Pexp_send (a, b)
        | Pexp_new (a) -> let a = (self#loc self#longident) a in Pexp_new (a)
        | Pexp_setinstvar (a,b) ->
            let a = (self#loc self#string) a in
            let b = self#expression b in Pexp_setinstvar (a, b)
        | Pexp_override (a) ->
            let a =
              (self#list
                 (fun (a,b)  ->
                    let a = (self#loc self#string) a in
                    let b = self#expression b in (a, b))) a in
            Pexp_override (a)
        | Pexp_letmodule (a,b,c) ->
            let a = (self#loc self#string) a in
            let b = self#module_expr b in
            let c = self#expression c in Pexp_letmodule (a, b, c)
        | Pexp_assert (a) -> let a = self#expression a in Pexp_assert (a)
        | Pexp_lazy (a) -> let a = self#expression a in Pexp_lazy (a)
        | Pexp_poly (a,b) ->
            let a = self#expression a in
            let b = (self#option self#core_type) b in Pexp_poly (a, b)
        | Pexp_object (a) ->
            let a = self#class_structure a in Pexp_object (a)
        | Pexp_newtype (a,b) ->
            let a = self#string a in
            let b = self#expression b in Pexp_newtype (a, b)
        | Pexp_pack (a) -> let a = self#module_expr a in Pexp_pack (a)
        | Pexp_open (a,b,c) ->
            let a = self#override_flag a in
            let b = (self#loc self#longident) b in
            let c = self#expression c in Pexp_open (a, b, c)
        | Pexp_extension (a) ->
            let a = self#extension a in Pexp_extension (a)
    method extension: extension -> extension =
      fun (a,b)  ->
        let a = (self#loc self#string) a in let b = self#payload b in (a, b)
    method extension_constructor:
      extension_constructor -> extension_constructor =
      fun { pext_name; pext_kind; pext_loc; pext_attributes }  ->
        let pext_name = (self#loc self#string) pext_name in
        let pext_kind = self#extension_constructor_kind pext_kind in
        let pext_loc = self#location pext_loc in
        let pext_attributes = self#attributes pext_attributes in
        { pext_name; pext_kind; pext_loc; pext_attributes }
    method extension_constructor_kind:
      extension_constructor_kind -> extension_constructor_kind =
      fun x  ->
        match x with
        | Pext_decl (a,b) ->
            let a = (self#list self#core_type) a in
            let b = (self#option self#core_type) b in Pext_decl (a, b)
        | Pext_rebind (a) ->
            let a = (self#loc self#longident) a in Pext_rebind (a)
    method include_declaration: include_declaration -> include_declaration =
      self#include_infos self#module_expr
    method include_description: include_description -> include_description =
      self#include_infos self#module_type
    method include_infos:
      'a . ('a -> 'a) -> 'a include_infos -> 'a include_infos =
      fun map_a  ->
        fun { pincl_mod; pincl_loc; pincl_attributes }  ->
          let pincl_mod = map_a pincl_mod in
          let pincl_loc = self#location pincl_loc in
          let pincl_attributes = self#attributes pincl_attributes in
          { pincl_mod; pincl_loc; pincl_attributes }
    method int: int -> int = fun x  -> x
    method int32: int32 -> int32 = fun x  -> x
    method int64: int64 -> int64 = fun x  -> x
    method label: Asttypes.label -> Asttypes.label = self#string
    method label_declaration: label_declaration -> label_declaration =
      fun { pld_name; pld_mutable; pld_type; pld_loc; pld_attributes }  ->
        let pld_name = (self#loc self#string) pld_name in
        let pld_mutable = self#mutable_flag pld_mutable in
        let pld_type = self#core_type pld_type in
        let pld_loc = self#location pld_loc in
        let pld_attributes = self#attributes pld_attributes in
        { pld_name; pld_mutable; pld_type; pld_loc; pld_attributes }
    method lexing_position: Lexing.position -> Lexing.position =
      fun { pos_fname; pos_lnum; pos_bol; pos_cnum }  ->
        let pos_fname = self#string pos_fname in
        let pos_lnum = self#int pos_lnum in
        let pos_bol = self#int pos_bol in
        let pos_cnum = self#int pos_cnum in
        { pos_fname; pos_lnum; pos_bol; pos_cnum }
    method list: 'a . ('a -> 'a) -> 'a list -> 'a list =
      fun map_a  ->
        fun x  ->
          match x with
          | [] -> []
          | a::b -> let a = map_a a in let b = (self#list map_a) b in a :: b
    method loc: 'a . ('a -> 'a) -> 'a Asttypes.loc -> 'a Asttypes.loc =
      fun map_a  ->
        fun { txt; loc }  ->
          let txt = map_a txt in let loc = self#location loc in { txt; loc }
    method location: Location.t -> Location.t =
      fun { loc_start; loc_end; loc_ghost }  ->
        let loc_start = self#lexing_position loc_start in
        let loc_end = self#lexing_position loc_end in
        let loc_ghost = self#bool loc_ghost in
        { loc_start; loc_end; loc_ghost }
    method longident: Longident.t -> Longident.t =
      fun x  ->
        match x with
        | Lident (a) -> let a = self#string a in Lident (a)
        | Ldot (a,b) ->
            let a = self#longident a in let b = self#string b in Ldot (a, b)
        | Lapply (a,b) ->
            let a = self#longident a in
            let b = self#longident b in Lapply (a, b)
    method module_binding: module_binding -> module_binding =
      fun { pmb_name; pmb_expr; pmb_attributes; pmb_loc }  ->
        let pmb_name = (self#loc self#string) pmb_name in
        let pmb_expr = self#module_expr pmb_expr in
        let pmb_attributes = self#attributes pmb_attributes in
        let pmb_loc = self#location pmb_loc in
        { pmb_name; pmb_expr; pmb_attributes; pmb_loc }
    method module_declaration: module_declaration -> module_declaration =
      fun { pmd_name; pmd_type; pmd_attributes; pmd_loc }  ->
        let pmd_name = (self#loc self#string) pmd_name in
        let pmd_type = self#module_type pmd_type in
        let pmd_attributes = self#attributes pmd_attributes in
        let pmd_loc = self#location pmd_loc in
        { pmd_name; pmd_type; pmd_attributes; pmd_loc }
    method module_expr: module_expr -> module_expr =
      fun { pmod_desc; pmod_loc; pmod_attributes }  ->
        let pmod_desc = self#module_expr_desc pmod_desc in
        let pmod_loc = self#location pmod_loc in
        let pmod_attributes = self#attributes pmod_attributes in
        { pmod_desc; pmod_loc; pmod_attributes }
    method module_expr_desc: module_expr_desc -> module_expr_desc =
      fun x  ->
        match x with
        | Pmod_ident (a) ->
            let a = (self#loc self#longident) a in Pmod_ident (a)
        | Pmod_structure (a) ->
            let a = self#structure a in Pmod_structure (a)
        | Pmod_functor (a,b,c) ->
            let a = (self#loc self#string) a in
            let b = (self#option self#module_type) b in
            let c = self#module_expr c in Pmod_functor (a, b, c)
        | Pmod_apply (a,b) ->
            let a = self#module_expr a in
            let b = self#module_expr b in Pmod_apply (a, b)
        | Pmod_constraint (a,b) ->
            let a = self#module_expr a in
            let b = self#module_type b in Pmod_constraint (a, b)
        | Pmod_unpack (a) -> let a = self#expression a in Pmod_unpack (a)
        | Pmod_extension (a) ->
            let a = self#extension a in Pmod_extension (a)
    method module_type: module_type -> module_type =
      fun { pmty_desc; pmty_loc; pmty_attributes }  ->
        let pmty_desc = self#module_type_desc pmty_desc in
        let pmty_loc = self#location pmty_loc in
        let pmty_attributes = self#attributes pmty_attributes in
        { pmty_desc; pmty_loc; pmty_attributes }
    method module_type_declaration:
      module_type_declaration -> module_type_declaration =
      fun { pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc }  ->
        let pmtd_name = (self#loc self#string) pmtd_name in
        let pmtd_type = (self#option self#module_type) pmtd_type in
        let pmtd_attributes = self#attributes pmtd_attributes in
        let pmtd_loc = self#location pmtd_loc in
        { pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc }
    method module_type_desc: module_type_desc -> module_type_desc =
      fun x  ->
        match x with
        | Pmty_ident (a) ->
            let a = (self#loc self#longident) a in Pmty_ident (a)
        | Pmty_signature (a) ->
            let a = self#signature a in Pmty_signature (a)
        | Pmty_functor (a,b,c) ->
            let a = (self#loc self#string) a in
            let b = (self#option self#module_type) b in
            let c = self#module_type c in Pmty_functor (a, b, c)
        | Pmty_with (a,b) ->
            let a = self#module_type a in
            let b = (self#list self#with_constraint) b in Pmty_with (a, b)
        | Pmty_typeof (a) -> let a = self#module_expr a in Pmty_typeof (a)
        | Pmty_extension (a) ->
            let a = self#extension a in Pmty_extension (a)
        | Pmty_alias (a) ->
            let a = (self#loc self#longident) a in Pmty_alias (a)
    method mutable_flag: Asttypes.mutable_flag -> Asttypes.mutable_flag =
      fun x  -> match x with | Immutable  -> Immutable | Mutable  -> Mutable
    method nativeint: nativeint -> nativeint = fun x  -> x
    method open_description: open_description -> open_description =
      fun { popen_lid; popen_override; popen_loc; popen_attributes }  ->
        let popen_lid = (self#loc self#longident) popen_lid in
        let popen_override = self#override_flag popen_override in
        let popen_loc = self#location popen_loc in
        let popen_attributes = self#attributes popen_attributes in
        { popen_lid; popen_override; popen_loc; popen_attributes }
    method option: 'a . ('a -> 'a) -> 'a option -> 'a option =
      fun map_a  ->
        fun x  ->
          match x with
          | None  -> None
          | Some (a) -> let a = map_a a in Some (a)
    method override_flag: Asttypes.override_flag -> Asttypes.override_flag =
      fun x  -> match x with | Override  -> Override | Fresh  -> Fresh
    method package_type: package_type -> package_type =
      fun (a,b)  ->
        let a = (self#loc self#longident) a in
        let b =
          (self#list
             (fun (a,b)  ->
                let a = (self#loc self#longident) a in
                let b = self#core_type b in (a, b))) b in
        (a, b)
    method pattern: pattern -> pattern =
      fun { ppat_desc; ppat_loc; ppat_attributes }  ->
        let ppat_desc = self#pattern_desc ppat_desc in
        let ppat_loc = self#location ppat_loc in
        let ppat_attributes = self#attributes ppat_attributes in
        { ppat_desc; ppat_loc; ppat_attributes }
    method pattern_desc: pattern_desc -> pattern_desc =
      fun x  ->
        match x with
        | Ppat_any  -> Ppat_any
        | Ppat_var (a) -> let a = (self#loc self#string) a in Ppat_var (a)
        | Ppat_alias (a,b) ->
            let a = self#pattern a in
            let b = (self#loc self#string) b in Ppat_alias (a, b)
        | Ppat_constant (a) -> let a = self#constant a in Ppat_constant (a)
        | Ppat_interval (a,b) ->
            let a = self#constant a in
            let b = self#constant b in Ppat_interval (a, b)
        | Ppat_tuple (a) ->
            let a = (self#list self#pattern) a in Ppat_tuple (a)
        | Ppat_construct (a,b) ->
            let a = (self#loc self#longident) a in
            let b = (self#option self#pattern) b in Ppat_construct (a, b)
        | Ppat_variant (a,b) ->
            let a = self#label a in
            let b = (self#option self#pattern) b in Ppat_variant (a, b)
        | Ppat_record (a,b) ->
            let a =
              (self#list
                 (fun (a,b)  ->
                    let a = (self#loc self#longident) a in
                    let b = self#pattern b in (a, b))) a in
            let b = self#closed_flag b in Ppat_record (a, b)
        | Ppat_array (a) ->
            let a = (self#list self#pattern) a in Ppat_array (a)
        | Ppat_or (a,b) ->
            let a = self#pattern a in
            let b = self#pattern b in Ppat_or (a, b)
        | Ppat_constraint (a,b) ->
            let a = self#pattern a in
            let b = self#core_type b in Ppat_constraint (a, b)
        | Ppat_type (a) ->
            let a = (self#loc self#longident) a in Ppat_type (a)
        | Ppat_lazy (a) -> let a = self#pattern a in Ppat_lazy (a)
        | Ppat_unpack (a) ->
            let a = (self#loc self#string) a in Ppat_unpack (a)
        | Ppat_exception (a) -> let a = self#pattern a in Ppat_exception (a)
        | Ppat_extension (a) ->
            let a = self#extension a in Ppat_extension (a)
    method payload: payload -> payload =
      fun x  ->
        match x with
        | PStr (a) -> let a = self#structure a in PStr (a)
        | PTyp (a) -> let a = self#core_type a in PTyp (a)
        | PPat (a,b) ->
            let a = self#pattern a in
            let b = (self#option self#expression) b in PPat (a, b)
    method private_flag: Asttypes.private_flag -> Asttypes.private_flag =
      fun x  -> match x with | Private  -> Private | Public  -> Public
    method rec_flag: Asttypes.rec_flag -> Asttypes.rec_flag =
      fun x  ->
        match x with
        | Nonrecursive  -> Nonrecursive
        | Recursive  -> Recursive
    method row_field: row_field -> row_field =
      fun x  ->
        match x with
        | Rtag (a,b,c,d) ->
            let a = self#label a in
            let b = self#attributes b in
            let c = self#bool c in
            let d = (self#list self#core_type) d in Rtag (a, b, c, d)
        | Rinherit (a) -> let a = self#core_type a in Rinherit (a)
    method signature: signature -> signature = self#list self#signature_item
    method signature_item: signature_item -> signature_item =
      fun { psig_desc; psig_loc }  ->
        let psig_desc = self#signature_item_desc psig_desc in
        let psig_loc = self#location psig_loc in { psig_desc; psig_loc }
    method signature_item_desc: signature_item_desc -> signature_item_desc =
      fun x  ->
        match x with
        | Psig_value (a) ->
            let a = self#value_description a in Psig_value (a)
        | Psig_type (a) ->
            let a = (self#list self#type_declaration) a in Psig_type (a)
        | Psig_typext (a) -> let a = self#type_extension a in Psig_typext (a)
        | Psig_exception (a) ->
            let a = self#extension_constructor a in Psig_exception (a)
        | Psig_module (a) ->
            let a = self#module_declaration a in Psig_module (a)
        | Psig_recmodule (a) ->
            let a = (self#list self#module_declaration) a in
            Psig_recmodule (a)
        | Psig_modtype (a) ->
            let a = self#module_type_declaration a in Psig_modtype (a)
        | Psig_open (a) -> let a = self#open_description a in Psig_open (a)
        | Psig_include (a) ->
            let a = self#include_description a in Psig_include (a)
        | Psig_class (a) ->
            let a = (self#list self#class_description) a in Psig_class (a)
        | Psig_class_type (a) ->
            let a = (self#list self#class_type_declaration) a in
            Psig_class_type (a)
        | Psig_attribute (a) ->
            let a = self#attribute a in Psig_attribute (a)
        | Psig_extension (a,b) ->
            let a = self#extension a in
            let b = self#attributes b in Psig_extension (a, b)
    method string: string -> string = fun x  -> x
    method structure: structure -> structure = self#list self#structure_item
    method structure_item: structure_item -> structure_item =
      fun { pstr_desc; pstr_loc }  ->
        let pstr_desc = self#structure_item_desc pstr_desc in
        let pstr_loc = self#location pstr_loc in { pstr_desc; pstr_loc }
    method structure_item_desc: structure_item_desc -> structure_item_desc =
      fun x  ->
        match x with
        | Pstr_eval (a,b) ->
            let a = self#expression a in
            let b = self#attributes b in Pstr_eval (a, b)
        | Pstr_value (a,b) ->
            let a = self#rec_flag a in
            let b = (self#list self#value_binding) b in Pstr_value (a, b)
        | Pstr_primitive (a) ->
            let a = self#value_description a in Pstr_primitive (a)
        | Pstr_type (a) ->
            let a = (self#list self#type_declaration) a in Pstr_type (a)
        | Pstr_typext (a) -> let a = self#type_extension a in Pstr_typext (a)
        | Pstr_exception (a) ->
            let a = self#extension_constructor a in Pstr_exception (a)
        | Pstr_module (a) -> let a = self#module_binding a in Pstr_module (a)
        | Pstr_recmodule (a) ->
            let a = (self#list self#module_binding) a in Pstr_recmodule (a)
        | Pstr_modtype (a) ->
            let a = self#module_type_declaration a in Pstr_modtype (a)
        | Pstr_open (a) -> let a = self#open_description a in Pstr_open (a)
        | Pstr_class (a) ->
            let a = (self#list self#class_declaration) a in Pstr_class (a)
        | Pstr_class_type (a) ->
            let a = (self#list self#class_type_declaration) a in
            Pstr_class_type (a)
        | Pstr_include (a) ->
            let a = self#include_declaration a in Pstr_include (a)
        | Pstr_attribute (a) ->
            let a = self#attribute a in Pstr_attribute (a)
        | Pstr_extension (a,b) ->
            let a = self#extension a in
            let b = self#attributes b in Pstr_extension (a, b)
    method toplevel_phrase: toplevel_phrase -> toplevel_phrase =
      fun x  ->
        match x with
        | Ptop_def (a) -> let a = self#structure a in Ptop_def (a)
        | Ptop_dir (a,b) ->
            let a = self#string a in
            let b = self#directive_argument b in Ptop_dir (a, b)
    method type_declaration: type_declaration -> type_declaration =
      fun
        { ptype_name; ptype_params; ptype_cstrs; ptype_kind; ptype_private;
          ptype_manifest; ptype_attributes; ptype_loc }
         ->
        let ptype_name = (self#loc self#string) ptype_name in
        let ptype_params =
          (self#list
             (fun (a,b)  ->
                let a = self#core_type a in let b = self#variance b in (a, b)))
            ptype_params in
        let ptype_cstrs =
          (self#list
             (fun (a,b,c)  ->
                let a = self#core_type a in
                let b = self#core_type b in
                let c = self#location c in (a, b, c))) ptype_cstrs in
        let ptype_kind = self#type_kind ptype_kind in
        let ptype_private = self#private_flag ptype_private in
        let ptype_manifest = (self#option self#core_type) ptype_manifest in
        let ptype_attributes = self#attributes ptype_attributes in
        let ptype_loc = self#location ptype_loc in
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
    method type_extension: type_extension -> type_extension =
      fun
        { ptyext_path; ptyext_params; ptyext_constructors; ptyext_private;
          ptyext_attributes }
         ->
        let ptyext_path = (self#loc self#longident) ptyext_path in
        let ptyext_params =
          (self#list
             (fun (a,b)  ->
                let a = self#core_type a in let b = self#variance b in (a, b)))
            ptyext_params in
        let ptyext_constructors =
          (self#list self#extension_constructor) ptyext_constructors in
        let ptyext_private = self#private_flag ptyext_private in
        let ptyext_attributes = self#attributes ptyext_attributes in
        {
          ptyext_path;
          ptyext_params;
          ptyext_constructors;
          ptyext_private;
          ptyext_attributes
        }
    method type_kind: type_kind -> type_kind =
      fun x  ->
        match x with
        | Ptype_abstract  -> Ptype_abstract
        | Ptype_variant (a) ->
            let a = (self#list self#constructor_declaration) a in
            Ptype_variant (a)
        | Ptype_record (a) ->
            let a = (self#list self#label_declaration) a in Ptype_record (a)
        | Ptype_open  -> Ptype_open
    method value_binding: value_binding -> value_binding =
      fun { pvb_pat; pvb_expr; pvb_attributes; pvb_loc }  ->
        let pvb_pat = self#pattern pvb_pat in
        let pvb_expr = self#expression pvb_expr in
        let pvb_attributes = self#attributes pvb_attributes in
        let pvb_loc = self#location pvb_loc in
        { pvb_pat; pvb_expr; pvb_attributes; pvb_loc }
    method value_description: value_description -> value_description =
      fun { pval_name; pval_type; pval_prim; pval_attributes; pval_loc }  ->
        let pval_name = (self#loc self#string) pval_name in
        let pval_type = self#core_type pval_type in
        let pval_prim = (self#list self#string) pval_prim in
        let pval_attributes = self#attributes pval_attributes in
        let pval_loc = self#location pval_loc in
        { pval_name; pval_type; pval_prim; pval_attributes; pval_loc }
    method variance: Asttypes.variance -> Asttypes.variance =
      fun x  ->
        match x with
        | Covariant  -> Covariant
        | Contravariant  -> Contravariant
        | Invariant  -> Invariant
    method virtual_flag: Asttypes.virtual_flag -> Asttypes.virtual_flag =
      fun x  -> match x with | Virtual  -> Virtual | Concrete  -> Concrete
    method with_constraint: with_constraint -> with_constraint =
      fun x  ->
        match x with
        | Pwith_type (a,b) ->
            let a = (self#loc self#longident) a in
            let b = self#type_declaration b in Pwith_type (a, b)
        | Pwith_module (a,b) ->
            let a = (self#loc self#longident) a in
            let b = (self#loc self#longident) b in Pwith_module (a, b)
        | Pwith_typesubst (a) ->
            let a = self#type_declaration a in Pwith_typesubst (a)
        | Pwith_modsubst (a,b) ->
            let a = (self#loc self#string) a in
            let b = (self#loc self#longident) b in Pwith_modsubst (a, b)
  end
