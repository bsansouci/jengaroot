open Parsetree
class ['acc] t =
  object (self)
    method attribute: attribute -> 'acc -> (attribute* 'acc) =
      fun (a,b)  ->
        fun acc  ->
          let (a,acc) = (self#loc self#string) a acc in
          let (b,acc) = self#payload b acc in ((a, b), acc)
    method attributes: attributes -> 'acc -> (attributes* 'acc) =
      self#list self#attribute
    method bool: bool -> 'acc -> (bool* 'acc) =
      fun x  ->
        fun acc  ->
          match x with | false  -> (false, acc) | true  -> (true, acc)
    method case: case -> 'acc -> (case* 'acc) =
      fun { pc_lhs; pc_guard; pc_rhs }  ->
        fun acc  ->
          let (pc_lhs,acc) = self#pattern pc_lhs acc in
          let (pc_guard,acc) = (self#option self#expression) pc_guard acc in
          let (pc_rhs,acc) = self#expression pc_rhs acc in
          ({ pc_lhs; pc_guard; pc_rhs }, acc)
    method char: char -> 'acc -> (char* 'acc) =
      fun x  -> fun acc  -> (x, acc)
    method class_declaration:
      class_declaration -> 'acc -> (class_declaration* 'acc) =
      self#class_infos self#class_expr
    method class_description:
      class_description -> 'acc -> (class_description* 'acc) =
      self#class_infos self#class_type
    method class_expr: class_expr -> 'acc -> (class_expr* 'acc) =
      fun { pcl_desc; pcl_loc; pcl_attributes }  ->
        fun acc  ->
          let (pcl_desc,acc) = self#class_expr_desc pcl_desc acc in
          let (pcl_loc,acc) = self#location pcl_loc acc in
          let (pcl_attributes,acc) = self#attributes pcl_attributes acc in
          ({ pcl_desc; pcl_loc; pcl_attributes }, acc)
    method class_expr_desc:
      class_expr_desc -> 'acc -> (class_expr_desc* 'acc) =
      fun x  ->
        fun acc  ->
          match x with
          | Pcl_constr (a,b) ->
              let (a,acc) = (self#loc self#longident) a acc in
              let (b,acc) = (self#list self#core_type) b acc in
              ((Pcl_constr (a, b)), acc)
          | Pcl_structure (a) ->
              let (a,acc) = self#class_structure a acc in
              ((Pcl_structure (a)), acc)
          | Pcl_fun (a,b,c,d) ->
              let (a,acc) = self#label a acc in
              let (b,acc) = (self#option self#expression) b acc in
              let (c,acc) = self#pattern c acc in
              let (d,acc) = self#class_expr d acc in
              ((Pcl_fun (a, b, c, d)), acc)
          | Pcl_apply (a,b) ->
              let (a,acc) = self#class_expr a acc in
              let (b,acc) =
                (self#list
                   (fun (a,b)  ->
                      fun acc  ->
                        let (a,acc) = self#label a acc in
                        let (b,acc) = self#expression b acc in ((a, b), acc)))
                  b acc in
              ((Pcl_apply (a, b)), acc)
          | Pcl_let (a,b,c) ->
              let (a,acc) = self#rec_flag a acc in
              let (b,acc) = (self#list self#value_binding) b acc in
              let (c,acc) = self#class_expr c acc in
              ((Pcl_let (a, b, c)), acc)
          | Pcl_constraint (a,b) ->
              let (a,acc) = self#class_expr a acc in
              let (b,acc) = self#class_type b acc in
              ((Pcl_constraint (a, b)), acc)
          | Pcl_extension (a) ->
              let (a,acc) = self#extension a acc in
              ((Pcl_extension (a)), acc)
    method class_field: class_field -> 'acc -> (class_field* 'acc) =
      fun { pcf_desc; pcf_loc; pcf_attributes }  ->
        fun acc  ->
          let (pcf_desc,acc) = self#class_field_desc pcf_desc acc in
          let (pcf_loc,acc) = self#location pcf_loc acc in
          let (pcf_attributes,acc) = self#attributes pcf_attributes acc in
          ({ pcf_desc; pcf_loc; pcf_attributes }, acc)
    method class_field_desc:
      class_field_desc -> 'acc -> (class_field_desc* 'acc) =
      fun x  ->
        fun acc  ->
          match x with
          | Pcf_inherit (a,b,c) ->
              let (a,acc) = self#override_flag a acc in
              let (b,acc) = self#class_expr b acc in
              let (c,acc) = (self#option self#string) c acc in
              ((Pcf_inherit (a, b, c)), acc)
          | Pcf_val (a) ->
              let (a,acc) =
                (fun (a,b,c)  ->
                   fun acc  ->
                     let (a,acc) = (self#loc self#string) a acc in
                     let (b,acc) = self#mutable_flag b acc in
                     let (c,acc) = self#class_field_kind c acc in
                     ((a, b, c), acc)) a acc in
              ((Pcf_val (a)), acc)
          | Pcf_method (a) ->
              let (a,acc) =
                (fun (a,b,c)  ->
                   fun acc  ->
                     let (a,acc) = (self#loc self#string) a acc in
                     let (b,acc) = self#private_flag b acc in
                     let (c,acc) = self#class_field_kind c acc in
                     ((a, b, c), acc)) a acc in
              ((Pcf_method (a)), acc)
          | Pcf_constraint (a) ->
              let (a,acc) =
                (fun (a,b)  ->
                   fun acc  ->
                     let (a,acc) = self#core_type a acc in
                     let (b,acc) = self#core_type b acc in ((a, b), acc)) a
                  acc in
              ((Pcf_constraint (a)), acc)
          | Pcf_initializer (a) ->
              let (a,acc) = self#expression a acc in
              ((Pcf_initializer (a)), acc)
          | Pcf_attribute (a) ->
              let (a,acc) = self#attribute a acc in
              ((Pcf_attribute (a)), acc)
          | Pcf_extension (a) ->
              let (a,acc) = self#extension a acc in
              ((Pcf_extension (a)), acc)
    method class_field_kind:
      class_field_kind -> 'acc -> (class_field_kind* 'acc) =
      fun x  ->
        fun acc  ->
          match x with
          | Cfk_virtual (a) ->
              let (a,acc) = self#core_type a acc in ((Cfk_virtual (a)), acc)
          | Cfk_concrete (a,b) ->
              let (a,acc) = self#override_flag a acc in
              let (b,acc) = self#expression b acc in
              ((Cfk_concrete (a, b)), acc)
    method class_infos:
      'a .
        ('a -> 'acc -> ('a* 'acc)) ->
          'a class_infos -> 'acc -> ('a class_infos* 'acc)
      =
      fun map_a  ->
        fun
          { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes
            }
           ->
          fun acc  ->
            let (pci_virt,acc) = self#virtual_flag pci_virt acc in
            let (pci_params,acc) =
              (self#list
                 (fun (a,b)  ->
                    fun acc  ->
                      let (a,acc) = self#core_type a acc in
                      let (b,acc) = self#variance b acc in ((a, b), acc)))
                pci_params acc in
            let (pci_name,acc) = (self#loc self#string) pci_name acc in
            let (pci_expr,acc) = map_a pci_expr acc in
            let (pci_loc,acc) = self#location pci_loc acc in
            let (pci_attributes,acc) = self#attributes pci_attributes acc in
            ({
               pci_virt;
               pci_params;
               pci_name;
               pci_expr;
               pci_loc;
               pci_attributes
             }, acc)
    method class_signature:
      class_signature -> 'acc -> (class_signature* 'acc) =
      fun { pcsig_self; pcsig_fields }  ->
        fun acc  ->
          let (pcsig_self,acc) = self#core_type pcsig_self acc in
          let (pcsig_fields,acc) =
            (self#list self#class_type_field) pcsig_fields acc in
          ({ pcsig_self; pcsig_fields }, acc)
    method class_structure:
      class_structure -> 'acc -> (class_structure* 'acc) =
      fun { pcstr_self; pcstr_fields }  ->
        fun acc  ->
          let (pcstr_self,acc) = self#pattern pcstr_self acc in
          let (pcstr_fields,acc) =
            (self#list self#class_field) pcstr_fields acc in
          ({ pcstr_self; pcstr_fields }, acc)
    method class_type: class_type -> 'acc -> (class_type* 'acc) =
      fun { pcty_desc; pcty_loc; pcty_attributes }  ->
        fun acc  ->
          let (pcty_desc,acc) = self#class_type_desc pcty_desc acc in
          let (pcty_loc,acc) = self#location pcty_loc acc in
          let (pcty_attributes,acc) = self#attributes pcty_attributes acc in
          ({ pcty_desc; pcty_loc; pcty_attributes }, acc)
    method class_type_declaration:
      class_type_declaration -> 'acc -> (class_type_declaration* 'acc) =
      self#class_infos self#class_type
    method class_type_desc:
      class_type_desc -> 'acc -> (class_type_desc* 'acc) =
      fun x  ->
        fun acc  ->
          match x with
          | Pcty_constr (a,b) ->
              let (a,acc) = (self#loc self#longident) a acc in
              let (b,acc) = (self#list self#core_type) b acc in
              ((Pcty_constr (a, b)), acc)
          | Pcty_signature (a) ->
              let (a,acc) = self#class_signature a acc in
              ((Pcty_signature (a)), acc)
          | Pcty_arrow (a,b,c) ->
              let (a,acc) = self#label a acc in
              let (b,acc) = self#core_type b acc in
              let (c,acc) = self#class_type c acc in
              ((Pcty_arrow (a, b, c)), acc)
          | Pcty_extension (a) ->
              let (a,acc) = self#extension a acc in
              ((Pcty_extension (a)), acc)
    method class_type_field:
      class_type_field -> 'acc -> (class_type_field* 'acc) =
      fun { pctf_desc; pctf_loc; pctf_attributes }  ->
        fun acc  ->
          let (pctf_desc,acc) = self#class_type_field_desc pctf_desc acc in
          let (pctf_loc,acc) = self#location pctf_loc acc in
          let (pctf_attributes,acc) = self#attributes pctf_attributes acc in
          ({ pctf_desc; pctf_loc; pctf_attributes }, acc)
    method class_type_field_desc:
      class_type_field_desc -> 'acc -> (class_type_field_desc* 'acc) =
      fun x  ->
        fun acc  ->
          match x with
          | Pctf_inherit (a) ->
              let (a,acc) = self#class_type a acc in
              ((Pctf_inherit (a)), acc)
          | Pctf_val (a) ->
              let (a,acc) =
                (fun (a,b,c,d)  ->
                   fun acc  ->
                     let (a,acc) = self#string a acc in
                     let (b,acc) = self#mutable_flag b acc in
                     let (c,acc) = self#virtual_flag c acc in
                     let (d,acc) = self#core_type d acc in
                     ((a, b, c, d), acc)) a acc in
              ((Pctf_val (a)), acc)
          | Pctf_method (a) ->
              let (a,acc) =
                (fun (a,b,c,d)  ->
                   fun acc  ->
                     let (a,acc) = self#string a acc in
                     let (b,acc) = self#private_flag b acc in
                     let (c,acc) = self#virtual_flag c acc in
                     let (d,acc) = self#core_type d acc in
                     ((a, b, c, d), acc)) a acc in
              ((Pctf_method (a)), acc)
          | Pctf_constraint (a) ->
              let (a,acc) =
                (fun (a,b)  ->
                   fun acc  ->
                     let (a,acc) = self#core_type a acc in
                     let (b,acc) = self#core_type b acc in ((a, b), acc)) a
                  acc in
              ((Pctf_constraint (a)), acc)
          | Pctf_attribute (a) ->
              let (a,acc) = self#attribute a acc in
              ((Pctf_attribute (a)), acc)
          | Pctf_extension (a) ->
              let (a,acc) = self#extension a acc in
              ((Pctf_extension (a)), acc)
    method closed_flag:
      Asttypes.closed_flag -> 'acc -> (Asttypes.closed_flag* 'acc) =
      fun x  ->
        fun acc  ->
          match x with | Closed  -> (Closed, acc) | Open  -> (Open, acc)
    method constant: Asttypes.constant -> 'acc -> (Asttypes.constant* 'acc) =
      fun x  ->
        fun acc  ->
          match x with
          | Const_int (a) ->
              let (a,acc) = self#int a acc in ((Const_int (a)), acc)
          | Const_char (a) ->
              let (a,acc) = self#char a acc in ((Const_char (a)), acc)
          | Const_string (a,b) ->
              let (a,acc) = self#string a acc in
              let (b,acc) = (self#option self#string) b acc in
              ((Const_string (a, b)), acc)
          | Const_float (a) ->
              let (a,acc) = self#string a acc in ((Const_float (a)), acc)
          | Const_int32 (a) ->
              let (a,acc) = self#int32 a acc in ((Const_int32 (a)), acc)
          | Const_int64 (a) ->
              let (a,acc) = self#int64 a acc in ((Const_int64 (a)), acc)
          | Const_nativeint (a) ->
              let (a,acc) = self#nativeint a acc in
              ((Const_nativeint (a)), acc)
    method constructor_declaration:
      constructor_declaration -> 'acc -> (constructor_declaration* 'acc) =
      fun { pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes }  ->
        fun acc  ->
          let (pcd_name,acc) = (self#loc self#string) pcd_name acc in
          let (pcd_args,acc) = (self#list self#core_type) pcd_args acc in
          let (pcd_res,acc) = (self#option self#core_type) pcd_res acc in
          let (pcd_loc,acc) = self#location pcd_loc acc in
          let (pcd_attributes,acc) = self#attributes pcd_attributes acc in
          ({ pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes }, acc)
    method core_type: core_type -> 'acc -> (core_type* 'acc) =
      fun { ptyp_desc; ptyp_loc; ptyp_attributes }  ->
        fun acc  ->
          let (ptyp_desc,acc) = self#core_type_desc ptyp_desc acc in
          let (ptyp_loc,acc) = self#location ptyp_loc acc in
          let (ptyp_attributes,acc) = self#attributes ptyp_attributes acc in
          ({ ptyp_desc; ptyp_loc; ptyp_attributes }, acc)
    method core_type_desc: core_type_desc -> 'acc -> (core_type_desc* 'acc) =
      fun x  ->
        fun acc  ->
          match x with
          | Ptyp_any  -> (Ptyp_any, acc)
          | Ptyp_var (a) ->
              let (a,acc) = self#string a acc in ((Ptyp_var (a)), acc)
          | Ptyp_arrow (a,b,c) ->
              let (a,acc) = self#label a acc in
              let (b,acc) = self#core_type b acc in
              let (c,acc) = self#core_type c acc in
              ((Ptyp_arrow (a, b, c)), acc)
          | Ptyp_tuple (a) ->
              let (a,acc) = (self#list self#core_type) a acc in
              ((Ptyp_tuple (a)), acc)
          | Ptyp_constr (a,b) ->
              let (a,acc) = (self#loc self#longident) a acc in
              let (b,acc) = (self#list self#core_type) b acc in
              ((Ptyp_constr (a, b)), acc)
          | Ptyp_object (a,b) ->
              let (a,acc) =
                (self#list
                   (fun (a,b,c)  ->
                      fun acc  ->
                        let (a,acc) = self#string a acc in
                        let (b,acc) = self#attributes b acc in
                        let (c,acc) = self#core_type c acc in
                        ((a, b, c), acc))) a acc in
              let (b,acc) = self#closed_flag b acc in
              ((Ptyp_object (a, b)), acc)
          | Ptyp_class (a,b) ->
              let (a,acc) = (self#loc self#longident) a acc in
              let (b,acc) = (self#list self#core_type) b acc in
              ((Ptyp_class (a, b)), acc)
          | Ptyp_alias (a,b) ->
              let (a,acc) = self#core_type a acc in
              let (b,acc) = self#string b acc in ((Ptyp_alias (a, b)), acc)
          | Ptyp_variant (a,b,c) ->
              let (a,acc) = (self#list self#row_field) a acc in
              let (b,acc) = self#closed_flag b acc in
              let (c,acc) = (self#option (self#list self#label)) c acc in
              ((Ptyp_variant (a, b, c)), acc)
          | Ptyp_poly (a,b) ->
              let (a,acc) = (self#list self#string) a acc in
              let (b,acc) = self#core_type b acc in ((Ptyp_poly (a, b)), acc)
          | Ptyp_package (a) ->
              let (a,acc) = self#package_type a acc in
              ((Ptyp_package (a)), acc)
          | Ptyp_extension (a) ->
              let (a,acc) = self#extension a acc in
              ((Ptyp_extension (a)), acc)
    method direction_flag:
      Asttypes.direction_flag -> 'acc -> (Asttypes.direction_flag* 'acc) =
      fun x  ->
        fun acc  ->
          match x with | Upto  -> (Upto, acc) | Downto  -> (Downto, acc)
    method directive_argument:
      directive_argument -> 'acc -> (directive_argument* 'acc) =
      fun x  ->
        fun acc  ->
          match x with
          | Pdir_none  -> (Pdir_none, acc)
          | Pdir_string (a) ->
              let (a,acc) = self#string a acc in ((Pdir_string (a)), acc)
          | Pdir_int (a) ->
              let (a,acc) = self#int a acc in ((Pdir_int (a)), acc)
          | Pdir_ident (a) ->
              let (a,acc) = self#longident a acc in ((Pdir_ident (a)), acc)
          | Pdir_bool (a) ->
              let (a,acc) = self#bool a acc in ((Pdir_bool (a)), acc)
    method expression: expression -> 'acc -> (expression* 'acc) =
      fun { pexp_desc; pexp_loc; pexp_attributes }  ->
        fun acc  ->
          let (pexp_desc,acc) = self#expression_desc pexp_desc acc in
          let (pexp_loc,acc) = self#location pexp_loc acc in
          let (pexp_attributes,acc) = self#attributes pexp_attributes acc in
          ({ pexp_desc; pexp_loc; pexp_attributes }, acc)
    method expression_desc:
      expression_desc -> 'acc -> (expression_desc* 'acc) =
      fun x  ->
        fun acc  ->
          match x with
          | Pexp_ident (a) ->
              let (a,acc) = (self#loc self#longident) a acc in
              ((Pexp_ident (a)), acc)
          | Pexp_constant (a) ->
              let (a,acc) = self#constant a acc in ((Pexp_constant (a)), acc)
          | Pexp_let (a,b,c) ->
              let (a,acc) = self#rec_flag a acc in
              let (b,acc) = (self#list self#value_binding) b acc in
              let (c,acc) = self#expression c acc in
              ((Pexp_let (a, b, c)), acc)
          | Pexp_function (a) ->
              let (a,acc) = (self#list self#case) a acc in
              ((Pexp_function (a)), acc)
          | Pexp_fun (a,b,c,d) ->
              let (a,acc) = self#label a acc in
              let (b,acc) = (self#option self#expression) b acc in
              let (c,acc) = self#pattern c acc in
              let (d,acc) = self#expression d acc in
              ((Pexp_fun (a, b, c, d)), acc)
          | Pexp_apply (a,b) ->
              let (a,acc) = self#expression a acc in
              let (b,acc) =
                (self#list
                   (fun (a,b)  ->
                      fun acc  ->
                        let (a,acc) = self#label a acc in
                        let (b,acc) = self#expression b acc in ((a, b), acc)))
                  b acc in
              ((Pexp_apply (a, b)), acc)
          | Pexp_match (a,b) ->
              let (a,acc) = self#expression a acc in
              let (b,acc) = (self#list self#case) b acc in
              ((Pexp_match (a, b)), acc)
          | Pexp_try (a,b) ->
              let (a,acc) = self#expression a acc in
              let (b,acc) = (self#list self#case) b acc in
              ((Pexp_try (a, b)), acc)
          | Pexp_tuple (a) ->
              let (a,acc) = (self#list self#expression) a acc in
              ((Pexp_tuple (a)), acc)
          | Pexp_construct (a,b) ->
              let (a,acc) = (self#loc self#longident) a acc in
              let (b,acc) = (self#option self#expression) b acc in
              ((Pexp_construct (a, b)), acc)
          | Pexp_variant (a,b) ->
              let (a,acc) = self#label a acc in
              let (b,acc) = (self#option self#expression) b acc in
              ((Pexp_variant (a, b)), acc)
          | Pexp_record (a,b) ->
              let (a,acc) =
                (self#list
                   (fun (a,b)  ->
                      fun acc  ->
                        let (a,acc) = (self#loc self#longident) a acc in
                        let (b,acc) = self#expression b acc in ((a, b), acc)))
                  a acc in
              let (b,acc) = (self#option self#expression) b acc in
              ((Pexp_record (a, b)), acc)
          | Pexp_field (a,b) ->
              let (a,acc) = self#expression a acc in
              let (b,acc) = (self#loc self#longident) b acc in
              ((Pexp_field (a, b)), acc)
          | Pexp_setfield (a,b,c) ->
              let (a,acc) = self#expression a acc in
              let (b,acc) = (self#loc self#longident) b acc in
              let (c,acc) = self#expression c acc in
              ((Pexp_setfield (a, b, c)), acc)
          | Pexp_array (a) ->
              let (a,acc) = (self#list self#expression) a acc in
              ((Pexp_array (a)), acc)
          | Pexp_ifthenelse (a,b,c) ->
              let (a,acc) = self#expression a acc in
              let (b,acc) = self#expression b acc in
              let (c,acc) = (self#option self#expression) c acc in
              ((Pexp_ifthenelse (a, b, c)), acc)
          | Pexp_sequence (a,b) ->
              let (a,acc) = self#expression a acc in
              let (b,acc) = self#expression b acc in
              ((Pexp_sequence (a, b)), acc)
          | Pexp_while (a,b) ->
              let (a,acc) = self#expression a acc in
              let (b,acc) = self#expression b acc in
              ((Pexp_while (a, b)), acc)
          | Pexp_for (a,b,c,d,e) ->
              let (a,acc) = self#pattern a acc in
              let (b,acc) = self#expression b acc in
              let (c,acc) = self#expression c acc in
              let (d,acc) = self#direction_flag d acc in
              let (e,acc) = self#expression e acc in
              ((Pexp_for (a, b, c, d, e)), acc)
          | Pexp_constraint (a,b) ->
              let (a,acc) = self#expression a acc in
              let (b,acc) = self#core_type b acc in
              ((Pexp_constraint (a, b)), acc)
          | Pexp_coerce (a,b,c) ->
              let (a,acc) = self#expression a acc in
              let (b,acc) = (self#option self#core_type) b acc in
              let (c,acc) = self#core_type c acc in
              ((Pexp_coerce (a, b, c)), acc)
          | Pexp_send (a,b) ->
              let (a,acc) = self#expression a acc in
              let (b,acc) = self#string b acc in ((Pexp_send (a, b)), acc)
          | Pexp_new (a) ->
              let (a,acc) = (self#loc self#longident) a acc in
              ((Pexp_new (a)), acc)
          | Pexp_setinstvar (a,b) ->
              let (a,acc) = (self#loc self#string) a acc in
              let (b,acc) = self#expression b acc in
              ((Pexp_setinstvar (a, b)), acc)
          | Pexp_override (a) ->
              let (a,acc) =
                (self#list
                   (fun (a,b)  ->
                      fun acc  ->
                        let (a,acc) = (self#loc self#string) a acc in
                        let (b,acc) = self#expression b acc in ((a, b), acc)))
                  a acc in
              ((Pexp_override (a)), acc)
          | Pexp_letmodule (a,b,c) ->
              let (a,acc) = (self#loc self#string) a acc in
              let (b,acc) = self#module_expr b acc in
              let (c,acc) = self#expression c acc in
              ((Pexp_letmodule (a, b, c)), acc)
          | Pexp_assert (a) ->
              let (a,acc) = self#expression a acc in ((Pexp_assert (a)), acc)
          | Pexp_lazy (a) ->
              let (a,acc) = self#expression a acc in ((Pexp_lazy (a)), acc)
          | Pexp_poly (a,b) ->
              let (a,acc) = self#expression a acc in
              let (b,acc) = (self#option self#core_type) b acc in
              ((Pexp_poly (a, b)), acc)
          | Pexp_object (a) ->
              let (a,acc) = self#class_structure a acc in
              ((Pexp_object (a)), acc)
          | Pexp_newtype (a,b) ->
              let (a,acc) = self#string a acc in
              let (b,acc) = self#expression b acc in
              ((Pexp_newtype (a, b)), acc)
          | Pexp_pack (a) ->
              let (a,acc) = self#module_expr a acc in ((Pexp_pack (a)), acc)
          | Pexp_open (a,b,c) ->
              let (a,acc) = self#override_flag a acc in
              let (b,acc) = (self#loc self#longident) b acc in
              let (c,acc) = self#expression c acc in
              ((Pexp_open (a, b, c)), acc)
          | Pexp_extension (a) ->
              let (a,acc) = self#extension a acc in
              ((Pexp_extension (a)), acc)
    method extension: extension -> 'acc -> (extension* 'acc) =
      fun (a,b)  ->
        fun acc  ->
          let (a,acc) = (self#loc self#string) a acc in
          let (b,acc) = self#payload b acc in ((a, b), acc)
    method extension_constructor:
      extension_constructor -> 'acc -> (extension_constructor* 'acc) =
      fun { pext_name; pext_kind; pext_loc; pext_attributes }  ->
        fun acc  ->
          let (pext_name,acc) = (self#loc self#string) pext_name acc in
          let (pext_kind,acc) = self#extension_constructor_kind pext_kind acc in
          let (pext_loc,acc) = self#location pext_loc acc in
          let (pext_attributes,acc) = self#attributes pext_attributes acc in
          ({ pext_name; pext_kind; pext_loc; pext_attributes }, acc)
    method extension_constructor_kind:
      extension_constructor_kind ->
        'acc -> (extension_constructor_kind* 'acc)
      =
      fun x  ->
        fun acc  ->
          match x with
          | Pext_decl (a,b) ->
              let (a,acc) = (self#list self#core_type) a acc in
              let (b,acc) = (self#option self#core_type) b acc in
              ((Pext_decl (a, b)), acc)
          | Pext_rebind (a) ->
              let (a,acc) = (self#loc self#longident) a acc in
              ((Pext_rebind (a)), acc)
    method include_declaration:
      include_declaration -> 'acc -> (include_declaration* 'acc) =
      self#include_infos self#module_expr
    method include_description:
      include_description -> 'acc -> (include_description* 'acc) =
      self#include_infos self#module_type
    method include_infos:
      'a .
        ('a -> 'acc -> ('a* 'acc)) ->
          'a include_infos -> 'acc -> ('a include_infos* 'acc)
      =
      fun map_a  ->
        fun { pincl_mod; pincl_loc; pincl_attributes }  ->
          fun acc  ->
            let (pincl_mod,acc) = map_a pincl_mod acc in
            let (pincl_loc,acc) = self#location pincl_loc acc in
            let (pincl_attributes,acc) = self#attributes pincl_attributes acc in
            ({ pincl_mod; pincl_loc; pincl_attributes }, acc)
    method int: int -> 'acc -> (int* 'acc) = fun x  -> fun acc  -> (x, acc)
    method int32: int32 -> 'acc -> (int32* 'acc) =
      fun x  -> fun acc  -> (x, acc)
    method int64: int64 -> 'acc -> (int64* 'acc) =
      fun x  -> fun acc  -> (x, acc)
    method label: Asttypes.label -> 'acc -> (Asttypes.label* 'acc) =
      self#string
    method label_declaration:
      label_declaration -> 'acc -> (label_declaration* 'acc) =
      fun { pld_name; pld_mutable; pld_type; pld_loc; pld_attributes }  ->
        fun acc  ->
          let (pld_name,acc) = (self#loc self#string) pld_name acc in
          let (pld_mutable,acc) = self#mutable_flag pld_mutable acc in
          let (pld_type,acc) = self#core_type pld_type acc in
          let (pld_loc,acc) = self#location pld_loc acc in
          let (pld_attributes,acc) = self#attributes pld_attributes acc in
          ({ pld_name; pld_mutable; pld_type; pld_loc; pld_attributes }, acc)
    method lexing_position:
      Lexing.position -> 'acc -> (Lexing.position* 'acc) =
      fun { pos_fname; pos_lnum; pos_bol; pos_cnum }  ->
        fun acc  ->
          let (pos_fname,acc) = self#string pos_fname acc in
          let (pos_lnum,acc) = self#int pos_lnum acc in
          let (pos_bol,acc) = self#int pos_bol acc in
          let (pos_cnum,acc) = self#int pos_cnum acc in
          ({ pos_fname; pos_lnum; pos_bol; pos_cnum }, acc)
    method list:
      'a . ('a -> 'acc -> ('a* 'acc)) -> 'a list -> 'acc -> ('a list* 'acc) =
      fun map_a  ->
        fun x  ->
          fun acc  ->
            match x with
            | [] -> ([], acc)
            | a::b ->
                let (a,acc) = map_a a acc in
                let (b,acc) = (self#list map_a) b acc in ((a :: b), acc)
    method loc:
      'a .
        ('a -> 'acc -> ('a* 'acc)) ->
          'a Asttypes.loc -> 'acc -> ('a Asttypes.loc* 'acc)
      =
      fun map_a  ->
        fun { txt; loc }  ->
          fun acc  ->
            let (txt,acc) = map_a txt acc in
            let (loc,acc) = self#location loc acc in ({ txt; loc }, acc)
    method location: Location.t -> 'acc -> (Location.t* 'acc) =
      fun { loc_start; loc_end; loc_ghost }  ->
        fun acc  ->
          let (loc_start,acc) = self#lexing_position loc_start acc in
          let (loc_end,acc) = self#lexing_position loc_end acc in
          let (loc_ghost,acc) = self#bool loc_ghost acc in
          ({ loc_start; loc_end; loc_ghost }, acc)
    method longident: Longident.t -> 'acc -> (Longident.t* 'acc) =
      fun x  ->
        fun acc  ->
          match x with
          | Lident (a) ->
              let (a,acc) = self#string a acc in ((Lident (a)), acc)
          | Ldot (a,b) ->
              let (a,acc) = self#longident a acc in
              let (b,acc) = self#string b acc in ((Ldot (a, b)), acc)
          | Lapply (a,b) ->
              let (a,acc) = self#longident a acc in
              let (b,acc) = self#longident b acc in ((Lapply (a, b)), acc)
    method module_binding: module_binding -> 'acc -> (module_binding* 'acc) =
      fun { pmb_name; pmb_expr; pmb_attributes; pmb_loc }  ->
        fun acc  ->
          let (pmb_name,acc) = (self#loc self#string) pmb_name acc in
          let (pmb_expr,acc) = self#module_expr pmb_expr acc in
          let (pmb_attributes,acc) = self#attributes pmb_attributes acc in
          let (pmb_loc,acc) = self#location pmb_loc acc in
          ({ pmb_name; pmb_expr; pmb_attributes; pmb_loc }, acc)
    method module_declaration:
      module_declaration -> 'acc -> (module_declaration* 'acc) =
      fun { pmd_name; pmd_type; pmd_attributes; pmd_loc }  ->
        fun acc  ->
          let (pmd_name,acc) = (self#loc self#string) pmd_name acc in
          let (pmd_type,acc) = self#module_type pmd_type acc in
          let (pmd_attributes,acc) = self#attributes pmd_attributes acc in
          let (pmd_loc,acc) = self#location pmd_loc acc in
          ({ pmd_name; pmd_type; pmd_attributes; pmd_loc }, acc)
    method module_expr: module_expr -> 'acc -> (module_expr* 'acc) =
      fun { pmod_desc; pmod_loc; pmod_attributes }  ->
        fun acc  ->
          let (pmod_desc,acc) = self#module_expr_desc pmod_desc acc in
          let (pmod_loc,acc) = self#location pmod_loc acc in
          let (pmod_attributes,acc) = self#attributes pmod_attributes acc in
          ({ pmod_desc; pmod_loc; pmod_attributes }, acc)
    method module_expr_desc:
      module_expr_desc -> 'acc -> (module_expr_desc* 'acc) =
      fun x  ->
        fun acc  ->
          match x with
          | Pmod_ident (a) ->
              let (a,acc) = (self#loc self#longident) a acc in
              ((Pmod_ident (a)), acc)
          | Pmod_structure (a) ->
              let (a,acc) = self#structure a acc in
              ((Pmod_structure (a)), acc)
          | Pmod_functor (a,b,c) ->
              let (a,acc) = (self#loc self#string) a acc in
              let (b,acc) = (self#option self#module_type) b acc in
              let (c,acc) = self#module_expr c acc in
              ((Pmod_functor (a, b, c)), acc)
          | Pmod_apply (a,b) ->
              let (a,acc) = self#module_expr a acc in
              let (b,acc) = self#module_expr b acc in
              ((Pmod_apply (a, b)), acc)
          | Pmod_constraint (a,b) ->
              let (a,acc) = self#module_expr a acc in
              let (b,acc) = self#module_type b acc in
              ((Pmod_constraint (a, b)), acc)
          | Pmod_unpack (a) ->
              let (a,acc) = self#expression a acc in ((Pmod_unpack (a)), acc)
          | Pmod_extension (a) ->
              let (a,acc) = self#extension a acc in
              ((Pmod_extension (a)), acc)
    method module_type: module_type -> 'acc -> (module_type* 'acc) =
      fun { pmty_desc; pmty_loc; pmty_attributes }  ->
        fun acc  ->
          let (pmty_desc,acc) = self#module_type_desc pmty_desc acc in
          let (pmty_loc,acc) = self#location pmty_loc acc in
          let (pmty_attributes,acc) = self#attributes pmty_attributes acc in
          ({ pmty_desc; pmty_loc; pmty_attributes }, acc)
    method module_type_declaration:
      module_type_declaration -> 'acc -> (module_type_declaration* 'acc) =
      fun { pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc }  ->
        fun acc  ->
          let (pmtd_name,acc) = (self#loc self#string) pmtd_name acc in
          let (pmtd_type,acc) = (self#option self#module_type) pmtd_type acc in
          let (pmtd_attributes,acc) = self#attributes pmtd_attributes acc in
          let (pmtd_loc,acc) = self#location pmtd_loc acc in
          ({ pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc }, acc)
    method module_type_desc:
      module_type_desc -> 'acc -> (module_type_desc* 'acc) =
      fun x  ->
        fun acc  ->
          match x with
          | Pmty_ident (a) ->
              let (a,acc) = (self#loc self#longident) a acc in
              ((Pmty_ident (a)), acc)
          | Pmty_signature (a) ->
              let (a,acc) = self#signature a acc in
              ((Pmty_signature (a)), acc)
          | Pmty_functor (a,b,c) ->
              let (a,acc) = (self#loc self#string) a acc in
              let (b,acc) = (self#option self#module_type) b acc in
              let (c,acc) = self#module_type c acc in
              ((Pmty_functor (a, b, c)), acc)
          | Pmty_with (a,b) ->
              let (a,acc) = self#module_type a acc in
              let (b,acc) = (self#list self#with_constraint) b acc in
              ((Pmty_with (a, b)), acc)
          | Pmty_typeof (a) ->
              let (a,acc) = self#module_expr a acc in
              ((Pmty_typeof (a)), acc)
          | Pmty_extension (a) ->
              let (a,acc) = self#extension a acc in
              ((Pmty_extension (a)), acc)
          | Pmty_alias (a) ->
              let (a,acc) = (self#loc self#longident) a acc in
              ((Pmty_alias (a)), acc)
    method mutable_flag:
      Asttypes.mutable_flag -> 'acc -> (Asttypes.mutable_flag* 'acc) =
      fun x  ->
        fun acc  ->
          match x with
          | Immutable  -> (Immutable, acc)
          | Mutable  -> (Mutable, acc)
    method nativeint: nativeint -> 'acc -> (nativeint* 'acc) =
      fun x  -> fun acc  -> (x, acc)
    method open_description:
      open_description -> 'acc -> (open_description* 'acc) =
      fun { popen_lid; popen_override; popen_loc; popen_attributes }  ->
        fun acc  ->
          let (popen_lid,acc) = (self#loc self#longident) popen_lid acc in
          let (popen_override,acc) = self#override_flag popen_override acc in
          let (popen_loc,acc) = self#location popen_loc acc in
          let (popen_attributes,acc) = self#attributes popen_attributes acc in
          ({ popen_lid; popen_override; popen_loc; popen_attributes }, acc)
    method option:
      'a .
        ('a -> 'acc -> ('a* 'acc)) -> 'a option -> 'acc -> ('a option* 'acc)
      =
      fun map_a  ->
        fun x  ->
          fun acc  ->
            match x with
            | None  -> (None, acc)
            | Some (a) -> let (a,acc) = map_a a acc in ((Some (a)), acc)
    method override_flag:
      Asttypes.override_flag -> 'acc -> (Asttypes.override_flag* 'acc) =
      fun x  ->
        fun acc  ->
          match x with
          | Override  -> (Override, acc)
          | Fresh  -> (Fresh, acc)
    method package_type: package_type -> 'acc -> (package_type* 'acc) =
      fun (a,b)  ->
        fun acc  ->
          let (a,acc) = (self#loc self#longident) a acc in
          let (b,acc) =
            (self#list
               (fun (a,b)  ->
                  fun acc  ->
                    let (a,acc) = (self#loc self#longident) a acc in
                    let (b,acc) = self#core_type b acc in ((a, b), acc))) b
              acc in
          ((a, b), acc)
    method pattern: pattern -> 'acc -> (pattern* 'acc) =
      fun { ppat_desc; ppat_loc; ppat_attributes }  ->
        fun acc  ->
          let (ppat_desc,acc) = self#pattern_desc ppat_desc acc in
          let (ppat_loc,acc) = self#location ppat_loc acc in
          let (ppat_attributes,acc) = self#attributes ppat_attributes acc in
          ({ ppat_desc; ppat_loc; ppat_attributes }, acc)
    method pattern_desc: pattern_desc -> 'acc -> (pattern_desc* 'acc) =
      fun x  ->
        fun acc  ->
          match x with
          | Ppat_any  -> (Ppat_any, acc)
          | Ppat_var (a) ->
              let (a,acc) = (self#loc self#string) a acc in
              ((Ppat_var (a)), acc)
          | Ppat_alias (a,b) ->
              let (a,acc) = self#pattern a acc in
              let (b,acc) = (self#loc self#string) b acc in
              ((Ppat_alias (a, b)), acc)
          | Ppat_constant (a) ->
              let (a,acc) = self#constant a acc in ((Ppat_constant (a)), acc)
          | Ppat_interval (a,b) ->
              let (a,acc) = self#constant a acc in
              let (b,acc) = self#constant b acc in
              ((Ppat_interval (a, b)), acc)
          | Ppat_tuple (a) ->
              let (a,acc) = (self#list self#pattern) a acc in
              ((Ppat_tuple (a)), acc)
          | Ppat_construct (a,b) ->
              let (a,acc) = (self#loc self#longident) a acc in
              let (b,acc) = (self#option self#pattern) b acc in
              ((Ppat_construct (a, b)), acc)
          | Ppat_variant (a,b) ->
              let (a,acc) = self#label a acc in
              let (b,acc) = (self#option self#pattern) b acc in
              ((Ppat_variant (a, b)), acc)
          | Ppat_record (a,b) ->
              let (a,acc) =
                (self#list
                   (fun (a,b)  ->
                      fun acc  ->
                        let (a,acc) = (self#loc self#longident) a acc in
                        let (b,acc) = self#pattern b acc in ((a, b), acc))) a
                  acc in
              let (b,acc) = self#closed_flag b acc in
              ((Ppat_record (a, b)), acc)
          | Ppat_array (a) ->
              let (a,acc) = (self#list self#pattern) a acc in
              ((Ppat_array (a)), acc)
          | Ppat_or (a,b) ->
              let (a,acc) = self#pattern a acc in
              let (b,acc) = self#pattern b acc in ((Ppat_or (a, b)), acc)
          | Ppat_constraint (a,b) ->
              let (a,acc) = self#pattern a acc in
              let (b,acc) = self#core_type b acc in
              ((Ppat_constraint (a, b)), acc)
          | Ppat_type (a) ->
              let (a,acc) = (self#loc self#longident) a acc in
              ((Ppat_type (a)), acc)
          | Ppat_lazy (a) ->
              let (a,acc) = self#pattern a acc in ((Ppat_lazy (a)), acc)
          | Ppat_unpack (a) ->
              let (a,acc) = (self#loc self#string) a acc in
              ((Ppat_unpack (a)), acc)
          | Ppat_exception (a) ->
              let (a,acc) = self#pattern a acc in ((Ppat_exception (a)), acc)
          | Ppat_extension (a) ->
              let (a,acc) = self#extension a acc in
              ((Ppat_extension (a)), acc)
    method payload: payload -> 'acc -> (payload* 'acc) =
      fun x  ->
        fun acc  ->
          match x with
          | PStr (a) ->
              let (a,acc) = self#structure a acc in ((PStr (a)), acc)
          | PTyp (a) ->
              let (a,acc) = self#core_type a acc in ((PTyp (a)), acc)
          | PPat (a,b) ->
              let (a,acc) = self#pattern a acc in
              let (b,acc) = (self#option self#expression) b acc in
              ((PPat (a, b)), acc)
    method private_flag:
      Asttypes.private_flag -> 'acc -> (Asttypes.private_flag* 'acc) =
      fun x  ->
        fun acc  ->
          match x with
          | Private  -> (Private, acc)
          | Public  -> (Public, acc)
    method rec_flag: Asttypes.rec_flag -> 'acc -> (Asttypes.rec_flag* 'acc) =
      fun x  ->
        fun acc  ->
          match x with
          | Nonrecursive  -> (Nonrecursive, acc)
          | Recursive  -> (Recursive, acc)
    method row_field: row_field -> 'acc -> (row_field* 'acc) =
      fun x  ->
        fun acc  ->
          match x with
          | Rtag (a,b,c,d) ->
              let (a,acc) = self#label a acc in
              let (b,acc) = self#attributes b acc in
              let (c,acc) = self#bool c acc in
              let (d,acc) = (self#list self#core_type) d acc in
              ((Rtag (a, b, c, d)), acc)
          | Rinherit (a) ->
              let (a,acc) = self#core_type a acc in ((Rinherit (a)), acc)
    method signature: signature -> 'acc -> (signature* 'acc) =
      self#list self#signature_item
    method signature_item: signature_item -> 'acc -> (signature_item* 'acc) =
      fun { psig_desc; psig_loc }  ->
        fun acc  ->
          let (psig_desc,acc) = self#signature_item_desc psig_desc acc in
          let (psig_loc,acc) = self#location psig_loc acc in
          ({ psig_desc; psig_loc }, acc)
    method signature_item_desc:
      signature_item_desc -> 'acc -> (signature_item_desc* 'acc) =
      fun x  ->
        fun acc  ->
          match x with
          | Psig_value (a) ->
              let (a,acc) = self#value_description a acc in
              ((Psig_value (a)), acc)
          | Psig_type (a) ->
              let (a,acc) = (self#list self#type_declaration) a acc in
              ((Psig_type (a)), acc)
          | Psig_typext (a) ->
              let (a,acc) = self#type_extension a acc in
              ((Psig_typext (a)), acc)
          | Psig_exception (a) ->
              let (a,acc) = self#extension_constructor a acc in
              ((Psig_exception (a)), acc)
          | Psig_module (a) ->
              let (a,acc) = self#module_declaration a acc in
              ((Psig_module (a)), acc)
          | Psig_recmodule (a) ->
              let (a,acc) = (self#list self#module_declaration) a acc in
              ((Psig_recmodule (a)), acc)
          | Psig_modtype (a) ->
              let (a,acc) = self#module_type_declaration a acc in
              ((Psig_modtype (a)), acc)
          | Psig_open (a) ->
              let (a,acc) = self#open_description a acc in
              ((Psig_open (a)), acc)
          | Psig_include (a) ->
              let (a,acc) = self#include_description a acc in
              ((Psig_include (a)), acc)
          | Psig_class (a) ->
              let (a,acc) = (self#list self#class_description) a acc in
              ((Psig_class (a)), acc)
          | Psig_class_type (a) ->
              let (a,acc) = (self#list self#class_type_declaration) a acc in
              ((Psig_class_type (a)), acc)
          | Psig_attribute (a) ->
              let (a,acc) = self#attribute a acc in
              ((Psig_attribute (a)), acc)
          | Psig_extension (a,b) ->
              let (a,acc) = self#extension a acc in
              let (b,acc) = self#attributes b acc in
              ((Psig_extension (a, b)), acc)
    method string: string -> 'acc -> (string* 'acc) =
      fun x  -> fun acc  -> (x, acc)
    method structure: structure -> 'acc -> (structure* 'acc) =
      self#list self#structure_item
    method structure_item: structure_item -> 'acc -> (structure_item* 'acc) =
      fun { pstr_desc; pstr_loc }  ->
        fun acc  ->
          let (pstr_desc,acc) = self#structure_item_desc pstr_desc acc in
          let (pstr_loc,acc) = self#location pstr_loc acc in
          ({ pstr_desc; pstr_loc }, acc)
    method structure_item_desc:
      structure_item_desc -> 'acc -> (structure_item_desc* 'acc) =
      fun x  ->
        fun acc  ->
          match x with
          | Pstr_eval (a,b) ->
              let (a,acc) = self#expression a acc in
              let (b,acc) = self#attributes b acc in
              ((Pstr_eval (a, b)), acc)
          | Pstr_value (a,b) ->
              let (a,acc) = self#rec_flag a acc in
              let (b,acc) = (self#list self#value_binding) b acc in
              ((Pstr_value (a, b)), acc)
          | Pstr_primitive (a) ->
              let (a,acc) = self#value_description a acc in
              ((Pstr_primitive (a)), acc)
          | Pstr_type (a) ->
              let (a,acc) = (self#list self#type_declaration) a acc in
              ((Pstr_type (a)), acc)
          | Pstr_typext (a) ->
              let (a,acc) = self#type_extension a acc in
              ((Pstr_typext (a)), acc)
          | Pstr_exception (a) ->
              let (a,acc) = self#extension_constructor a acc in
              ((Pstr_exception (a)), acc)
          | Pstr_module (a) ->
              let (a,acc) = self#module_binding a acc in
              ((Pstr_module (a)), acc)
          | Pstr_recmodule (a) ->
              let (a,acc) = (self#list self#module_binding) a acc in
              ((Pstr_recmodule (a)), acc)
          | Pstr_modtype (a) ->
              let (a,acc) = self#module_type_declaration a acc in
              ((Pstr_modtype (a)), acc)
          | Pstr_open (a) ->
              let (a,acc) = self#open_description a acc in
              ((Pstr_open (a)), acc)
          | Pstr_class (a) ->
              let (a,acc) = (self#list self#class_declaration) a acc in
              ((Pstr_class (a)), acc)
          | Pstr_class_type (a) ->
              let (a,acc) = (self#list self#class_type_declaration) a acc in
              ((Pstr_class_type (a)), acc)
          | Pstr_include (a) ->
              let (a,acc) = self#include_declaration a acc in
              ((Pstr_include (a)), acc)
          | Pstr_attribute (a) ->
              let (a,acc) = self#attribute a acc in
              ((Pstr_attribute (a)), acc)
          | Pstr_extension (a,b) ->
              let (a,acc) = self#extension a acc in
              let (b,acc) = self#attributes b acc in
              ((Pstr_extension (a, b)), acc)
    method toplevel_phrase:
      toplevel_phrase -> 'acc -> (toplevel_phrase* 'acc) =
      fun x  ->
        fun acc  ->
          match x with
          | Ptop_def (a) ->
              let (a,acc) = self#structure a acc in ((Ptop_def (a)), acc)
          | Ptop_dir (a,b) ->
              let (a,acc) = self#string a acc in
              let (b,acc) = self#directive_argument b acc in
              ((Ptop_dir (a, b)), acc)
    method type_declaration:
      type_declaration -> 'acc -> (type_declaration* 'acc) =
      fun
        { ptype_name; ptype_params; ptype_cstrs; ptype_kind; ptype_private;
          ptype_manifest; ptype_attributes; ptype_loc }
         ->
        fun acc  ->
          let (ptype_name,acc) = (self#loc self#string) ptype_name acc in
          let (ptype_params,acc) =
            (self#list
               (fun (a,b)  ->
                  fun acc  ->
                    let (a,acc) = self#core_type a acc in
                    let (b,acc) = self#variance b acc in ((a, b), acc)))
              ptype_params acc in
          let (ptype_cstrs,acc) =
            (self#list
               (fun (a,b,c)  ->
                  fun acc  ->
                    let (a,acc) = self#core_type a acc in
                    let (b,acc) = self#core_type b acc in
                    let (c,acc) = self#location c acc in ((a, b, c), acc)))
              ptype_cstrs acc in
          let (ptype_kind,acc) = self#type_kind ptype_kind acc in
          let (ptype_private,acc) = self#private_flag ptype_private acc in
          let (ptype_manifest,acc) =
            (self#option self#core_type) ptype_manifest acc in
          let (ptype_attributes,acc) = self#attributes ptype_attributes acc in
          let (ptype_loc,acc) = self#location ptype_loc acc in
          ({
             ptype_name;
             ptype_params;
             ptype_cstrs;
             ptype_kind;
             ptype_private;
             ptype_manifest;
             ptype_attributes;
             ptype_loc
           }, acc)
    method type_extension: type_extension -> 'acc -> (type_extension* 'acc) =
      fun
        { ptyext_path; ptyext_params; ptyext_constructors; ptyext_private;
          ptyext_attributes }
         ->
        fun acc  ->
          let (ptyext_path,acc) = (self#loc self#longident) ptyext_path acc in
          let (ptyext_params,acc) =
            (self#list
               (fun (a,b)  ->
                  fun acc  ->
                    let (a,acc) = self#core_type a acc in
                    let (b,acc) = self#variance b acc in ((a, b), acc)))
              ptyext_params acc in
          let (ptyext_constructors,acc) =
            (self#list self#extension_constructor) ptyext_constructors acc in
          let (ptyext_private,acc) = self#private_flag ptyext_private acc in
          let (ptyext_attributes,acc) = self#attributes ptyext_attributes acc in
          ({
             ptyext_path;
             ptyext_params;
             ptyext_constructors;
             ptyext_private;
             ptyext_attributes
           }, acc)
    method type_kind: type_kind -> 'acc -> (type_kind* 'acc) =
      fun x  ->
        fun acc  ->
          match x with
          | Ptype_abstract  -> (Ptype_abstract, acc)
          | Ptype_variant (a) ->
              let (a,acc) = (self#list self#constructor_declaration) a acc in
              ((Ptype_variant (a)), acc)
          | Ptype_record (a) ->
              let (a,acc) = (self#list self#label_declaration) a acc in
              ((Ptype_record (a)), acc)
          | Ptype_open  -> (Ptype_open, acc)
    method value_binding: value_binding -> 'acc -> (value_binding* 'acc) =
      fun { pvb_pat; pvb_expr; pvb_attributes; pvb_loc }  ->
        fun acc  ->
          let (pvb_pat,acc) = self#pattern pvb_pat acc in
          let (pvb_expr,acc) = self#expression pvb_expr acc in
          let (pvb_attributes,acc) = self#attributes pvb_attributes acc in
          let (pvb_loc,acc) = self#location pvb_loc acc in
          ({ pvb_pat; pvb_expr; pvb_attributes; pvb_loc }, acc)
    method value_description:
      value_description -> 'acc -> (value_description* 'acc) =
      fun { pval_name; pval_type; pval_prim; pval_attributes; pval_loc }  ->
        fun acc  ->
          let (pval_name,acc) = (self#loc self#string) pval_name acc in
          let (pval_type,acc) = self#core_type pval_type acc in
          let (pval_prim,acc) = (self#list self#string) pval_prim acc in
          let (pval_attributes,acc) = self#attributes pval_attributes acc in
          let (pval_loc,acc) = self#location pval_loc acc in
          ({ pval_name; pval_type; pval_prim; pval_attributes; pval_loc },
            acc)
    method variance: Asttypes.variance -> 'acc -> (Asttypes.variance* 'acc) =
      fun x  ->
        fun acc  ->
          match x with
          | Covariant  -> (Covariant, acc)
          | Contravariant  -> (Contravariant, acc)
          | Invariant  -> (Invariant, acc)
    method virtual_flag:
      Asttypes.virtual_flag -> 'acc -> (Asttypes.virtual_flag* 'acc) =
      fun x  ->
        fun acc  ->
          match x with
          | Virtual  -> (Virtual, acc)
          | Concrete  -> (Concrete, acc)
    method with_constraint:
      with_constraint -> 'acc -> (with_constraint* 'acc) =
      fun x  ->
        fun acc  ->
          match x with
          | Pwith_type (a,b) ->
              let (a,acc) = (self#loc self#longident) a acc in
              let (b,acc) = self#type_declaration b acc in
              ((Pwith_type (a, b)), acc)
          | Pwith_module (a,b) ->
              let (a,acc) = (self#loc self#longident) a acc in
              let (b,acc) = (self#loc self#longident) b acc in
              ((Pwith_module (a, b)), acc)
          | Pwith_typesubst (a) ->
              let (a,acc) = self#type_declaration a acc in
              ((Pwith_typesubst (a)), acc)
          | Pwith_modsubst (a,b) ->
              let (a,acc) = (self#loc self#string) a acc in
              let (b,acc) = (self#loc self#longident) b acc in
              ((Pwith_modsubst (a, b)), acc)
  end
