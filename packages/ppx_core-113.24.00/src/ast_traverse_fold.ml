open Parsetree
class ['acc] t =
  object (self)
    method attribute: attribute -> 'acc -> 'acc =
      fun (a,b)  ->
        fun acc  ->
          let acc = (self#loc self#string) a acc in
          let acc = self#payload b acc in acc
    method attributes: attributes -> 'acc -> 'acc = self#list self#attribute
    method bool: bool -> 'acc -> 'acc =
      fun x  -> fun acc  -> match x with | false  -> acc | true  -> acc
    method case: case -> 'acc -> 'acc =
      fun { pc_lhs; pc_guard; pc_rhs }  ->
        fun acc  ->
          let acc = self#pattern pc_lhs acc in
          let acc = (self#option self#expression) pc_guard acc in
          let acc = self#expression pc_rhs acc in acc
    method char: char -> 'acc -> 'acc = fun _  -> fun acc  -> acc
    method class_declaration: class_declaration -> 'acc -> 'acc =
      self#class_infos self#class_expr
    method class_description: class_description -> 'acc -> 'acc =
      self#class_infos self#class_type
    method class_expr: class_expr -> 'acc -> 'acc =
      fun { pcl_desc; pcl_loc; pcl_attributes }  ->
        fun acc  ->
          let acc = self#class_expr_desc pcl_desc acc in
          let acc = self#location pcl_loc acc in
          let acc = self#attributes pcl_attributes acc in acc
    method class_expr_desc: class_expr_desc -> 'acc -> 'acc =
      fun x  ->
        fun acc  ->
          match x with
          | Pcl_constr (a,b) ->
              let acc = (self#loc self#longident) a acc in
              let acc = (self#list self#core_type) b acc in acc
          | Pcl_structure (a) -> self#class_structure a acc
          | Pcl_fun (a,b,c,d) ->
              let acc = self#label a acc in
              let acc = (self#option self#expression) b acc in
              let acc = self#pattern c acc in
              let acc = self#class_expr d acc in acc
          | Pcl_apply (a,b) ->
              let acc = self#class_expr a acc in
              let acc =
                (self#list
                   (fun (a,b)  ->
                      fun acc  ->
                        let acc = self#label a acc in
                        let acc = self#expression b acc in acc)) b acc in
              acc
          | Pcl_let (a,b,c) ->
              let acc = self#rec_flag a acc in
              let acc = (self#list self#value_binding) b acc in
              let acc = self#class_expr c acc in acc
          | Pcl_constraint (a,b) ->
              let acc = self#class_expr a acc in
              let acc = self#class_type b acc in acc
          | Pcl_extension (a) -> self#extension a acc
    method class_field: class_field -> 'acc -> 'acc =
      fun { pcf_desc; pcf_loc; pcf_attributes }  ->
        fun acc  ->
          let acc = self#class_field_desc pcf_desc acc in
          let acc = self#location pcf_loc acc in
          let acc = self#attributes pcf_attributes acc in acc
    method class_field_desc: class_field_desc -> 'acc -> 'acc =
      fun x  ->
        fun acc  ->
          match x with
          | Pcf_inherit (a,b,c) ->
              let acc = self#override_flag a acc in
              let acc = self#class_expr b acc in
              let acc = (self#option self#string) c acc in acc
          | Pcf_val (a) ->
              ((fun (a,b,c)  ->
                  fun acc  ->
                    let acc = (self#loc self#string) a acc in
                    let acc = self#mutable_flag b acc in
                    let acc = self#class_field_kind c acc in acc)) a acc
          | Pcf_method (a) ->
              ((fun (a,b,c)  ->
                  fun acc  ->
                    let acc = (self#loc self#string) a acc in
                    let acc = self#private_flag b acc in
                    let acc = self#class_field_kind c acc in acc)) a acc
          | Pcf_constraint (a) ->
              ((fun (a,b)  ->
                  fun acc  ->
                    let acc = self#core_type a acc in
                    let acc = self#core_type b acc in acc)) a acc
          | Pcf_initializer (a) -> self#expression a acc
          | Pcf_attribute (a) -> self#attribute a acc
          | Pcf_extension (a) -> self#extension a acc
    method class_field_kind: class_field_kind -> 'acc -> 'acc =
      fun x  ->
        fun acc  ->
          match x with
          | Cfk_virtual (a) -> self#core_type a acc
          | Cfk_concrete (a,b) ->
              let acc = self#override_flag a acc in
              let acc = self#expression b acc in acc
    method class_infos:
      'a . ('a -> 'acc -> 'acc) -> 'a class_infos -> 'acc -> 'acc =
      fun map_a  ->
        fun
          { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes
            }
           ->
          fun acc  ->
            let acc = self#virtual_flag pci_virt acc in
            let acc =
              (self#list
                 (fun (a,b)  ->
                    fun acc  ->
                      let acc = self#core_type a acc in
                      let acc = self#variance b acc in acc)) pci_params acc in
            let acc = (self#loc self#string) pci_name acc in
            let acc = map_a pci_expr acc in
            let acc = self#location pci_loc acc in
            let acc = self#attributes pci_attributes acc in acc
    method class_signature: class_signature -> 'acc -> 'acc =
      fun { pcsig_self; pcsig_fields }  ->
        fun acc  ->
          let acc = self#core_type pcsig_self acc in
          let acc = (self#list self#class_type_field) pcsig_fields acc in acc
    method class_structure: class_structure -> 'acc -> 'acc =
      fun { pcstr_self; pcstr_fields }  ->
        fun acc  ->
          let acc = self#pattern pcstr_self acc in
          let acc = (self#list self#class_field) pcstr_fields acc in acc
    method class_type: class_type -> 'acc -> 'acc =
      fun { pcty_desc; pcty_loc; pcty_attributes }  ->
        fun acc  ->
          let acc = self#class_type_desc pcty_desc acc in
          let acc = self#location pcty_loc acc in
          let acc = self#attributes pcty_attributes acc in acc
    method class_type_declaration: class_type_declaration -> 'acc -> 'acc =
      self#class_infos self#class_type
    method class_type_desc: class_type_desc -> 'acc -> 'acc =
      fun x  ->
        fun acc  ->
          match x with
          | Pcty_constr (a,b) ->
              let acc = (self#loc self#longident) a acc in
              let acc = (self#list self#core_type) b acc in acc
          | Pcty_signature (a) -> self#class_signature a acc
          | Pcty_arrow (a,b,c) ->
              let acc = self#label a acc in
              let acc = self#core_type b acc in
              let acc = self#class_type c acc in acc
          | Pcty_extension (a) -> self#extension a acc
    method class_type_field: class_type_field -> 'acc -> 'acc =
      fun { pctf_desc; pctf_loc; pctf_attributes }  ->
        fun acc  ->
          let acc = self#class_type_field_desc pctf_desc acc in
          let acc = self#location pctf_loc acc in
          let acc = self#attributes pctf_attributes acc in acc
    method class_type_field_desc: class_type_field_desc -> 'acc -> 'acc =
      fun x  ->
        fun acc  ->
          match x with
          | Pctf_inherit (a) -> self#class_type a acc
          | Pctf_val (a) ->
              ((fun (a,b,c,d)  ->
                  fun acc  ->
                    let acc = self#string a acc in
                    let acc = self#mutable_flag b acc in
                    let acc = self#virtual_flag c acc in
                    let acc = self#core_type d acc in acc)) a acc
          | Pctf_method (a) ->
              ((fun (a,b,c,d)  ->
                  fun acc  ->
                    let acc = self#string a acc in
                    let acc = self#private_flag b acc in
                    let acc = self#virtual_flag c acc in
                    let acc = self#core_type d acc in acc)) a acc
          | Pctf_constraint (a) ->
              ((fun (a,b)  ->
                  fun acc  ->
                    let acc = self#core_type a acc in
                    let acc = self#core_type b acc in acc)) a acc
          | Pctf_attribute (a) -> self#attribute a acc
          | Pctf_extension (a) -> self#extension a acc
    method closed_flag: Asttypes.closed_flag -> 'acc -> 'acc =
      fun x  -> fun acc  -> match x with | Closed  -> acc | Open  -> acc
    method constant: Asttypes.constant -> 'acc -> 'acc =
      fun x  ->
        fun acc  ->
          match x with
          | Const_int (a) -> self#int a acc
          | Const_char (a) -> self#char a acc
          | Const_string (a,b) ->
              let acc = self#string a acc in
              let acc = (self#option self#string) b acc in acc
          | Const_float (a) -> self#string a acc
          | Const_int32 (a) -> self#int32 a acc
          | Const_int64 (a) -> self#int64 a acc
          | Const_nativeint (a) -> self#nativeint a acc
    method constructor_declaration: constructor_declaration -> 'acc -> 'acc =
      fun { pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes }  ->
        fun acc  ->
          let acc = (self#loc self#string) pcd_name acc in
          let acc = (self#list self#core_type) pcd_args acc in
          let acc = (self#option self#core_type) pcd_res acc in
          let acc = self#location pcd_loc acc in
          let acc = self#attributes pcd_attributes acc in acc
    method core_type: core_type -> 'acc -> 'acc =
      fun { ptyp_desc; ptyp_loc; ptyp_attributes }  ->
        fun acc  ->
          let acc = self#core_type_desc ptyp_desc acc in
          let acc = self#location ptyp_loc acc in
          let acc = self#attributes ptyp_attributes acc in acc
    method core_type_desc: core_type_desc -> 'acc -> 'acc =
      fun x  ->
        fun acc  ->
          match x with
          | Ptyp_any  -> acc
          | Ptyp_var (a) -> self#string a acc
          | Ptyp_arrow (a,b,c) ->
              let acc = self#label a acc in
              let acc = self#core_type b acc in
              let acc = self#core_type c acc in acc
          | Ptyp_tuple (a) -> (self#list self#core_type) a acc
          | Ptyp_constr (a,b) ->
              let acc = (self#loc self#longident) a acc in
              let acc = (self#list self#core_type) b acc in acc
          | Ptyp_object (a,b) ->
              let acc =
                (self#list
                   (fun (a,b,c)  ->
                      fun acc  ->
                        let acc = self#string a acc in
                        let acc = self#attributes b acc in
                        let acc = self#core_type c acc in acc)) a acc in
              let acc = self#closed_flag b acc in acc
          | Ptyp_class (a,b) ->
              let acc = (self#loc self#longident) a acc in
              let acc = (self#list self#core_type) b acc in acc
          | Ptyp_alias (a,b) ->
              let acc = self#core_type a acc in
              let acc = self#string b acc in acc
          | Ptyp_variant (a,b,c) ->
              let acc = (self#list self#row_field) a acc in
              let acc = self#closed_flag b acc in
              let acc = (self#option (self#list self#label)) c acc in acc
          | Ptyp_poly (a,b) ->
              let acc = (self#list self#string) a acc in
              let acc = self#core_type b acc in acc
          | Ptyp_package (a) -> self#package_type a acc
          | Ptyp_extension (a) -> self#extension a acc
    method direction_flag: Asttypes.direction_flag -> 'acc -> 'acc =
      fun x  -> fun acc  -> match x with | Upto  -> acc | Downto  -> acc
    method directive_argument: directive_argument -> 'acc -> 'acc =
      fun x  ->
        fun acc  ->
          match x with
          | Pdir_none  -> acc
          | Pdir_string (a) -> self#string a acc
          | Pdir_int (a) -> self#int a acc
          | Pdir_ident (a) -> self#longident a acc
          | Pdir_bool (a) -> self#bool a acc
    method expression: expression -> 'acc -> 'acc =
      fun { pexp_desc; pexp_loc; pexp_attributes }  ->
        fun acc  ->
          let acc = self#expression_desc pexp_desc acc in
          let acc = self#location pexp_loc acc in
          let acc = self#attributes pexp_attributes acc in acc
    method expression_desc: expression_desc -> 'acc -> 'acc =
      fun x  ->
        fun acc  ->
          match x with
          | Pexp_ident (a) -> (self#loc self#longident) a acc
          | Pexp_constant (a) -> self#constant a acc
          | Pexp_let (a,b,c) ->
              let acc = self#rec_flag a acc in
              let acc = (self#list self#value_binding) b acc in
              let acc = self#expression c acc in acc
          | Pexp_function (a) -> (self#list self#case) a acc
          | Pexp_fun (a,b,c,d) ->
              let acc = self#label a acc in
              let acc = (self#option self#expression) b acc in
              let acc = self#pattern c acc in
              let acc = self#expression d acc in acc
          | Pexp_apply (a,b) ->
              let acc = self#expression a acc in
              let acc =
                (self#list
                   (fun (a,b)  ->
                      fun acc  ->
                        let acc = self#label a acc in
                        let acc = self#expression b acc in acc)) b acc in
              acc
          | Pexp_match (a,b) ->
              let acc = self#expression a acc in
              let acc = (self#list self#case) b acc in acc
          | Pexp_try (a,b) ->
              let acc = self#expression a acc in
              let acc = (self#list self#case) b acc in acc
          | Pexp_tuple (a) -> (self#list self#expression) a acc
          | Pexp_construct (a,b) ->
              let acc = (self#loc self#longident) a acc in
              let acc = (self#option self#expression) b acc in acc
          | Pexp_variant (a,b) ->
              let acc = self#label a acc in
              let acc = (self#option self#expression) b acc in acc
          | Pexp_record (a,b) ->
              let acc =
                (self#list
                   (fun (a,b)  ->
                      fun acc  ->
                        let acc = (self#loc self#longident) a acc in
                        let acc = self#expression b acc in acc)) a acc in
              let acc = (self#option self#expression) b acc in acc
          | Pexp_field (a,b) ->
              let acc = self#expression a acc in
              let acc = (self#loc self#longident) b acc in acc
          | Pexp_setfield (a,b,c) ->
              let acc = self#expression a acc in
              let acc = (self#loc self#longident) b acc in
              let acc = self#expression c acc in acc
          | Pexp_array (a) -> (self#list self#expression) a acc
          | Pexp_ifthenelse (a,b,c) ->
              let acc = self#expression a acc in
              let acc = self#expression b acc in
              let acc = (self#option self#expression) c acc in acc
          | Pexp_sequence (a,b) ->
              let acc = self#expression a acc in
              let acc = self#expression b acc in acc
          | Pexp_while (a,b) ->
              let acc = self#expression a acc in
              let acc = self#expression b acc in acc
          | Pexp_for (a,b,c,d,e) ->
              let acc = self#pattern a acc in
              let acc = self#expression b acc in
              let acc = self#expression c acc in
              let acc = self#direction_flag d acc in
              let acc = self#expression e acc in acc
          | Pexp_constraint (a,b) ->
              let acc = self#expression a acc in
              let acc = self#core_type b acc in acc
          | Pexp_coerce (a,b,c) ->
              let acc = self#expression a acc in
              let acc = (self#option self#core_type) b acc in
              let acc = self#core_type c acc in acc
          | Pexp_send (a,b) ->
              let acc = self#expression a acc in
              let acc = self#string b acc in acc
          | Pexp_new (a) -> (self#loc self#longident) a acc
          | Pexp_setinstvar (a,b) ->
              let acc = (self#loc self#string) a acc in
              let acc = self#expression b acc in acc
          | Pexp_override (a) ->
              (self#list
                 (fun (a,b)  ->
                    fun acc  ->
                      let acc = (self#loc self#string) a acc in
                      let acc = self#expression b acc in acc)) a acc
          | Pexp_letmodule (a,b,c) ->
              let acc = (self#loc self#string) a acc in
              let acc = self#module_expr b acc in
              let acc = self#expression c acc in acc
          | Pexp_assert (a) -> self#expression a acc
          | Pexp_lazy (a) -> self#expression a acc
          | Pexp_poly (a,b) ->
              let acc = self#expression a acc in
              let acc = (self#option self#core_type) b acc in acc
          | Pexp_object (a) -> self#class_structure a acc
          | Pexp_newtype (a,b) ->
              let acc = self#string a acc in
              let acc = self#expression b acc in acc
          | Pexp_pack (a) -> self#module_expr a acc
          | Pexp_open (a,b,c) ->
              let acc = self#override_flag a acc in
              let acc = (self#loc self#longident) b acc in
              let acc = self#expression c acc in acc
          | Pexp_extension (a) -> self#extension a acc
    method extension: extension -> 'acc -> 'acc =
      fun (a,b)  ->
        fun acc  ->
          let acc = (self#loc self#string) a acc in
          let acc = self#payload b acc in acc
    method extension_constructor: extension_constructor -> 'acc -> 'acc =
      fun { pext_name; pext_kind; pext_loc; pext_attributes }  ->
        fun acc  ->
          let acc = (self#loc self#string) pext_name acc in
          let acc = self#extension_constructor_kind pext_kind acc in
          let acc = self#location pext_loc acc in
          let acc = self#attributes pext_attributes acc in acc
    method extension_constructor_kind:
      extension_constructor_kind -> 'acc -> 'acc =
      fun x  ->
        fun acc  ->
          match x with
          | Pext_decl (a,b) ->
              let acc = (self#list self#core_type) a acc in
              let acc = (self#option self#core_type) b acc in acc
          | Pext_rebind (a) -> (self#loc self#longident) a acc
    method include_declaration: include_declaration -> 'acc -> 'acc =
      self#include_infos self#module_expr
    method include_description: include_description -> 'acc -> 'acc =
      self#include_infos self#module_type
    method include_infos:
      'a . ('a -> 'acc -> 'acc) -> 'a include_infos -> 'acc -> 'acc =
      fun map_a  ->
        fun { pincl_mod; pincl_loc; pincl_attributes }  ->
          fun acc  ->
            let acc = map_a pincl_mod acc in
            let acc = self#location pincl_loc acc in
            let acc = self#attributes pincl_attributes acc in acc
    method int: int -> 'acc -> 'acc = fun _  -> fun acc  -> acc
    method int32: int32 -> 'acc -> 'acc = fun _  -> fun acc  -> acc
    method int64: int64 -> 'acc -> 'acc = fun _  -> fun acc  -> acc
    method label: Asttypes.label -> 'acc -> 'acc = self#string
    method label_declaration: label_declaration -> 'acc -> 'acc =
      fun { pld_name; pld_mutable; pld_type; pld_loc; pld_attributes }  ->
        fun acc  ->
          let acc = (self#loc self#string) pld_name acc in
          let acc = self#mutable_flag pld_mutable acc in
          let acc = self#core_type pld_type acc in
          let acc = self#location pld_loc acc in
          let acc = self#attributes pld_attributes acc in acc
    method lexing_position: Lexing.position -> 'acc -> 'acc =
      fun { pos_fname; pos_lnum; pos_bol; pos_cnum }  ->
        fun acc  ->
          let acc = self#string pos_fname acc in
          let acc = self#int pos_lnum acc in
          let acc = self#int pos_bol acc in
          let acc = self#int pos_cnum acc in acc
    method list: 'a . ('a -> 'acc -> 'acc) -> 'a list -> 'acc -> 'acc =
      fun map_a  ->
        fun x  ->
          fun acc  ->
            match x with
            | [] -> acc
            | a::b ->
                let acc = map_a a acc in
                let acc = (self#list map_a) b acc in acc
    method loc: 'a . ('a -> 'acc -> 'acc) -> 'a Asttypes.loc -> 'acc -> 'acc
      =
      fun map_a  ->
        fun { txt; loc }  ->
          fun acc  ->
            let acc = map_a txt acc in let acc = self#location loc acc in acc
    method location: Location.t -> 'acc -> 'acc =
      fun { loc_start; loc_end; loc_ghost }  ->
        fun acc  ->
          let acc = self#lexing_position loc_start acc in
          let acc = self#lexing_position loc_end acc in
          let acc = self#bool loc_ghost acc in acc
    method longident: Longident.t -> 'acc -> 'acc =
      fun x  ->
        fun acc  ->
          match x with
          | Lident (a) -> self#string a acc
          | Ldot (a,b) ->
              let acc = self#longident a acc in
              let acc = self#string b acc in acc
          | Lapply (a,b) ->
              let acc = self#longident a acc in
              let acc = self#longident b acc in acc
    method module_binding: module_binding -> 'acc -> 'acc =
      fun { pmb_name; pmb_expr; pmb_attributes; pmb_loc }  ->
        fun acc  ->
          let acc = (self#loc self#string) pmb_name acc in
          let acc = self#module_expr pmb_expr acc in
          let acc = self#attributes pmb_attributes acc in
          let acc = self#location pmb_loc acc in acc
    method module_declaration: module_declaration -> 'acc -> 'acc =
      fun { pmd_name; pmd_type; pmd_attributes; pmd_loc }  ->
        fun acc  ->
          let acc = (self#loc self#string) pmd_name acc in
          let acc = self#module_type pmd_type acc in
          let acc = self#attributes pmd_attributes acc in
          let acc = self#location pmd_loc acc in acc
    method module_expr: module_expr -> 'acc -> 'acc =
      fun { pmod_desc; pmod_loc; pmod_attributes }  ->
        fun acc  ->
          let acc = self#module_expr_desc pmod_desc acc in
          let acc = self#location pmod_loc acc in
          let acc = self#attributes pmod_attributes acc in acc
    method module_expr_desc: module_expr_desc -> 'acc -> 'acc =
      fun x  ->
        fun acc  ->
          match x with
          | Pmod_ident (a) -> (self#loc self#longident) a acc
          | Pmod_structure (a) -> self#structure a acc
          | Pmod_functor (a,b,c) ->
              let acc = (self#loc self#string) a acc in
              let acc = (self#option self#module_type) b acc in
              let acc = self#module_expr c acc in acc
          | Pmod_apply (a,b) ->
              let acc = self#module_expr a acc in
              let acc = self#module_expr b acc in acc
          | Pmod_constraint (a,b) ->
              let acc = self#module_expr a acc in
              let acc = self#module_type b acc in acc
          | Pmod_unpack (a) -> self#expression a acc
          | Pmod_extension (a) -> self#extension a acc
    method module_type: module_type -> 'acc -> 'acc =
      fun { pmty_desc; pmty_loc; pmty_attributes }  ->
        fun acc  ->
          let acc = self#module_type_desc pmty_desc acc in
          let acc = self#location pmty_loc acc in
          let acc = self#attributes pmty_attributes acc in acc
    method module_type_declaration: module_type_declaration -> 'acc -> 'acc =
      fun { pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc }  ->
        fun acc  ->
          let acc = (self#loc self#string) pmtd_name acc in
          let acc = (self#option self#module_type) pmtd_type acc in
          let acc = self#attributes pmtd_attributes acc in
          let acc = self#location pmtd_loc acc in acc
    method module_type_desc: module_type_desc -> 'acc -> 'acc =
      fun x  ->
        fun acc  ->
          match x with
          | Pmty_ident (a) -> (self#loc self#longident) a acc
          | Pmty_signature (a) -> self#signature a acc
          | Pmty_functor (a,b,c) ->
              let acc = (self#loc self#string) a acc in
              let acc = (self#option self#module_type) b acc in
              let acc = self#module_type c acc in acc
          | Pmty_with (a,b) ->
              let acc = self#module_type a acc in
              let acc = (self#list self#with_constraint) b acc in acc
          | Pmty_typeof (a) -> self#module_expr a acc
          | Pmty_extension (a) -> self#extension a acc
          | Pmty_alias (a) -> (self#loc self#longident) a acc
    method mutable_flag: Asttypes.mutable_flag -> 'acc -> 'acc =
      fun x  ->
        fun acc  -> match x with | Immutable  -> acc | Mutable  -> acc
    method nativeint: nativeint -> 'acc -> 'acc = fun _  -> fun acc  -> acc
    method open_description: open_description -> 'acc -> 'acc =
      fun { popen_lid; popen_override; popen_loc; popen_attributes }  ->
        fun acc  ->
          let acc = (self#loc self#longident) popen_lid acc in
          let acc = self#override_flag popen_override acc in
          let acc = self#location popen_loc acc in
          let acc = self#attributes popen_attributes acc in acc
    method option: 'a . ('a -> 'acc -> 'acc) -> 'a option -> 'acc -> 'acc =
      fun map_a  ->
        fun x  ->
          fun acc  -> match x with | None  -> acc | Some (a) -> map_a a acc
    method override_flag: Asttypes.override_flag -> 'acc -> 'acc =
      fun x  -> fun acc  -> match x with | Override  -> acc | Fresh  -> acc
    method package_type: package_type -> 'acc -> 'acc =
      fun (a,b)  ->
        fun acc  ->
          let acc = (self#loc self#longident) a acc in
          let acc =
            (self#list
               (fun (a,b)  ->
                  fun acc  ->
                    let acc = (self#loc self#longident) a acc in
                    let acc = self#core_type b acc in acc)) b acc in
          acc
    method pattern: pattern -> 'acc -> 'acc =
      fun { ppat_desc; ppat_loc; ppat_attributes }  ->
        fun acc  ->
          let acc = self#pattern_desc ppat_desc acc in
          let acc = self#location ppat_loc acc in
          let acc = self#attributes ppat_attributes acc in acc
    method pattern_desc: pattern_desc -> 'acc -> 'acc =
      fun x  ->
        fun acc  ->
          match x with
          | Ppat_any  -> acc
          | Ppat_var (a) -> (self#loc self#string) a acc
          | Ppat_alias (a,b) ->
              let acc = self#pattern a acc in
              let acc = (self#loc self#string) b acc in acc
          | Ppat_constant (a) -> self#constant a acc
          | Ppat_interval (a,b) ->
              let acc = self#constant a acc in
              let acc = self#constant b acc in acc
          | Ppat_tuple (a) -> (self#list self#pattern) a acc
          | Ppat_construct (a,b) ->
              let acc = (self#loc self#longident) a acc in
              let acc = (self#option self#pattern) b acc in acc
          | Ppat_variant (a,b) ->
              let acc = self#label a acc in
              let acc = (self#option self#pattern) b acc in acc
          | Ppat_record (a,b) ->
              let acc =
                (self#list
                   (fun (a,b)  ->
                      fun acc  ->
                        let acc = (self#loc self#longident) a acc in
                        let acc = self#pattern b acc in acc)) a acc in
              let acc = self#closed_flag b acc in acc
          | Ppat_array (a) -> (self#list self#pattern) a acc
          | Ppat_or (a,b) ->
              let acc = self#pattern a acc in
              let acc = self#pattern b acc in acc
          | Ppat_constraint (a,b) ->
              let acc = self#pattern a acc in
              let acc = self#core_type b acc in acc
          | Ppat_type (a) -> (self#loc self#longident) a acc
          | Ppat_lazy (a) -> self#pattern a acc
          | Ppat_unpack (a) -> (self#loc self#string) a acc
          | Ppat_exception (a) -> self#pattern a acc
          | Ppat_extension (a) -> self#extension a acc
    method payload: payload -> 'acc -> 'acc =
      fun x  ->
        fun acc  ->
          match x with
          | PStr (a) -> self#structure a acc
          | PTyp (a) -> self#core_type a acc
          | PPat (a,b) ->
              let acc = self#pattern a acc in
              let acc = (self#option self#expression) b acc in acc
    method private_flag: Asttypes.private_flag -> 'acc -> 'acc =
      fun x  -> fun acc  -> match x with | Private  -> acc | Public  -> acc
    method rec_flag: Asttypes.rec_flag -> 'acc -> 'acc =
      fun x  ->
        fun acc  -> match x with | Nonrecursive  -> acc | Recursive  -> acc
    method row_field: row_field -> 'acc -> 'acc =
      fun x  ->
        fun acc  ->
          match x with
          | Rtag (a,b,c,d) ->
              let acc = self#label a acc in
              let acc = self#attributes b acc in
              let acc = self#bool c acc in
              let acc = (self#list self#core_type) d acc in acc
          | Rinherit (a) -> self#core_type a acc
    method signature: signature -> 'acc -> 'acc =
      self#list self#signature_item
    method signature_item: signature_item -> 'acc -> 'acc =
      fun { psig_desc; psig_loc }  ->
        fun acc  ->
          let acc = self#signature_item_desc psig_desc acc in
          let acc = self#location psig_loc acc in acc
    method signature_item_desc: signature_item_desc -> 'acc -> 'acc =
      fun x  ->
        fun acc  ->
          match x with
          | Psig_value (a) -> self#value_description a acc
          | Psig_type (a) -> (self#list self#type_declaration) a acc
          | Psig_typext (a) -> self#type_extension a acc
          | Psig_exception (a) -> self#extension_constructor a acc
          | Psig_module (a) -> self#module_declaration a acc
          | Psig_recmodule (a) -> (self#list self#module_declaration) a acc
          | Psig_modtype (a) -> self#module_type_declaration a acc
          | Psig_open (a) -> self#open_description a acc
          | Psig_include (a) -> self#include_description a acc
          | Psig_class (a) -> (self#list self#class_description) a acc
          | Psig_class_type (a) ->
              (self#list self#class_type_declaration) a acc
          | Psig_attribute (a) -> self#attribute a acc
          | Psig_extension (a,b) ->
              let acc = self#extension a acc in
              let acc = self#attributes b acc in acc
    method string: string -> 'acc -> 'acc = fun _  -> fun acc  -> acc
    method structure: structure -> 'acc -> 'acc =
      self#list self#structure_item
    method structure_item: structure_item -> 'acc -> 'acc =
      fun { pstr_desc; pstr_loc }  ->
        fun acc  ->
          let acc = self#structure_item_desc pstr_desc acc in
          let acc = self#location pstr_loc acc in acc
    method structure_item_desc: structure_item_desc -> 'acc -> 'acc =
      fun x  ->
        fun acc  ->
          match x with
          | Pstr_eval (a,b) ->
              let acc = self#expression a acc in
              let acc = self#attributes b acc in acc
          | Pstr_value (a,b) ->
              let acc = self#rec_flag a acc in
              let acc = (self#list self#value_binding) b acc in acc
          | Pstr_primitive (a) -> self#value_description a acc
          | Pstr_type (a) -> (self#list self#type_declaration) a acc
          | Pstr_typext (a) -> self#type_extension a acc
          | Pstr_exception (a) -> self#extension_constructor a acc
          | Pstr_module (a) -> self#module_binding a acc
          | Pstr_recmodule (a) -> (self#list self#module_binding) a acc
          | Pstr_modtype (a) -> self#module_type_declaration a acc
          | Pstr_open (a) -> self#open_description a acc
          | Pstr_class (a) -> (self#list self#class_declaration) a acc
          | Pstr_class_type (a) ->
              (self#list self#class_type_declaration) a acc
          | Pstr_include (a) -> self#include_declaration a acc
          | Pstr_attribute (a) -> self#attribute a acc
          | Pstr_extension (a,b) ->
              let acc = self#extension a acc in
              let acc = self#attributes b acc in acc
    method toplevel_phrase: toplevel_phrase -> 'acc -> 'acc =
      fun x  ->
        fun acc  ->
          match x with
          | Ptop_def (a) -> self#structure a acc
          | Ptop_dir (a,b) ->
              let acc = self#string a acc in
              let acc = self#directive_argument b acc in acc
    method type_declaration: type_declaration -> 'acc -> 'acc =
      fun
        { ptype_name; ptype_params; ptype_cstrs; ptype_kind; ptype_private;
          ptype_manifest; ptype_attributes; ptype_loc }
         ->
        fun acc  ->
          let acc = (self#loc self#string) ptype_name acc in
          let acc =
            (self#list
               (fun (a,b)  ->
                  fun acc  ->
                    let acc = self#core_type a acc in
                    let acc = self#variance b acc in acc)) ptype_params acc in
          let acc =
            (self#list
               (fun (a,b,c)  ->
                  fun acc  ->
                    let acc = self#core_type a acc in
                    let acc = self#core_type b acc in
                    let acc = self#location c acc in acc)) ptype_cstrs acc in
          let acc = self#type_kind ptype_kind acc in
          let acc = self#private_flag ptype_private acc in
          let acc = (self#option self#core_type) ptype_manifest acc in
          let acc = self#attributes ptype_attributes acc in
          let acc = self#location ptype_loc acc in acc
    method type_extension: type_extension -> 'acc -> 'acc =
      fun
        { ptyext_path; ptyext_params; ptyext_constructors; ptyext_private;
          ptyext_attributes }
         ->
        fun acc  ->
          let acc = (self#loc self#longident) ptyext_path acc in
          let acc =
            (self#list
               (fun (a,b)  ->
                  fun acc  ->
                    let acc = self#core_type a acc in
                    let acc = self#variance b acc in acc)) ptyext_params acc in
          let acc =
            (self#list self#extension_constructor) ptyext_constructors acc in
          let acc = self#private_flag ptyext_private acc in
          let acc = self#attributes ptyext_attributes acc in acc
    method type_kind: type_kind -> 'acc -> 'acc =
      fun x  ->
        fun acc  ->
          match x with
          | Ptype_abstract  -> acc
          | Ptype_variant (a) ->
              (self#list self#constructor_declaration) a acc
          | Ptype_record (a) -> (self#list self#label_declaration) a acc
          | Ptype_open  -> acc
    method value_binding: value_binding -> 'acc -> 'acc =
      fun { pvb_pat; pvb_expr; pvb_attributes; pvb_loc }  ->
        fun acc  ->
          let acc = self#pattern pvb_pat acc in
          let acc = self#expression pvb_expr acc in
          let acc = self#attributes pvb_attributes acc in
          let acc = self#location pvb_loc acc in acc
    method value_description: value_description -> 'acc -> 'acc =
      fun { pval_name; pval_type; pval_prim; pval_attributes; pval_loc }  ->
        fun acc  ->
          let acc = (self#loc self#string) pval_name acc in
          let acc = self#core_type pval_type acc in
          let acc = (self#list self#string) pval_prim acc in
          let acc = self#attributes pval_attributes acc in
          let acc = self#location pval_loc acc in acc
    method variance: Asttypes.variance -> 'acc -> 'acc =
      fun x  ->
        fun acc  ->
          match x with
          | Covariant  -> acc
          | Contravariant  -> acc
          | Invariant  -> acc
    method virtual_flag: Asttypes.virtual_flag -> 'acc -> 'acc =
      fun x  -> fun acc  -> match x with | Virtual  -> acc | Concrete  -> acc
    method with_constraint: with_constraint -> 'acc -> 'acc =
      fun x  ->
        fun acc  ->
          match x with
          | Pwith_type (a,b) ->
              let acc = (self#loc self#longident) a acc in
              let acc = self#type_declaration b acc in acc
          | Pwith_module (a,b) ->
              let acc = (self#loc self#longident) a acc in
              let acc = (self#loc self#longident) b acc in acc
          | Pwith_typesubst (a) -> self#type_declaration a acc
          | Pwith_modsubst (a,b) ->
              let acc = (self#loc self#string) a acc in
              let acc = (self#loc self#longident) b acc in acc
  end
