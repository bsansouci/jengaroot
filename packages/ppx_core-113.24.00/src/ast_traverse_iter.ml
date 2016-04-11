open Parsetree
class t =
  object (self)
    method attribute: attribute -> unit =
      fun (a,b)  -> (self#loc self#string) a; self#payload b
    method attributes: attributes -> unit = self#list self#attribute
    method bool: bool -> unit =
      fun x  -> match x with | false  -> () | true  -> ()
    method case: case -> unit =
      fun { pc_lhs; pc_guard; pc_rhs }  ->
        self#pattern pc_lhs;
        (self#option self#expression) pc_guard;
        self#expression pc_rhs
    method char: char -> unit = ignore
    method class_declaration: class_declaration -> unit =
      self#class_infos self#class_expr
    method class_description: class_description -> unit =
      self#class_infos self#class_type
    method class_expr: class_expr -> unit =
      fun { pcl_desc; pcl_loc; pcl_attributes }  ->
        self#class_expr_desc pcl_desc;
        self#location pcl_loc;
        self#attributes pcl_attributes
    method class_expr_desc: class_expr_desc -> unit =
      fun x  ->
        match x with
        | Pcl_constr (a,b) ->
            ((self#loc self#longident) a; (self#list self#core_type) b)
        | Pcl_structure (a) -> self#class_structure a
        | Pcl_fun (a,b,c,d) ->
            (self#label a;
             (self#option self#expression) b;
             self#pattern c;
             self#class_expr d)
        | Pcl_apply (a,b) ->
            (self#class_expr a;
             (self#list (fun (a,b)  -> self#label a; self#expression b)) b)
        | Pcl_let (a,b,c) ->
            (self#rec_flag a;
             (self#list self#value_binding) b;
             self#class_expr c)
        | Pcl_constraint (a,b) -> (self#class_expr a; self#class_type b)
        | Pcl_extension (a) -> self#extension a
    method class_field: class_field -> unit =
      fun { pcf_desc; pcf_loc; pcf_attributes }  ->
        self#class_field_desc pcf_desc;
        self#location pcf_loc;
        self#attributes pcf_attributes
    method class_field_desc: class_field_desc -> unit =
      fun x  ->
        match x with
        | Pcf_inherit (a,b,c) ->
            (self#override_flag a;
             self#class_expr b;
             (self#option self#string) c)
        | Pcf_val (a) ->
            ((fun (a,b,c)  ->
                (self#loc self#string) a;
                self#mutable_flag b;
                self#class_field_kind c)) a
        | Pcf_method (a) ->
            ((fun (a,b,c)  ->
                (self#loc self#string) a;
                self#private_flag b;
                self#class_field_kind c)) a
        | Pcf_constraint (a) ->
            ((fun (a,b)  -> self#core_type a; self#core_type b)) a
        | Pcf_initializer (a) -> self#expression a
        | Pcf_attribute (a) -> self#attribute a
        | Pcf_extension (a) -> self#extension a
    method class_field_kind: class_field_kind -> unit =
      fun x  ->
        match x with
        | Cfk_virtual (a) -> self#core_type a
        | Cfk_concrete (a,b) -> (self#override_flag a; self#expression b)
    method class_infos: 'a . ('a -> unit) -> 'a class_infos -> unit =
      fun map_a  ->
        fun
          { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes
            }
           ->
          self#virtual_flag pci_virt;
          (self#list (fun (a,b)  -> self#core_type a; self#variance b))
            pci_params;
          (self#loc self#string) pci_name;
          map_a pci_expr;
          self#location pci_loc;
          self#attributes pci_attributes
    method class_signature: class_signature -> unit =
      fun { pcsig_self; pcsig_fields }  ->
        self#core_type pcsig_self;
        (self#list self#class_type_field) pcsig_fields
    method class_structure: class_structure -> unit =
      fun { pcstr_self; pcstr_fields }  ->
        self#pattern pcstr_self; (self#list self#class_field) pcstr_fields
    method class_type: class_type -> unit =
      fun { pcty_desc; pcty_loc; pcty_attributes }  ->
        self#class_type_desc pcty_desc;
        self#location pcty_loc;
        self#attributes pcty_attributes
    method class_type_declaration: class_type_declaration -> unit =
      self#class_infos self#class_type
    method class_type_desc: class_type_desc -> unit =
      fun x  ->
        match x with
        | Pcty_constr (a,b) ->
            ((self#loc self#longident) a; (self#list self#core_type) b)
        | Pcty_signature (a) -> self#class_signature a
        | Pcty_arrow (a,b,c) ->
            (self#label a; self#core_type b; self#class_type c)
        | Pcty_extension (a) -> self#extension a
    method class_type_field: class_type_field -> unit =
      fun { pctf_desc; pctf_loc; pctf_attributes }  ->
        self#class_type_field_desc pctf_desc;
        self#location pctf_loc;
        self#attributes pctf_attributes
    method class_type_field_desc: class_type_field_desc -> unit =
      fun x  ->
        match x with
        | Pctf_inherit (a) -> self#class_type a
        | Pctf_val (a) ->
            ((fun (a,b,c,d)  ->
                self#string a;
                self#mutable_flag b;
                self#virtual_flag c;
                self#core_type d)) a
        | Pctf_method (a) ->
            ((fun (a,b,c,d)  ->
                self#string a;
                self#private_flag b;
                self#virtual_flag c;
                self#core_type d)) a
        | Pctf_constraint (a) ->
            ((fun (a,b)  -> self#core_type a; self#core_type b)) a
        | Pctf_attribute (a) -> self#attribute a
        | Pctf_extension (a) -> self#extension a
    method closed_flag: Asttypes.closed_flag -> unit =
      fun x  -> match x with | Closed  -> () | Open  -> ()
    method constant: Asttypes.constant -> unit =
      fun x  ->
        match x with
        | Const_int (a) -> self#int a
        | Const_char (a) -> self#char a
        | Const_string (a,b) -> (self#string a; (self#option self#string) b)
        | Const_float (a) -> self#string a
        | Const_int32 (a) -> self#int32 a
        | Const_int64 (a) -> self#int64 a
        | Const_nativeint (a) -> self#nativeint a
    method constructor_declaration: constructor_declaration -> unit =
      fun { pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes }  ->
        (self#loc self#string) pcd_name;
        (self#list self#core_type) pcd_args;
        (self#option self#core_type) pcd_res;
        self#location pcd_loc;
        self#attributes pcd_attributes
    method core_type: core_type -> unit =
      fun { ptyp_desc; ptyp_loc; ptyp_attributes }  ->
        self#core_type_desc ptyp_desc;
        self#location ptyp_loc;
        self#attributes ptyp_attributes
    method core_type_desc: core_type_desc -> unit =
      fun x  ->
        match x with
        | Ptyp_any  -> ()
        | Ptyp_var (a) -> self#string a
        | Ptyp_arrow (a,b,c) ->
            (self#label a; self#core_type b; self#core_type c)
        | Ptyp_tuple (a) -> (self#list self#core_type) a
        | Ptyp_constr (a,b) ->
            ((self#loc self#longident) a; (self#list self#core_type) b)
        | Ptyp_object (a,b) ->
            ((self#list
                (fun (a,b,c)  ->
                   self#string a; self#attributes b; self#core_type c)) a;
             self#closed_flag b)
        | Ptyp_class (a,b) ->
            ((self#loc self#longident) a; (self#list self#core_type) b)
        | Ptyp_alias (a,b) -> (self#core_type a; self#string b)
        | Ptyp_variant (a,b,c) ->
            ((self#list self#row_field) a;
             self#closed_flag b;
             (self#option (self#list self#label)) c)
        | Ptyp_poly (a,b) -> ((self#list self#string) a; self#core_type b)
        | Ptyp_package (a) -> self#package_type a
        | Ptyp_extension (a) -> self#extension a
    method direction_flag: Asttypes.direction_flag -> unit =
      fun x  -> match x with | Upto  -> () | Downto  -> ()
    method directive_argument: directive_argument -> unit =
      fun x  ->
        match x with
        | Pdir_none  -> ()
        | Pdir_string (a) -> self#string a
        | Pdir_int (a) -> self#int a
        | Pdir_ident (a) -> self#longident a
        | Pdir_bool (a) -> self#bool a
    method expression: expression -> unit =
      fun { pexp_desc; pexp_loc; pexp_attributes }  ->
        self#expression_desc pexp_desc;
        self#location pexp_loc;
        self#attributes pexp_attributes
    method expression_desc: expression_desc -> unit =
      fun x  ->
        match x with
        | Pexp_ident (a) -> (self#loc self#longident) a
        | Pexp_constant (a) -> self#constant a
        | Pexp_let (a,b,c) ->
            (self#rec_flag a;
             (self#list self#value_binding) b;
             self#expression c)
        | Pexp_function (a) -> (self#list self#case) a
        | Pexp_fun (a,b,c,d) ->
            (self#label a;
             (self#option self#expression) b;
             self#pattern c;
             self#expression d)
        | Pexp_apply (a,b) ->
            (self#expression a;
             (self#list (fun (a,b)  -> self#label a; self#expression b)) b)
        | Pexp_match (a,b) -> (self#expression a; (self#list self#case) b)
        | Pexp_try (a,b) -> (self#expression a; (self#list self#case) b)
        | Pexp_tuple (a) -> (self#list self#expression) a
        | Pexp_construct (a,b) ->
            ((self#loc self#longident) a; (self#option self#expression) b)
        | Pexp_variant (a,b) ->
            (self#label a; (self#option self#expression) b)
        | Pexp_record (a,b) ->
            ((self#list
                (fun (a,b)  -> (self#loc self#longident) a; self#expression b))
               a;
             (self#option self#expression) b)
        | Pexp_field (a,b) ->
            (self#expression a; (self#loc self#longident) b)
        | Pexp_setfield (a,b,c) ->
            (self#expression a;
             (self#loc self#longident) b;
             self#expression c)
        | Pexp_array (a) -> (self#list self#expression) a
        | Pexp_ifthenelse (a,b,c) ->
            (self#expression a;
             self#expression b;
             (self#option self#expression) c)
        | Pexp_sequence (a,b) -> (self#expression a; self#expression b)
        | Pexp_while (a,b) -> (self#expression a; self#expression b)
        | Pexp_for (a,b,c,d,e) ->
            (self#pattern a;
             self#expression b;
             self#expression c;
             self#direction_flag d;
             self#expression e)
        | Pexp_constraint (a,b) -> (self#expression a; self#core_type b)
        | Pexp_coerce (a,b,c) ->
            (self#expression a;
             (self#option self#core_type) b;
             self#core_type c)
        | Pexp_send (a,b) -> (self#expression a; self#string b)
        | Pexp_new (a) -> (self#loc self#longident) a
        | Pexp_setinstvar (a,b) ->
            ((self#loc self#string) a; self#expression b)
        | Pexp_override (a) ->
            (self#list
               (fun (a,b)  -> (self#loc self#string) a; self#expression b)) a
        | Pexp_letmodule (a,b,c) ->
            ((self#loc self#string) a; self#module_expr b; self#expression c)
        | Pexp_assert (a) -> self#expression a
        | Pexp_lazy (a) -> self#expression a
        | Pexp_poly (a,b) ->
            (self#expression a; (self#option self#core_type) b)
        | Pexp_object (a) -> self#class_structure a
        | Pexp_newtype (a,b) -> (self#string a; self#expression b)
        | Pexp_pack (a) -> self#module_expr a
        | Pexp_open (a,b,c) ->
            (self#override_flag a;
             (self#loc self#longident) b;
             self#expression c)
        | Pexp_extension (a) -> self#extension a
    method extension: extension -> unit =
      fun (a,b)  -> (self#loc self#string) a; self#payload b
    method extension_constructor: extension_constructor -> unit =
      fun { pext_name; pext_kind; pext_loc; pext_attributes }  ->
        (self#loc self#string) pext_name;
        self#extension_constructor_kind pext_kind;
        self#location pext_loc;
        self#attributes pext_attributes
    method extension_constructor_kind: extension_constructor_kind -> unit =
      fun x  ->
        match x with
        | Pext_decl (a,b) ->
            ((self#list self#core_type) a; (self#option self#core_type) b)
        | Pext_rebind (a) -> (self#loc self#longident) a
    method include_declaration: include_declaration -> unit =
      self#include_infos self#module_expr
    method include_description: include_description -> unit =
      self#include_infos self#module_type
    method include_infos: 'a . ('a -> unit) -> 'a include_infos -> unit =
      fun map_a  ->
        fun { pincl_mod; pincl_loc; pincl_attributes }  ->
          map_a pincl_mod;
          self#location pincl_loc;
          self#attributes pincl_attributes
    method int: int -> unit = ignore
    method int32: int32 -> unit = ignore
    method int64: int64 -> unit = ignore
    method label: Asttypes.label -> unit = self#string
    method label_declaration: label_declaration -> unit =
      fun { pld_name; pld_mutable; pld_type; pld_loc; pld_attributes }  ->
        (self#loc self#string) pld_name;
        self#mutable_flag pld_mutable;
        self#core_type pld_type;
        self#location pld_loc;
        self#attributes pld_attributes
    method lexing_position: Lexing.position -> unit =
      fun { pos_fname; pos_lnum; pos_bol; pos_cnum }  ->
        self#string pos_fname;
        self#int pos_lnum;
        self#int pos_bol;
        self#int pos_cnum
    method list: 'a . ('a -> unit) -> 'a list -> unit =
      fun map_a  ->
        fun x  ->
          match x with | [] -> () | a::b -> (map_a a; (self#list map_a) b)
    method loc: 'a . ('a -> unit) -> 'a Asttypes.loc -> unit =
      fun map_a  -> fun { txt; loc }  -> map_a txt; self#location loc
    method location: Location.t -> unit =
      fun { loc_start; loc_end; loc_ghost }  ->
        self#lexing_position loc_start;
        self#lexing_position loc_end;
        self#bool loc_ghost
    method longident: Longident.t -> unit =
      fun x  ->
        match x with
        | Lident (a) -> self#string a
        | Ldot (a,b) -> (self#longident a; self#string b)
        | Lapply (a,b) -> (self#longident a; self#longident b)
    method module_binding: module_binding -> unit =
      fun { pmb_name; pmb_expr; pmb_attributes; pmb_loc }  ->
        (self#loc self#string) pmb_name;
        self#module_expr pmb_expr;
        self#attributes pmb_attributes;
        self#location pmb_loc
    method module_declaration: module_declaration -> unit =
      fun { pmd_name; pmd_type; pmd_attributes; pmd_loc }  ->
        (self#loc self#string) pmd_name;
        self#module_type pmd_type;
        self#attributes pmd_attributes;
        self#location pmd_loc
    method module_expr: module_expr -> unit =
      fun { pmod_desc; pmod_loc; pmod_attributes }  ->
        self#module_expr_desc pmod_desc;
        self#location pmod_loc;
        self#attributes pmod_attributes
    method module_expr_desc: module_expr_desc -> unit =
      fun x  ->
        match x with
        | Pmod_ident (a) -> (self#loc self#longident) a
        | Pmod_structure (a) -> self#structure a
        | Pmod_functor (a,b,c) ->
            ((self#loc self#string) a;
             (self#option self#module_type) b;
             self#module_expr c)
        | Pmod_apply (a,b) -> (self#module_expr a; self#module_expr b)
        | Pmod_constraint (a,b) -> (self#module_expr a; self#module_type b)
        | Pmod_unpack (a) -> self#expression a
        | Pmod_extension (a) -> self#extension a
    method module_type: module_type -> unit =
      fun { pmty_desc; pmty_loc; pmty_attributes }  ->
        self#module_type_desc pmty_desc;
        self#location pmty_loc;
        self#attributes pmty_attributes
    method module_type_declaration: module_type_declaration -> unit =
      fun { pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc }  ->
        (self#loc self#string) pmtd_name;
        (self#option self#module_type) pmtd_type;
        self#attributes pmtd_attributes;
        self#location pmtd_loc
    method module_type_desc: module_type_desc -> unit =
      fun x  ->
        match x with
        | Pmty_ident (a) -> (self#loc self#longident) a
        | Pmty_signature (a) -> self#signature a
        | Pmty_functor (a,b,c) ->
            ((self#loc self#string) a;
             (self#option self#module_type) b;
             self#module_type c)
        | Pmty_with (a,b) ->
            (self#module_type a; (self#list self#with_constraint) b)
        | Pmty_typeof (a) -> self#module_expr a
        | Pmty_extension (a) -> self#extension a
        | Pmty_alias (a) -> (self#loc self#longident) a
    method mutable_flag: Asttypes.mutable_flag -> unit =
      fun x  -> match x with | Immutable  -> () | Mutable  -> ()
    method nativeint: nativeint -> unit = ignore
    method open_description: open_description -> unit =
      fun { popen_lid; popen_override; popen_loc; popen_attributes }  ->
        (self#loc self#longident) popen_lid;
        self#override_flag popen_override;
        self#location popen_loc;
        self#attributes popen_attributes
    method option: 'a . ('a -> unit) -> 'a option -> unit =
      fun map_a  ->
        fun x  -> match x with | None  -> () | Some (a) -> map_a a
    method override_flag: Asttypes.override_flag -> unit =
      fun x  -> match x with | Override  -> () | Fresh  -> ()
    method package_type: package_type -> unit =
      fun (a,b)  ->
        (self#loc self#longident) a;
        (self#list
           (fun (a,b)  -> (self#loc self#longident) a; self#core_type b)) b
    method pattern: pattern -> unit =
      fun { ppat_desc; ppat_loc; ppat_attributes }  ->
        self#pattern_desc ppat_desc;
        self#location ppat_loc;
        self#attributes ppat_attributes
    method pattern_desc: pattern_desc -> unit =
      fun x  ->
        match x with
        | Ppat_any  -> ()
        | Ppat_var (a) -> (self#loc self#string) a
        | Ppat_alias (a,b) -> (self#pattern a; (self#loc self#string) b)
        | Ppat_constant (a) -> self#constant a
        | Ppat_interval (a,b) -> (self#constant a; self#constant b)
        | Ppat_tuple (a) -> (self#list self#pattern) a
        | Ppat_construct (a,b) ->
            ((self#loc self#longident) a; (self#option self#pattern) b)
        | Ppat_variant (a,b) -> (self#label a; (self#option self#pattern) b)
        | Ppat_record (a,b) ->
            ((self#list
                (fun (a,b)  -> (self#loc self#longident) a; self#pattern b))
               a;
             self#closed_flag b)
        | Ppat_array (a) -> (self#list self#pattern) a
        | Ppat_or (a,b) -> (self#pattern a; self#pattern b)
        | Ppat_constraint (a,b) -> (self#pattern a; self#core_type b)
        | Ppat_type (a) -> (self#loc self#longident) a
        | Ppat_lazy (a) -> self#pattern a
        | Ppat_unpack (a) -> (self#loc self#string) a
        | Ppat_exception (a) -> self#pattern a
        | Ppat_extension (a) -> self#extension a
    method payload: payload -> unit =
      fun x  ->
        match x with
        | PStr (a) -> self#structure a
        | PTyp (a) -> self#core_type a
        | PPat (a,b) -> (self#pattern a; (self#option self#expression) b)
    method private_flag: Asttypes.private_flag -> unit =
      fun x  -> match x with | Private  -> () | Public  -> ()
    method rec_flag: Asttypes.rec_flag -> unit =
      fun x  -> match x with | Nonrecursive  -> () | Recursive  -> ()
    method row_field: row_field -> unit =
      fun x  ->
        match x with
        | Rtag (a,b,c,d) ->
            (self#label a;
             self#attributes b;
             self#bool c;
             (self#list self#core_type) d)
        | Rinherit (a) -> self#core_type a
    method signature: signature -> unit = self#list self#signature_item
    method signature_item: signature_item -> unit =
      fun { psig_desc; psig_loc }  ->
        self#signature_item_desc psig_desc; self#location psig_loc
    method signature_item_desc: signature_item_desc -> unit =
      fun x  ->
        match x with
        | Psig_value (a) -> self#value_description a
        | Psig_type (a) -> (self#list self#type_declaration) a
        | Psig_typext (a) -> self#type_extension a
        | Psig_exception (a) -> self#extension_constructor a
        | Psig_module (a) -> self#module_declaration a
        | Psig_recmodule (a) -> (self#list self#module_declaration) a
        | Psig_modtype (a) -> self#module_type_declaration a
        | Psig_open (a) -> self#open_description a
        | Psig_include (a) -> self#include_description a
        | Psig_class (a) -> (self#list self#class_description) a
        | Psig_class_type (a) -> (self#list self#class_type_declaration) a
        | Psig_attribute (a) -> self#attribute a
        | Psig_extension (a,b) -> (self#extension a; self#attributes b)
    method string: string -> unit = ignore
    method structure: structure -> unit = self#list self#structure_item
    method structure_item: structure_item -> unit =
      fun { pstr_desc; pstr_loc }  ->
        self#structure_item_desc pstr_desc; self#location pstr_loc
    method structure_item_desc: structure_item_desc -> unit =
      fun x  ->
        match x with
        | Pstr_eval (a,b) -> (self#expression a; self#attributes b)
        | Pstr_value (a,b) ->
            (self#rec_flag a; (self#list self#value_binding) b)
        | Pstr_primitive (a) -> self#value_description a
        | Pstr_type (a) -> (self#list self#type_declaration) a
        | Pstr_typext (a) -> self#type_extension a
        | Pstr_exception (a) -> self#extension_constructor a
        | Pstr_module (a) -> self#module_binding a
        | Pstr_recmodule (a) -> (self#list self#module_binding) a
        | Pstr_modtype (a) -> self#module_type_declaration a
        | Pstr_open (a) -> self#open_description a
        | Pstr_class (a) -> (self#list self#class_declaration) a
        | Pstr_class_type (a) -> (self#list self#class_type_declaration) a
        | Pstr_include (a) -> self#include_declaration a
        | Pstr_attribute (a) -> self#attribute a
        | Pstr_extension (a,b) -> (self#extension a; self#attributes b)
    method toplevel_phrase: toplevel_phrase -> unit =
      fun x  ->
        match x with
        | Ptop_def (a) -> self#structure a
        | Ptop_dir (a,b) -> (self#string a; self#directive_argument b)
    method type_declaration: type_declaration -> unit =
      fun
        { ptype_name; ptype_params; ptype_cstrs; ptype_kind; ptype_private;
          ptype_manifest; ptype_attributes; ptype_loc }
         ->
        (self#loc self#string) ptype_name;
        (self#list (fun (a,b)  -> self#core_type a; self#variance b))
          ptype_params;
        (self#list
           (fun (a,b,c)  ->
              self#core_type a; self#core_type b; self#location c))
          ptype_cstrs;
        self#type_kind ptype_kind;
        self#private_flag ptype_private;
        (self#option self#core_type) ptype_manifest;
        self#attributes ptype_attributes;
        self#location ptype_loc
    method type_extension: type_extension -> unit =
      fun
        { ptyext_path; ptyext_params; ptyext_constructors; ptyext_private;
          ptyext_attributes }
         ->
        (self#loc self#longident) ptyext_path;
        (self#list (fun (a,b)  -> self#core_type a; self#variance b))
          ptyext_params;
        (self#list self#extension_constructor) ptyext_constructors;
        self#private_flag ptyext_private;
        self#attributes ptyext_attributes
    method type_kind: type_kind -> unit =
      fun x  ->
        match x with
        | Ptype_abstract  -> ()
        | Ptype_variant (a) -> (self#list self#constructor_declaration) a
        | Ptype_record (a) -> (self#list self#label_declaration) a
        | Ptype_open  -> ()
    method value_binding: value_binding -> unit =
      fun { pvb_pat; pvb_expr; pvb_attributes; pvb_loc }  ->
        self#pattern pvb_pat;
        self#expression pvb_expr;
        self#attributes pvb_attributes;
        self#location pvb_loc
    method value_description: value_description -> unit =
      fun { pval_name; pval_type; pval_prim; pval_attributes; pval_loc }  ->
        (self#loc self#string) pval_name;
        self#core_type pval_type;
        (self#list self#string) pval_prim;
        self#attributes pval_attributes;
        self#location pval_loc
    method variance: Asttypes.variance -> unit =
      fun x  ->
        match x with
        | Covariant  -> ()
        | Contravariant  -> ()
        | Invariant  -> ()
    method virtual_flag: Asttypes.virtual_flag -> unit =
      fun x  -> match x with | Virtual  -> () | Concrete  -> ()
    method with_constraint: with_constraint -> unit =
      fun x  ->
        match x with
        | Pwith_type (a,b) ->
            ((self#loc self#longident) a; self#type_declaration b)
        | Pwith_module (a,b) ->
            ((self#loc self#longident) a; (self#loc self#longident) b)
        | Pwith_typesubst (a) -> self#type_declaration a
        | Pwith_modsubst (a,b) ->
            ((self#loc self#string) a; (self#loc self#longident) b)
  end
