open Parsetree
open Ast_pattern0
let false_ =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             match x with
             | false  -> (ctx.matched <- ctx.matched + 1; k)
             | _ -> fail loc "false")
let true_ =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             match x with
             | true  -> (ctx.matched <- ctx.matched + 1; k)
             | _ -> fail loc "true")
let case ~lhs:(T lhs)  ~guard:(T guard)  ~rhs:(T rhs)  =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             let k = lhs ctx loc x.pc_lhs k in
             let k = guard ctx loc x.pc_guard k in
             let k = rhs ctx loc x.pc_rhs k in k)
let pcl_loc (T f1) (T f2) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.pcl_loc in
             let k = f1 ctx loc loc k in let k = f2 ctx loc x k in k)
let pcl_attributes (T f1) (T f2) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.pcl_loc in
             let k = f1 ctx loc x.pcl_attributes k in
             let x = { x with pcl_attributes = [] } in
             let k = f2 ctx loc x k in k)
let pcl_constr (T f0) (T f1) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pcl_attributes;
             (let loc = x.pcl_loc in
              let x = x.pcl_desc in
              match x with
              | Pcl_constr (x0,x1) ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx x0.loc x0.txt k in
                    let k = f1 ctx loc x1 k in k))
              | _ -> fail loc "constr"))
let pcl_structure (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pcl_attributes;
             (let loc = x.pcl_loc in
              let x = x.pcl_desc in
              match x with
              | Pcl_structure x0 ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in k))
              | _ -> fail loc "structure"))
let pcl_fun (T f0) (T f1) (T f2) (T f3) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pcl_attributes;
             (let loc = x.pcl_loc in
              let x = x.pcl_desc in
              match x with
              | Pcl_fun (x0,x1,x2,x3) ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in
                    let k = f1 ctx loc x1 k in
                    let k = f2 ctx loc x2 k in let k = f3 ctx loc x3 k in k))
              | _ -> fail loc "fun"))
let pcl_apply (T f0) (T f1) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pcl_attributes;
             (let loc = x.pcl_loc in
              let x = x.pcl_desc in
              match x with
              | Pcl_apply (x0,x1) ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in let k = f1 ctx loc x1 k in k))
              | _ -> fail loc "apply"))
let pcl_let (T f0) (T f1) (T f2) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pcl_attributes;
             (let loc = x.pcl_loc in
              let x = x.pcl_desc in
              match x with
              | Pcl_let (x0,x1,x2) ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in
                    let k = f1 ctx loc x1 k in let k = f2 ctx loc x2 k in k))
              | _ -> fail loc "let"))
let pcl_constraint (T f0) (T f1) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pcl_attributes;
             (let loc = x.pcl_loc in
              let x = x.pcl_desc in
              match x with
              | Pcl_constraint (x0,x1) ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in let k = f1 ctx loc x1 k in k))
              | _ -> fail loc "constraint"))
let pcl_extension (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pcl_attributes;
             (let loc = x.pcl_loc in
              let x = x.pcl_desc in
              match x with
              | Pcl_extension x0 ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in k))
              | _ -> fail loc "extension"))
let pcf_loc (T f1) (T f2) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.pcf_loc in
             let k = f1 ctx loc loc k in let k = f2 ctx loc x k in k)
let pcf_attributes (T f1) (T f2) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.pcf_loc in
             let k = f1 ctx loc x.pcf_attributes k in
             let x = { x with pcf_attributes = [] } in
             let k = f2 ctx loc x k in k)
let pcf_inherit (T f0) (T f1) (T f2) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pcf_attributes;
             (let loc = x.pcf_loc in
              let x = x.pcf_desc in
              match x with
              | Pcf_inherit (x0,x1,x2) ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in
                    let k = f1 ctx loc x1 k in let k = f2 ctx loc x2 k in k))
              | _ -> fail loc "inherit"))
let pcf_val (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pcf_attributes;
             (let loc = x.pcf_loc in
              let x = x.pcf_desc in
              match x with
              | Pcf_val x0 ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in k))
              | _ -> fail loc "val"))
let pcf_method (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pcf_attributes;
             (let loc = x.pcf_loc in
              let x = x.pcf_desc in
              match x with
              | Pcf_method x0 ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in k))
              | _ -> fail loc "method"))
let pcf_constraint (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pcf_attributes;
             (let loc = x.pcf_loc in
              let x = x.pcf_desc in
              match x with
              | Pcf_constraint x0 ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in k))
              | _ -> fail loc "constraint"))
let pcf_initializer (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pcf_attributes;
             (let loc = x.pcf_loc in
              let x = x.pcf_desc in
              match x with
              | Pcf_initializer x0 ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in k))
              | _ -> fail loc "initializer"))
let pcf_attribute (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pcf_attributes;
             (let loc = x.pcf_loc in
              let x = x.pcf_desc in
              match x with
              | Pcf_attribute x0 ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in k))
              | _ -> fail loc "attribute"))
let pcf_extension (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pcf_attributes;
             (let loc = x.pcf_loc in
              let x = x.pcf_desc in
              match x with
              | Pcf_extension x0 ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in k))
              | _ -> fail loc "extension"))
let cfk_virtual (T f0) =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             match x with
             | Cfk_virtual x0 ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx loc x0 k in k))
             | _ -> fail loc "virtual")
let cfk_concrete (T f0) (T f1) =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             match x with
             | Cfk_concrete (x0,x1) ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx loc x0 k in let k = f1 ctx loc x1 k in k))
             | _ -> fail loc "concrete")
let class_infos_attributes (T f1) (T f2) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.pci_loc in
             let k = f1 ctx loc x.pci_attributes k in
             let x = { x with pci_attributes = [] } in
             let k = f2 ctx loc x k in k)
let class_infos ~virt:(T virt)  ~params:(T params)  ~name:(T name)  ~expr:(T
  expr)  =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pci_attributes;
             (let k = virt ctx loc x.pci_virt k in
              let k = params ctx loc x.pci_params k in
              let k = name ctx (x.pci_name).loc (x.pci_name).txt k in
              let k = expr ctx loc x.pci_expr k in k))
let class_signature ~self:(T self)  ~fields:(T fields)  =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             let k = self ctx loc x.pcsig_self k in
             let k = fields ctx loc x.pcsig_fields k in k)
let class_structure ~self:(T self)  ~fields:(T fields)  =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             let k = self ctx loc x.pcstr_self k in
             let k = fields ctx loc x.pcstr_fields k in k)
let pcty_loc (T f1) (T f2) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.pcty_loc in
             let k = f1 ctx loc loc k in let k = f2 ctx loc x k in k)
let pcty_attributes (T f1) (T f2) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.pcty_loc in
             let k = f1 ctx loc x.pcty_attributes k in
             let x = { x with pcty_attributes = [] } in
             let k = f2 ctx loc x k in k)
let pcty_constr (T f0) (T f1) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pcty_attributes;
             (let loc = x.pcty_loc in
              let x = x.pcty_desc in
              match x with
              | Pcty_constr (x0,x1) ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx x0.loc x0.txt k in
                    let k = f1 ctx loc x1 k in k))
              | _ -> fail loc "constr"))
let pcty_signature (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pcty_attributes;
             (let loc = x.pcty_loc in
              let x = x.pcty_desc in
              match x with
              | Pcty_signature x0 ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in k))
              | _ -> fail loc "signature"))
let pcty_arrow (T f0) (T f1) (T f2) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pcty_attributes;
             (let loc = x.pcty_loc in
              let x = x.pcty_desc in
              match x with
              | Pcty_arrow (x0,x1,x2) ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in
                    let k = f1 ctx loc x1 k in let k = f2 ctx loc x2 k in k))
              | _ -> fail loc "arrow"))
let pcty_extension (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pcty_attributes;
             (let loc = x.pcty_loc in
              let x = x.pcty_desc in
              match x with
              | Pcty_extension x0 ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in k))
              | _ -> fail loc "extension"))
let pctf_loc (T f1) (T f2) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.pctf_loc in
             let k = f1 ctx loc loc k in let k = f2 ctx loc x k in k)
let pctf_attributes (T f1) (T f2) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.pctf_loc in
             let k = f1 ctx loc x.pctf_attributes k in
             let x = { x with pctf_attributes = [] } in
             let k = f2 ctx loc x k in k)
let pctf_inherit (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pctf_attributes;
             (let loc = x.pctf_loc in
              let x = x.pctf_desc in
              match x with
              | Pctf_inherit x0 ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in k))
              | _ -> fail loc "inherit"))
let pctf_val (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pctf_attributes;
             (let loc = x.pctf_loc in
              let x = x.pctf_desc in
              match x with
              | Pctf_val x0 ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in k))
              | _ -> fail loc "val"))
let pctf_method (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pctf_attributes;
             (let loc = x.pctf_loc in
              let x = x.pctf_desc in
              match x with
              | Pctf_method x0 ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in k))
              | _ -> fail loc "method"))
let pctf_constraint (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pctf_attributes;
             (let loc = x.pctf_loc in
              let x = x.pctf_desc in
              match x with
              | Pctf_constraint x0 ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in k))
              | _ -> fail loc "constraint"))
let pctf_attribute (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pctf_attributes;
             (let loc = x.pctf_loc in
              let x = x.pctf_desc in
              match x with
              | Pctf_attribute x0 ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in k))
              | _ -> fail loc "attribute"))
let pctf_extension (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pctf_attributes;
             (let loc = x.pctf_loc in
              let x = x.pctf_desc in
              match x with
              | Pctf_extension x0 ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in k))
              | _ -> fail loc "extension"))
let closed =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             match x with
             | Asttypes.Closed  -> (ctx.matched <- ctx.matched + 1; k)
             | _ -> fail loc "Closed")
let open_ =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             match x with
             | Asttypes.Open  -> (ctx.matched <- ctx.matched + 1; k)
             | _ -> fail loc "Open")
let const_int (T f0) =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             match x with
             | Asttypes.Const_int x0 ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx loc x0 k in k))
             | _ -> fail loc "int")
let const_char (T f0) =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             match x with
             | Asttypes.Const_char x0 ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx loc x0 k in k))
             | _ -> fail loc "char")
let const_string (T f0) (T f1) =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             match x with
             | Asttypes.Const_string (x0,x1) ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx loc x0 k in let k = f1 ctx loc x1 k in k))
             | _ -> fail loc "string")
let const_float (T f0) =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             match x with
             | Asttypes.Const_float x0 ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx loc x0 k in k))
             | _ -> fail loc "float")
let const_int32 (T f0) =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             match x with
             | Asttypes.Const_int32 x0 ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx loc x0 k in k))
             | _ -> fail loc "int32")
let const_int64 (T f0) =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             match x with
             | Asttypes.Const_int64 x0 ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx loc x0 k in k))
             | _ -> fail loc "int64")
let const_nativeint (T f0) =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             match x with
             | Asttypes.Const_nativeint x0 ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx loc x0 k in k))
             | _ -> fail loc "nativeint")
let constructor_declaration_attributes (T f1) (T f2) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.pcd_loc in
             let k = f1 ctx loc x.pcd_attributes k in
             let x = { x with pcd_attributes = [] } in
             let k = f2 ctx loc x k in k)
let constructor_declaration ~name:(T name)  ~args:(T args)  ~res:(T res)  =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pcd_attributes;
             (let k = name ctx (x.pcd_name).loc (x.pcd_name).txt k in
              let k = args ctx loc x.pcd_args k in
              let k = res ctx loc x.pcd_res k in k))
let ptyp_loc (T f1) (T f2) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.ptyp_loc in
             let k = f1 ctx loc loc k in let k = f2 ctx loc x k in k)
let ptyp_attributes (T f1) (T f2) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.ptyp_loc in
             let k = f1 ctx loc x.ptyp_attributes k in
             let x = { x with ptyp_attributes = [] } in
             let k = f2 ctx loc x k in k)
let ptyp_any =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.ptyp_attributes;
             (let loc = x.ptyp_loc in
              let x = x.ptyp_desc in
              match x with
              | Ptyp_any  -> (ctx.matched <- ctx.matched + 1; k)
              | _ -> fail loc "any"))
let ptyp_var (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.ptyp_attributes;
             (let loc = x.ptyp_loc in
              let x = x.ptyp_desc in
              match x with
              | Ptyp_var x0 ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in k))
              | _ -> fail loc "var"))
let ptyp_arrow (T f0) (T f1) (T f2) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.ptyp_attributes;
             (let loc = x.ptyp_loc in
              let x = x.ptyp_desc in
              match x with
              | Ptyp_arrow (x0,x1,x2) ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in
                    let k = f1 ctx loc x1 k in let k = f2 ctx loc x2 k in k))
              | _ -> fail loc "arrow"))
let ptyp_tuple (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.ptyp_attributes;
             (let loc = x.ptyp_loc in
              let x = x.ptyp_desc in
              match x with
              | Ptyp_tuple x0 ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in k))
              | _ -> fail loc "tuple"))
let ptyp_constr (T f0) (T f1) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.ptyp_attributes;
             (let loc = x.ptyp_loc in
              let x = x.ptyp_desc in
              match x with
              | Ptyp_constr (x0,x1) ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx x0.loc x0.txt k in
                    let k = f1 ctx loc x1 k in k))
              | _ -> fail loc "constr"))
let ptyp_object (T f0) (T f1) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.ptyp_attributes;
             (let loc = x.ptyp_loc in
              let x = x.ptyp_desc in
              match x with
              | Ptyp_object (x0,x1) ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in let k = f1 ctx loc x1 k in k))
              | _ -> fail loc "object"))
let ptyp_class (T f0) (T f1) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.ptyp_attributes;
             (let loc = x.ptyp_loc in
              let x = x.ptyp_desc in
              match x with
              | Ptyp_class (x0,x1) ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx x0.loc x0.txt k in
                    let k = f1 ctx loc x1 k in k))
              | _ -> fail loc "class"))
let ptyp_alias (T f0) (T f1) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.ptyp_attributes;
             (let loc = x.ptyp_loc in
              let x = x.ptyp_desc in
              match x with
              | Ptyp_alias (x0,x1) ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in let k = f1 ctx loc x1 k in k))
              | _ -> fail loc "alias"))
let ptyp_variant (T f0) (T f1) (T f2) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.ptyp_attributes;
             (let loc = x.ptyp_loc in
              let x = x.ptyp_desc in
              match x with
              | Ptyp_variant (x0,x1,x2) ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in
                    let k = f1 ctx loc x1 k in let k = f2 ctx loc x2 k in k))
              | _ -> fail loc "variant"))
let ptyp_poly (T f0) (T f1) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.ptyp_attributes;
             (let loc = x.ptyp_loc in
              let x = x.ptyp_desc in
              match x with
              | Ptyp_poly (x0,x1) ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in let k = f1 ctx loc x1 k in k))
              | _ -> fail loc "poly"))
let ptyp_package (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.ptyp_attributes;
             (let loc = x.ptyp_loc in
              let x = x.ptyp_desc in
              match x with
              | Ptyp_package x0 ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in k))
              | _ -> fail loc "package"))
let ptyp_extension (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.ptyp_attributes;
             (let loc = x.ptyp_loc in
              let x = x.ptyp_desc in
              match x with
              | Ptyp_extension x0 ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in k))
              | _ -> fail loc "extension"))
let upto =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             match x with
             | Asttypes.Upto  -> (ctx.matched <- ctx.matched + 1; k)
             | _ -> fail loc "Upto")
let downto_ =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             match x with
             | Asttypes.Downto  -> (ctx.matched <- ctx.matched + 1; k)
             | _ -> fail loc "Downto")
let pdir_none =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             match x with
             | Pdir_none  -> (ctx.matched <- ctx.matched + 1; k)
             | _ -> fail loc "none")
let pdir_string (T f0) =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             match x with
             | Pdir_string x0 ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx loc x0 k in k))
             | _ -> fail loc "string")
let pdir_int (T f0) =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             match x with
             | Pdir_int x0 ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx loc x0 k in k))
             | _ -> fail loc "int")
let pdir_ident (T f0) =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             match x with
             | Pdir_ident x0 ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx loc x0 k in k))
             | _ -> fail loc "ident")
let pdir_bool (T f0) =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             match x with
             | Pdir_bool x0 ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx loc x0 k in k))
             | _ -> fail loc "bool")
let pexp_loc (T f1) (T f2) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.pexp_loc in
             let k = f1 ctx loc loc k in let k = f2 ctx loc x k in k)
let pexp_attributes (T f1) (T f2) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.pexp_loc in
             let k = f1 ctx loc x.pexp_attributes k in
             let x = { x with pexp_attributes = [] } in
             let k = f2 ctx loc x k in k)
let pexp_ident (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pexp_attributes;
             (let loc = x.pexp_loc in
              let x = x.pexp_desc in
              match x with
              | Pexp_ident x0 ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx x0.loc x0.txt k in k))
              | _ -> fail loc "ident"))
let pexp_constant (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pexp_attributes;
             (let loc = x.pexp_loc in
              let x = x.pexp_desc in
              match x with
              | Pexp_constant x0 ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in k))
              | _ -> fail loc "constant"))
let pexp_let (T f0) (T f1) (T f2) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pexp_attributes;
             (let loc = x.pexp_loc in
              let x = x.pexp_desc in
              match x with
              | Pexp_let (x0,x1,x2) ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in
                    let k = f1 ctx loc x1 k in let k = f2 ctx loc x2 k in k))
              | _ -> fail loc "let"))
let pexp_function (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pexp_attributes;
             (let loc = x.pexp_loc in
              let x = x.pexp_desc in
              match x with
              | Pexp_function x0 ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in k))
              | _ -> fail loc "function"))
let pexp_fun (T f0) (T f1) (T f2) (T f3) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pexp_attributes;
             (let loc = x.pexp_loc in
              let x = x.pexp_desc in
              match x with
              | Pexp_fun (x0,x1,x2,x3) ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in
                    let k = f1 ctx loc x1 k in
                    let k = f2 ctx loc x2 k in let k = f3 ctx loc x3 k in k))
              | _ -> fail loc "fun"))
let pexp_apply (T f0) (T f1) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pexp_attributes;
             (let loc = x.pexp_loc in
              let x = x.pexp_desc in
              match x with
              | Pexp_apply (x0,x1) ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in let k = f1 ctx loc x1 k in k))
              | _ -> fail loc "apply"))
let pexp_match (T f0) (T f1) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pexp_attributes;
             (let loc = x.pexp_loc in
              let x = x.pexp_desc in
              match x with
              | Pexp_match (x0,x1) ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in let k = f1 ctx loc x1 k in k))
              | _ -> fail loc "match"))
let pexp_try (T f0) (T f1) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pexp_attributes;
             (let loc = x.pexp_loc in
              let x = x.pexp_desc in
              match x with
              | Pexp_try (x0,x1) ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in let k = f1 ctx loc x1 k in k))
              | _ -> fail loc "try"))
let pexp_tuple (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pexp_attributes;
             (let loc = x.pexp_loc in
              let x = x.pexp_desc in
              match x with
              | Pexp_tuple x0 ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in k))
              | _ -> fail loc "tuple"))
let pexp_construct (T f0) (T f1) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pexp_attributes;
             (let loc = x.pexp_loc in
              let x = x.pexp_desc in
              match x with
              | Pexp_construct (x0,x1) ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx x0.loc x0.txt k in
                    let k = f1 ctx loc x1 k in k))
              | _ -> fail loc "construct"))
let pexp_variant (T f0) (T f1) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pexp_attributes;
             (let loc = x.pexp_loc in
              let x = x.pexp_desc in
              match x with
              | Pexp_variant (x0,x1) ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in let k = f1 ctx loc x1 k in k))
              | _ -> fail loc "variant"))
let pexp_record (T f0) (T f1) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pexp_attributes;
             (let loc = x.pexp_loc in
              let x = x.pexp_desc in
              match x with
              | Pexp_record (x0,x1) ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in let k = f1 ctx loc x1 k in k))
              | _ -> fail loc "record"))
let pexp_field (T f0) (T f1) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pexp_attributes;
             (let loc = x.pexp_loc in
              let x = x.pexp_desc in
              match x with
              | Pexp_field (x0,x1) ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in
                    let k = f1 ctx x1.loc x1.txt k in k))
              | _ -> fail loc "field"))
let pexp_setfield (T f0) (T f1) (T f2) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pexp_attributes;
             (let loc = x.pexp_loc in
              let x = x.pexp_desc in
              match x with
              | Pexp_setfield (x0,x1,x2) ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in
                    let k = f1 ctx x1.loc x1.txt k in
                    let k = f2 ctx loc x2 k in k))
              | _ -> fail loc "setfield"))
let pexp_array (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pexp_attributes;
             (let loc = x.pexp_loc in
              let x = x.pexp_desc in
              match x with
              | Pexp_array x0 ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in k))
              | _ -> fail loc "array"))
let pexp_ifthenelse (T f0) (T f1) (T f2) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pexp_attributes;
             (let loc = x.pexp_loc in
              let x = x.pexp_desc in
              match x with
              | Pexp_ifthenelse (x0,x1,x2) ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in
                    let k = f1 ctx loc x1 k in let k = f2 ctx loc x2 k in k))
              | _ -> fail loc "ifthenelse"))
let pexp_sequence (T f0) (T f1) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pexp_attributes;
             (let loc = x.pexp_loc in
              let x = x.pexp_desc in
              match x with
              | Pexp_sequence (x0,x1) ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in let k = f1 ctx loc x1 k in k))
              | _ -> fail loc "sequence"))
let pexp_while (T f0) (T f1) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pexp_attributes;
             (let loc = x.pexp_loc in
              let x = x.pexp_desc in
              match x with
              | Pexp_while (x0,x1) ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in let k = f1 ctx loc x1 k in k))
              | _ -> fail loc "while"))
let pexp_for (T f0) (T f1) (T f2) (T f3) (T f4) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pexp_attributes;
             (let loc = x.pexp_loc in
              let x = x.pexp_desc in
              match x with
              | Pexp_for (x0,x1,x2,x3,x4) ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in
                    let k = f1 ctx loc x1 k in
                    let k = f2 ctx loc x2 k in
                    let k = f3 ctx loc x3 k in let k = f4 ctx loc x4 k in k))
              | _ -> fail loc "for"))
let pexp_constraint (T f0) (T f1) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pexp_attributes;
             (let loc = x.pexp_loc in
              let x = x.pexp_desc in
              match x with
              | Pexp_constraint (x0,x1) ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in let k = f1 ctx loc x1 k in k))
              | _ -> fail loc "constraint"))
let pexp_coerce (T f0) (T f1) (T f2) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pexp_attributes;
             (let loc = x.pexp_loc in
              let x = x.pexp_desc in
              match x with
              | Pexp_coerce (x0,x1,x2) ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in
                    let k = f1 ctx loc x1 k in let k = f2 ctx loc x2 k in k))
              | _ -> fail loc "coerce"))
let pexp_send (T f0) (T f1) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pexp_attributes;
             (let loc = x.pexp_loc in
              let x = x.pexp_desc in
              match x with
              | Pexp_send (x0,x1) ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in let k = f1 ctx loc x1 k in k))
              | _ -> fail loc "send"))
let pexp_new (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pexp_attributes;
             (let loc = x.pexp_loc in
              let x = x.pexp_desc in
              match x with
              | Pexp_new x0 ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx x0.loc x0.txt k in k))
              | _ -> fail loc "new"))
let pexp_setinstvar (T f0) (T f1) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pexp_attributes;
             (let loc = x.pexp_loc in
              let x = x.pexp_desc in
              match x with
              | Pexp_setinstvar (x0,x1) ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx x0.loc x0.txt k in
                    let k = f1 ctx loc x1 k in k))
              | _ -> fail loc "setinstvar"))
let pexp_override (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pexp_attributes;
             (let loc = x.pexp_loc in
              let x = x.pexp_desc in
              match x with
              | Pexp_override x0 ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in k))
              | _ -> fail loc "override"))
let pexp_letmodule (T f0) (T f1) (T f2) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pexp_attributes;
             (let loc = x.pexp_loc in
              let x = x.pexp_desc in
              match x with
              | Pexp_letmodule (x0,x1,x2) ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx x0.loc x0.txt k in
                    let k = f1 ctx loc x1 k in let k = f2 ctx loc x2 k in k))
              | _ -> fail loc "letmodule"))
let pexp_assert (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pexp_attributes;
             (let loc = x.pexp_loc in
              let x = x.pexp_desc in
              match x with
              | Pexp_assert x0 ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in k))
              | _ -> fail loc "assert"))
let pexp_lazy (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pexp_attributes;
             (let loc = x.pexp_loc in
              let x = x.pexp_desc in
              match x with
              | Pexp_lazy x0 ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in k))
              | _ -> fail loc "lazy"))
let pexp_poly (T f0) (T f1) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pexp_attributes;
             (let loc = x.pexp_loc in
              let x = x.pexp_desc in
              match x with
              | Pexp_poly (x0,x1) ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in let k = f1 ctx loc x1 k in k))
              | _ -> fail loc "poly"))
let pexp_object (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pexp_attributes;
             (let loc = x.pexp_loc in
              let x = x.pexp_desc in
              match x with
              | Pexp_object x0 ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in k))
              | _ -> fail loc "object"))
let pexp_newtype (T f0) (T f1) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pexp_attributes;
             (let loc = x.pexp_loc in
              let x = x.pexp_desc in
              match x with
              | Pexp_newtype (x0,x1) ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in let k = f1 ctx loc x1 k in k))
              | _ -> fail loc "newtype"))
let pexp_pack (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pexp_attributes;
             (let loc = x.pexp_loc in
              let x = x.pexp_desc in
              match x with
              | Pexp_pack x0 ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in k))
              | _ -> fail loc "pack"))
let pexp_open (T f0) (T f1) (T f2) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pexp_attributes;
             (let loc = x.pexp_loc in
              let x = x.pexp_desc in
              match x with
              | Pexp_open (x0,x1,x2) ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in
                    let k = f1 ctx x1.loc x1.txt k in
                    let k = f2 ctx loc x2 k in k))
              | _ -> fail loc "open"))
let pexp_extension (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pexp_attributes;
             (let loc = x.pexp_loc in
              let x = x.pexp_desc in
              match x with
              | Pexp_extension x0 ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in k))
              | _ -> fail loc "extension"))
let extension_constructor_attributes (T f1) (T f2) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.pext_loc in
             let k = f1 ctx loc x.pext_attributes k in
             let x = { x with pext_attributes = [] } in
             let k = f2 ctx loc x k in k)
let extension_constructor ~name:(T name)  ~kind:(T kind)  =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pext_attributes;
             (let k = name ctx (x.pext_name).loc (x.pext_name).txt k in
              let k = kind ctx loc x.pext_kind k in k))
let pext_decl (T f0) (T f1) =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             match x with
             | Pext_decl (x0,x1) ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx loc x0 k in let k = f1 ctx loc x1 k in k))
             | _ -> fail loc "decl")
let pext_rebind (T f0) =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             match x with
             | Pext_rebind x0 ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx x0.loc x0.txt k in k))
             | _ -> fail loc "rebind")
let include_infos_attributes (T f1) (T f2) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.pincl_loc in
             let k = f1 ctx loc x.pincl_attributes k in
             let x = { x with pincl_attributes = [] } in
             let k = f2 ctx loc x k in k)
let include_infos ~mod_:(T mod_)  =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pincl_attributes;
             (let k = mod_ ctx loc x.pincl_mod k in k))
let label_declaration_attributes (T f1) (T f2) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.pld_loc in
             let k = f1 ctx loc x.pld_attributes k in
             let x = { x with pld_attributes = [] } in
             let k = f2 ctx loc x k in k)
let label_declaration ~name:(T name)  ~mutable_:(T mutable_)  ~type_:(T
  type_)  =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pld_attributes;
             (let k = name ctx (x.pld_name).loc (x.pld_name).txt k in
              let k = mutable_ ctx loc x.pld_mutable k in
              let k = type_ ctx loc x.pld_type k in k))
let lexing_position ~fname:(T fname)  ~lnum:(T lnum)  ~bol:(T bol)  ~cnum:(T
  cnum)  =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             let k = fname ctx loc x.Lexing.pos_fname k in
             let k = lnum ctx loc x.Lexing.pos_lnum k in
             let k = bol ctx loc x.Lexing.pos_bol k in
             let k = cnum ctx loc x.Lexing.pos_cnum k in k)
let nil =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             match x with
             | [] -> (ctx.matched <- ctx.matched + 1; k)
             | _ -> fail loc "[]")
let cons (T f0) (T f1) =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             match x with
             | x0::x1 ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx loc x0 k in let k = f1 ctx loc x1 k in k))
             | _ -> fail loc "::")
let location ~start:(T start)  ~end_:(T end_)  ~ghost:(T ghost)  =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             let k = start ctx loc x.Location.loc_start k in
             let k = end_ ctx loc x.Location.loc_end k in
             let k = ghost ctx loc x.Location.loc_ghost k in k)
let lident (T f0) =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             match x with
             | Longident.Lident x0 ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx loc x0 k in k))
             | _ -> fail loc "Lident")
let ldot (T f0) (T f1) =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             match x with
             | Longident.Ldot (x0,x1) ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx loc x0 k in let k = f1 ctx loc x1 k in k))
             | _ -> fail loc "Ldot")
let lapply (T f0) (T f1) =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             match x with
             | Longident.Lapply (x0,x1) ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx loc x0 k in let k = f1 ctx loc x1 k in k))
             | _ -> fail loc "Lapply")
let module_binding_attributes (T f1) (T f2) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.pmb_loc in
             let k = f1 ctx loc x.pmb_attributes k in
             let x = { x with pmb_attributes = [] } in
             let k = f2 ctx loc x k in k)
let module_binding ~name:(T name)  ~expr:(T expr)  =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pmb_attributes;
             (let k = name ctx (x.pmb_name).loc (x.pmb_name).txt k in
              let k = expr ctx loc x.pmb_expr k in k))
let module_declaration_attributes (T f1) (T f2) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.pmd_loc in
             let k = f1 ctx loc x.pmd_attributes k in
             let x = { x with pmd_attributes = [] } in
             let k = f2 ctx loc x k in k)
let module_declaration ~name:(T name)  ~type_:(T type_)  =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pmd_attributes;
             (let k = name ctx (x.pmd_name).loc (x.pmd_name).txt k in
              let k = type_ ctx loc x.pmd_type k in k))
let pmod_loc (T f1) (T f2) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.pmod_loc in
             let k = f1 ctx loc loc k in let k = f2 ctx loc x k in k)
let pmod_attributes (T f1) (T f2) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.pmod_loc in
             let k = f1 ctx loc x.pmod_attributes k in
             let x = { x with pmod_attributes = [] } in
             let k = f2 ctx loc x k in k)
let pmod_ident (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pmod_attributes;
             (let loc = x.pmod_loc in
              let x = x.pmod_desc in
              match x with
              | Pmod_ident x0 ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx x0.loc x0.txt k in k))
              | _ -> fail loc "ident"))
let pmod_structure (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pmod_attributes;
             (let loc = x.pmod_loc in
              let x = x.pmod_desc in
              match x with
              | Pmod_structure x0 ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in k))
              | _ -> fail loc "structure"))
let pmod_functor (T f0) (T f1) (T f2) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pmod_attributes;
             (let loc = x.pmod_loc in
              let x = x.pmod_desc in
              match x with
              | Pmod_functor (x0,x1,x2) ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx x0.loc x0.txt k in
                    let k = f1 ctx loc x1 k in let k = f2 ctx loc x2 k in k))
              | _ -> fail loc "functor"))
let pmod_apply (T f0) (T f1) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pmod_attributes;
             (let loc = x.pmod_loc in
              let x = x.pmod_desc in
              match x with
              | Pmod_apply (x0,x1) ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in let k = f1 ctx loc x1 k in k))
              | _ -> fail loc "apply"))
let pmod_constraint (T f0) (T f1) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pmod_attributes;
             (let loc = x.pmod_loc in
              let x = x.pmod_desc in
              match x with
              | Pmod_constraint (x0,x1) ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in let k = f1 ctx loc x1 k in k))
              | _ -> fail loc "constraint"))
let pmod_unpack (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pmod_attributes;
             (let loc = x.pmod_loc in
              let x = x.pmod_desc in
              match x with
              | Pmod_unpack x0 ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in k))
              | _ -> fail loc "unpack"))
let pmod_extension (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pmod_attributes;
             (let loc = x.pmod_loc in
              let x = x.pmod_desc in
              match x with
              | Pmod_extension x0 ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in k))
              | _ -> fail loc "extension"))
let pmty_loc (T f1) (T f2) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.pmty_loc in
             let k = f1 ctx loc loc k in let k = f2 ctx loc x k in k)
let pmty_attributes (T f1) (T f2) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.pmty_loc in
             let k = f1 ctx loc x.pmty_attributes k in
             let x = { x with pmty_attributes = [] } in
             let k = f2 ctx loc x k in k)
let pmty_ident (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pmty_attributes;
             (let loc = x.pmty_loc in
              let x = x.pmty_desc in
              match x with
              | Pmty_ident x0 ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx x0.loc x0.txt k in k))
              | _ -> fail loc "ident"))
let pmty_signature (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pmty_attributes;
             (let loc = x.pmty_loc in
              let x = x.pmty_desc in
              match x with
              | Pmty_signature x0 ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in k))
              | _ -> fail loc "signature"))
let pmty_functor (T f0) (T f1) (T f2) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pmty_attributes;
             (let loc = x.pmty_loc in
              let x = x.pmty_desc in
              match x with
              | Pmty_functor (x0,x1,x2) ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx x0.loc x0.txt k in
                    let k = f1 ctx loc x1 k in let k = f2 ctx loc x2 k in k))
              | _ -> fail loc "functor"))
let pmty_with (T f0) (T f1) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pmty_attributes;
             (let loc = x.pmty_loc in
              let x = x.pmty_desc in
              match x with
              | Pmty_with (x0,x1) ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in let k = f1 ctx loc x1 k in k))
              | _ -> fail loc "with"))
let pmty_typeof (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pmty_attributes;
             (let loc = x.pmty_loc in
              let x = x.pmty_desc in
              match x with
              | Pmty_typeof x0 ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in k))
              | _ -> fail loc "typeof"))
let pmty_extension (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pmty_attributes;
             (let loc = x.pmty_loc in
              let x = x.pmty_desc in
              match x with
              | Pmty_extension x0 ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in k))
              | _ -> fail loc "extension"))
let pmty_alias (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pmty_attributes;
             (let loc = x.pmty_loc in
              let x = x.pmty_desc in
              match x with
              | Pmty_alias x0 ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx x0.loc x0.txt k in k))
              | _ -> fail loc "alias"))
let module_type_declaration_attributes (T f1) (T f2) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.pmtd_loc in
             let k = f1 ctx loc x.pmtd_attributes k in
             let x = { x with pmtd_attributes = [] } in
             let k = f2 ctx loc x k in k)
let module_type_declaration ~name:(T name)  ~type_:(T type_)  =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pmtd_attributes;
             (let k = name ctx (x.pmtd_name).loc (x.pmtd_name).txt k in
              let k = type_ ctx loc x.pmtd_type k in k))
let immutable =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             match x with
             | Asttypes.Immutable  -> (ctx.matched <- ctx.matched + 1; k)
             | _ -> fail loc "Immutable")
let mutable_ =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             match x with
             | Asttypes.Mutable  -> (ctx.matched <- ctx.matched + 1; k)
             | _ -> fail loc "Mutable")
let open_description_attributes (T f1) (T f2) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.popen_loc in
             let k = f1 ctx loc x.popen_attributes k in
             let x = { x with popen_attributes = [] } in
             let k = f2 ctx loc x k in k)
let open_description ~lid:(T lid)  ~override:(T override)  =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.popen_attributes;
             (let k = lid ctx (x.popen_lid).loc (x.popen_lid).txt k in
              let k = override ctx loc x.popen_override k in k))
let none =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             match x with
             | None  -> (ctx.matched <- ctx.matched + 1; k)
             | _ -> fail loc "None")
let some (T f0) =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             match x with
             | Some x0 ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx loc x0 k in k))
             | _ -> fail loc "Some")
let override =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             match x with
             | Asttypes.Override  -> (ctx.matched <- ctx.matched + 1; k)
             | _ -> fail loc "Override")
let fresh =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             match x with
             | Asttypes.Fresh  -> (ctx.matched <- ctx.matched + 1; k)
             | _ -> fail loc "Fresh")
let ppat_loc (T f1) (T f2) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.ppat_loc in
             let k = f1 ctx loc loc k in let k = f2 ctx loc x k in k)
let ppat_attributes (T f1) (T f2) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.ppat_loc in
             let k = f1 ctx loc x.ppat_attributes k in
             let x = { x with ppat_attributes = [] } in
             let k = f2 ctx loc x k in k)
let ppat_any =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.ppat_attributes;
             (let loc = x.ppat_loc in
              let x = x.ppat_desc in
              match x with
              | Ppat_any  -> (ctx.matched <- ctx.matched + 1; k)
              | _ -> fail loc "any"))
let ppat_var (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.ppat_attributes;
             (let loc = x.ppat_loc in
              let x = x.ppat_desc in
              match x with
              | Ppat_var x0 ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx x0.loc x0.txt k in k))
              | _ -> fail loc "var"))
let ppat_alias (T f0) (T f1) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.ppat_attributes;
             (let loc = x.ppat_loc in
              let x = x.ppat_desc in
              match x with
              | Ppat_alias (x0,x1) ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in
                    let k = f1 ctx x1.loc x1.txt k in k))
              | _ -> fail loc "alias"))
let ppat_constant (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.ppat_attributes;
             (let loc = x.ppat_loc in
              let x = x.ppat_desc in
              match x with
              | Ppat_constant x0 ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in k))
              | _ -> fail loc "constant"))
let ppat_interval (T f0) (T f1) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.ppat_attributes;
             (let loc = x.ppat_loc in
              let x = x.ppat_desc in
              match x with
              | Ppat_interval (x0,x1) ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in let k = f1 ctx loc x1 k in k))
              | _ -> fail loc "interval"))
let ppat_tuple (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.ppat_attributes;
             (let loc = x.ppat_loc in
              let x = x.ppat_desc in
              match x with
              | Ppat_tuple x0 ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in k))
              | _ -> fail loc "tuple"))
let ppat_construct (T f0) (T f1) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.ppat_attributes;
             (let loc = x.ppat_loc in
              let x = x.ppat_desc in
              match x with
              | Ppat_construct (x0,x1) ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx x0.loc x0.txt k in
                    let k = f1 ctx loc x1 k in k))
              | _ -> fail loc "construct"))
let ppat_variant (T f0) (T f1) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.ppat_attributes;
             (let loc = x.ppat_loc in
              let x = x.ppat_desc in
              match x with
              | Ppat_variant (x0,x1) ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in let k = f1 ctx loc x1 k in k))
              | _ -> fail loc "variant"))
let ppat_record (T f0) (T f1) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.ppat_attributes;
             (let loc = x.ppat_loc in
              let x = x.ppat_desc in
              match x with
              | Ppat_record (x0,x1) ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in let k = f1 ctx loc x1 k in k))
              | _ -> fail loc "record"))
let ppat_array (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.ppat_attributes;
             (let loc = x.ppat_loc in
              let x = x.ppat_desc in
              match x with
              | Ppat_array x0 ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in k))
              | _ -> fail loc "array"))
let ppat_or (T f0) (T f1) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.ppat_attributes;
             (let loc = x.ppat_loc in
              let x = x.ppat_desc in
              match x with
              | Ppat_or (x0,x1) ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in let k = f1 ctx loc x1 k in k))
              | _ -> fail loc "or"))
let ppat_constraint (T f0) (T f1) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.ppat_attributes;
             (let loc = x.ppat_loc in
              let x = x.ppat_desc in
              match x with
              | Ppat_constraint (x0,x1) ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in let k = f1 ctx loc x1 k in k))
              | _ -> fail loc "constraint"))
let ppat_type (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.ppat_attributes;
             (let loc = x.ppat_loc in
              let x = x.ppat_desc in
              match x with
              | Ppat_type x0 ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx x0.loc x0.txt k in k))
              | _ -> fail loc "type"))
let ppat_lazy (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.ppat_attributes;
             (let loc = x.ppat_loc in
              let x = x.ppat_desc in
              match x with
              | Ppat_lazy x0 ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in k))
              | _ -> fail loc "lazy"))
let ppat_unpack (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.ppat_attributes;
             (let loc = x.ppat_loc in
              let x = x.ppat_desc in
              match x with
              | Ppat_unpack x0 ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx x0.loc x0.txt k in k))
              | _ -> fail loc "unpack"))
let ppat_exception (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.ppat_attributes;
             (let loc = x.ppat_loc in
              let x = x.ppat_desc in
              match x with
              | Ppat_exception x0 ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in k))
              | _ -> fail loc "exception"))
let ppat_extension (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.ppat_attributes;
             (let loc = x.ppat_loc in
              let x = x.ppat_desc in
              match x with
              | Ppat_extension x0 ->
                  (ctx.matched <- ctx.matched + 1;
                   (let k = f0 ctx loc x0 k in k))
              | _ -> fail loc "extension"))
let pstr (T f0) =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             match x with
             | PStr x0 ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx loc x0 k in k))
             | _ -> fail loc "PStr")
let ptyp (T f0) =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             match x with
             | PTyp x0 ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx loc x0 k in k))
             | _ -> fail loc "PTyp")
let ppat (T f0) (T f1) =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             match x with
             | PPat (x0,x1) ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx loc x0 k in let k = f1 ctx loc x1 k in k))
             | _ -> fail loc "PPat")
let private_ =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             match x with
             | Asttypes.Private  -> (ctx.matched <- ctx.matched + 1; k)
             | _ -> fail loc "Private")
let public =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             match x with
             | Asttypes.Public  -> (ctx.matched <- ctx.matched + 1; k)
             | _ -> fail loc "Public")
let nonrecursive =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             match x with
             | Asttypes.Nonrecursive  -> (ctx.matched <- ctx.matched + 1; k)
             | _ -> fail loc "Nonrecursive")
let recursive =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             match x with
             | Asttypes.Recursive  -> (ctx.matched <- ctx.matched + 1; k)
             | _ -> fail loc "Recursive")
let rtag (T f0) (T f1) (T f2) (T f3) =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             match x with
             | Rtag (x0,x1,x2,x3) ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx loc x0 k in
                   let k = f1 ctx loc x1 k in
                   let k = f2 ctx loc x2 k in let k = f3 ctx loc x3 k in k))
             | _ -> fail loc "Rtag")
let rinherit (T f0) =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             match x with
             | Rinherit x0 ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx loc x0 k in k))
             | _ -> fail loc "Rinherit")
let psig_loc (T f1) (T f2) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.psig_loc in
             let k = f1 ctx loc loc k in let k = f2 ctx loc x k in k)
let psig_value (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.psig_loc in
             let x = x.psig_desc in
             match x with
             | Psig_value x0 ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx loc x0 k in k))
             | _ -> fail loc "value")
let psig_type (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.psig_loc in
             let x = x.psig_desc in
             match x with
             | Psig_type x0 ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx loc x0 k in k))
             | _ -> fail loc "type")
let psig_typext (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.psig_loc in
             let x = x.psig_desc in
             match x with
             | Psig_typext x0 ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx loc x0 k in k))
             | _ -> fail loc "typext")
let psig_exception (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.psig_loc in
             let x = x.psig_desc in
             match x with
             | Psig_exception x0 ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx loc x0 k in k))
             | _ -> fail loc "exception")
let psig_module (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.psig_loc in
             let x = x.psig_desc in
             match x with
             | Psig_module x0 ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx loc x0 k in k))
             | _ -> fail loc "module")
let psig_recmodule (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.psig_loc in
             let x = x.psig_desc in
             match x with
             | Psig_recmodule x0 ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx loc x0 k in k))
             | _ -> fail loc "recmodule")
let psig_modtype (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.psig_loc in
             let x = x.psig_desc in
             match x with
             | Psig_modtype x0 ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx loc x0 k in k))
             | _ -> fail loc "modtype")
let psig_open (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.psig_loc in
             let x = x.psig_desc in
             match x with
             | Psig_open x0 ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx loc x0 k in k))
             | _ -> fail loc "open")
let psig_include (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.psig_loc in
             let x = x.psig_desc in
             match x with
             | Psig_include x0 ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx loc x0 k in k))
             | _ -> fail loc "include")
let psig_class (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.psig_loc in
             let x = x.psig_desc in
             match x with
             | Psig_class x0 ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx loc x0 k in k))
             | _ -> fail loc "class")
let psig_class_type (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.psig_loc in
             let x = x.psig_desc in
             match x with
             | Psig_class_type x0 ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx loc x0 k in k))
             | _ -> fail loc "class_type")
let psig_attribute (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.psig_loc in
             let x = x.psig_desc in
             match x with
             | Psig_attribute x0 ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx loc x0 k in k))
             | _ -> fail loc "attribute")
let psig_extension (T f0) (T f1) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.psig_loc in
             let x = x.psig_desc in
             match x with
             | Psig_extension (x0,x1) ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx loc x0 k in let k = f1 ctx loc x1 k in k))
             | _ -> fail loc "extension")
let pstr_loc (T f1) (T f2) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.pstr_loc in
             let k = f1 ctx loc loc k in let k = f2 ctx loc x k in k)
let pstr_eval (T f0) (T f1) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.pstr_loc in
             let x = x.pstr_desc in
             match x with
             | Pstr_eval (x0,x1) ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx loc x0 k in let k = f1 ctx loc x1 k in k))
             | _ -> fail loc "eval")
let pstr_value (T f0) (T f1) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.pstr_loc in
             let x = x.pstr_desc in
             match x with
             | Pstr_value (x0,x1) ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx loc x0 k in let k = f1 ctx loc x1 k in k))
             | _ -> fail loc "value")
let pstr_primitive (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.pstr_loc in
             let x = x.pstr_desc in
             match x with
             | Pstr_primitive x0 ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx loc x0 k in k))
             | _ -> fail loc "primitive")
let pstr_type (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.pstr_loc in
             let x = x.pstr_desc in
             match x with
             | Pstr_type x0 ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx loc x0 k in k))
             | _ -> fail loc "type")
let pstr_typext (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.pstr_loc in
             let x = x.pstr_desc in
             match x with
             | Pstr_typext x0 ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx loc x0 k in k))
             | _ -> fail loc "typext")
let pstr_exception (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.pstr_loc in
             let x = x.pstr_desc in
             match x with
             | Pstr_exception x0 ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx loc x0 k in k))
             | _ -> fail loc "exception")
let pstr_module (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.pstr_loc in
             let x = x.pstr_desc in
             match x with
             | Pstr_module x0 ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx loc x0 k in k))
             | _ -> fail loc "module")
let pstr_recmodule (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.pstr_loc in
             let x = x.pstr_desc in
             match x with
             | Pstr_recmodule x0 ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx loc x0 k in k))
             | _ -> fail loc "recmodule")
let pstr_modtype (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.pstr_loc in
             let x = x.pstr_desc in
             match x with
             | Pstr_modtype x0 ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx loc x0 k in k))
             | _ -> fail loc "modtype")
let pstr_open (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.pstr_loc in
             let x = x.pstr_desc in
             match x with
             | Pstr_open x0 ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx loc x0 k in k))
             | _ -> fail loc "open")
let pstr_class (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.pstr_loc in
             let x = x.pstr_desc in
             match x with
             | Pstr_class x0 ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx loc x0 k in k))
             | _ -> fail loc "class")
let pstr_class_type (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.pstr_loc in
             let x = x.pstr_desc in
             match x with
             | Pstr_class_type x0 ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx loc x0 k in k))
             | _ -> fail loc "class_type")
let pstr_include (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.pstr_loc in
             let x = x.pstr_desc in
             match x with
             | Pstr_include x0 ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx loc x0 k in k))
             | _ -> fail loc "include")
let pstr_attribute (T f0) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.pstr_loc in
             let x = x.pstr_desc in
             match x with
             | Pstr_attribute x0 ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx loc x0 k in k))
             | _ -> fail loc "attribute")
let pstr_extension (T f0) (T f1) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.pstr_loc in
             let x = x.pstr_desc in
             match x with
             | Pstr_extension (x0,x1) ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx loc x0 k in let k = f1 ctx loc x1 k in k))
             | _ -> fail loc "extension")
let ptop_def (T f0) =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             match x with
             | Ptop_def x0 ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx loc x0 k in k))
             | _ -> fail loc "def")
let ptop_dir (T f0) (T f1) =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             match x with
             | Ptop_dir (x0,x1) ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx loc x0 k in let k = f1 ctx loc x1 k in k))
             | _ -> fail loc "dir")
let type_declaration_attributes (T f1) (T f2) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.ptype_loc in
             let k = f1 ctx loc x.ptype_attributes k in
             let x = { x with ptype_attributes = [] } in
             let k = f2 ctx loc x k in k)
let type_declaration ~name:(T name)  ~params:(T params)  ~cstrs:(T cstrs) 
  ~kind:(T kind)  ~private_:(T private_)  ~manifest:(T manifest)  =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.ptype_attributes;
             (let k = name ctx (x.ptype_name).loc (x.ptype_name).txt k in
              let k = params ctx loc x.ptype_params k in
              let k = cstrs ctx loc x.ptype_cstrs k in
              let k = kind ctx loc x.ptype_kind k in
              let k = private_ ctx loc x.ptype_private k in
              let k = manifest ctx loc x.ptype_manifest k in k))
let type_extension_attributes (T f1) (T f2) =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             let k = f1 ctx loc x.ptyext_attributes k in
             let x = { x with ptyext_attributes = [] } in
             let k = f2 ctx loc x k in k)
let type_extension ~path:(T path)  ~params:(T params)  ~constructors:(T
  constructors)  ~private_:(T private_)  =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.ptyext_attributes;
             (let k = path ctx (x.ptyext_path).loc (x.ptyext_path).txt k in
              let k = params ctx loc x.ptyext_params k in
              let k = constructors ctx loc x.ptyext_constructors k in
              let k = private_ ctx loc x.ptyext_private k in k))
let ptype_abstract =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             match x with
             | Ptype_abstract  -> (ctx.matched <- ctx.matched + 1; k)
             | _ -> fail loc "abstract")
let ptype_variant (T f0) =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             match x with
             | Ptype_variant x0 ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx loc x0 k in k))
             | _ -> fail loc "variant")
let ptype_record (T f0) =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             match x with
             | Ptype_record x0 ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx loc x0 k in k))
             | _ -> fail loc "record")
let ptype_open =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             match x with
             | Ptype_open  -> (ctx.matched <- ctx.matched + 1; k)
             | _ -> fail loc "open")
let value_binding_attributes (T f1) (T f2) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.pvb_loc in
             let k = f1 ctx loc x.pvb_attributes k in
             let x = { x with pvb_attributes = [] } in
             let k = f2 ctx loc x k in k)
let value_binding ~pat:(T pat)  ~expr:(T expr)  =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pvb_attributes;
             (let k = pat ctx loc x.pvb_pat k in
              let k = expr ctx loc x.pvb_expr k in k))
let value_description_attributes (T f1) (T f2) =
  T
    (fun ctx  ->
       fun _loc  ->
         fun x  ->
           fun k  ->
             let loc = x.pval_loc in
             let k = f1 ctx loc x.pval_attributes k in
             let x = { x with pval_attributes = [] } in
             let k = f2 ctx loc x k in k)
let value_description ~name:(T name)  ~type_:(T type_)  ~prim:(T prim)  =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             Common.assert_no_attributes x.pval_attributes;
             (let k = name ctx (x.pval_name).loc (x.pval_name).txt k in
              let k = type_ ctx loc x.pval_type k in
              let k = prim ctx loc x.pval_prim k in k))
let covariant =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             match x with
             | Asttypes.Covariant  -> (ctx.matched <- ctx.matched + 1; k)
             | _ -> fail loc "Covariant")
let contravariant =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             match x with
             | Asttypes.Contravariant  -> (ctx.matched <- ctx.matched + 1; k)
             | _ -> fail loc "Contravariant")
let invariant =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             match x with
             | Asttypes.Invariant  -> (ctx.matched <- ctx.matched + 1; k)
             | _ -> fail loc "Invariant")
let virtual_ =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             match x with
             | Asttypes.Virtual  -> (ctx.matched <- ctx.matched + 1; k)
             | _ -> fail loc "Virtual")
let concrete =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             match x with
             | Asttypes.Concrete  -> (ctx.matched <- ctx.matched + 1; k)
             | _ -> fail loc "Concrete")
let pwith_type (T f0) (T f1) =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             match x with
             | Pwith_type (x0,x1) ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx x0.loc x0.txt k in
                   let k = f1 ctx loc x1 k in k))
             | _ -> fail loc "type")
let pwith_module (T f0) (T f1) =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             match x with
             | Pwith_module (x0,x1) ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx x0.loc x0.txt k in
                   let k = f1 ctx x1.loc x1.txt k in k))
             | _ -> fail loc "module")
let pwith_typesubst (T f0) =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             match x with
             | Pwith_typesubst x0 ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx loc x0 k in k))
             | _ -> fail loc "typesubst")
let pwith_modsubst (T f0) (T f1) =
  T
    (fun ctx  ->
       fun loc  ->
         fun x  ->
           fun k  ->
             match x with
             | Pwith_modsubst (x0,x1) ->
                 (ctx.matched <- ctx.matched + 1;
                  (let k = f0 ctx x0.loc x0.txt k in
                   let k = f1 ctx x1.loc x1.txt k in k))
             | _ -> fail loc "modsubst")
