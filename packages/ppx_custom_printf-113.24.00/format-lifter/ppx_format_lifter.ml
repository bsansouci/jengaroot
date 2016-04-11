class virtual ['res] lifter =
  object (this)
    method virtual  arrow : 'a . 'a -> 'res
    method virtual  existential : 'a . 'a -> 'res
    method lift_Pervasives_format6 :
      'f0 'f1 'f2 'f3 'f4 'f5 .
        ('f0 -> 'res) ->
          ('f1 -> 'res) ->
            ('f2 -> 'res) ->
              ('f3 -> 'res) ->
                ('f4 -> 'res) ->
                  ('f5 -> 'res) ->
                    ('f0,'f1,'f2,'f3,'f4,'f5) Pervasives.format6 -> 'res=
      fun (type f0) -> fun (type f1) -> fun (type f2) -> fun (type f3) -> fun
      (type f4) -> fun (type f5) ->
      (fun f0  ->
         fun f1  ->
           fun f2  ->
             fun f3  ->
               fun f4  ->
                 fun f5  ->
                   fun x  ->
                     this#lift_CamlinternalFormatBasics_format6 f0 f1 f2 f3
                       f4 f5 x : (f0 -> 'res) ->
                                   (f1 -> 'res) ->
                                     (f2 -> 'res) ->
                                       (f3 -> 'res) ->
                                         (f4 -> 'res) ->
                                           (f5 -> 'res) ->
                                             (f0,f1,f2,f3,f4,f5)
                                               Pervasives.format6 -> 
                                               'res)
    method lift_CamlinternalFormatBasics_format6 :
      'f0 'f1 'f2 'f3 'f4 'f5 .
        ('f0 -> 'res) ->
          ('f1 -> 'res) ->
            ('f2 -> 'res) ->
              ('f3 -> 'res) ->
                ('f4 -> 'res) ->
                  ('f5 -> 'res) ->
                    ('f0,'f1,'f2,'f3,'f4,'f5)
                      CamlinternalFormatBasics.format6 -> 'res=
      fun (type f0) -> fun (type f1) -> fun (type f2) -> fun (type f3) -> fun
      (type f4) -> fun (type f5) ->
      (fun f0  ->
         fun f1  ->
           fun f2  ->
             fun f3  ->
               fun f4  ->
                 fun f5  ->
                   function
                   | CamlinternalFormatBasics.Format (x0,x1) ->
                       this#constr "CamlinternalFormatBasics.format6"
                         ("Format",
                           [this#lift_CamlinternalFormatBasics_fmt f0 f1 f2
                              f3 f4 f5 x0;
                           this#string x1]) : (f0 -> 'res) ->
                                                (f1 -> 'res) ->
                                                  (f2 -> 'res) ->
                                                    (f3 -> 'res) ->
                                                      (f4 -> 'res) ->
                                                        (f5 -> 'res) ->
                                                          (f0,f1,f2,f3,
                                                            f4,f5)
                                                            CamlinternalFormatBasics.format6
                                                            -> 'res)
    method lift_CamlinternalFormatBasics_fmt :
      'f0 'f1 'f2 'f3 'f4 'f5 .
        ('f0 -> 'res) ->
          ('f1 -> 'res) ->
            ('f2 -> 'res) ->
              ('f3 -> 'res) ->
                ('f4 -> 'res) ->
                  ('f5 -> 'res) ->
                    ('f0,'f1,'f2,'f3,'f4,'f5) CamlinternalFormatBasics.fmt ->
                      'res=
      fun (type f0) -> fun (type f1) -> fun (type f2) -> fun (type f3) -> fun
      (type f4) -> fun (type f5) ->
      (fun f0  ->
         fun f1  ->
           fun f2  ->
             fun f3  ->
               fun f4  ->
                 fun f5  ->
                   function
                   | CamlinternalFormatBasics.Char x0 ->
                       this#constr "CamlinternalFormatBasics.fmt"
                         ("Char",
                           [this#lift_CamlinternalFormatBasics_fmt
                              this#existential this#existential
                              this#existential this#existential
                              this#existential this#existential x0])
                   | CamlinternalFormatBasics.Caml_char x0 ->
                       this#constr "CamlinternalFormatBasics.fmt"
                         ("Caml_char",
                           [this#lift_CamlinternalFormatBasics_fmt
                              this#existential this#existential
                              this#existential this#existential
                              this#existential this#existential x0])
                   | CamlinternalFormatBasics.String (x0,x1) ->
                       this#constr "CamlinternalFormatBasics.fmt"
                         ("String",
                           [this#lift_CamlinternalFormatBasics_padding
                              this#existential this#arrow x0;
                           this#lift_CamlinternalFormatBasics_fmt
                             this#existential this#existential
                             this#existential this#existential
                             this#existential this#existential x1])
                   | CamlinternalFormatBasics.Caml_string (x0,x1) ->
                       this#constr "CamlinternalFormatBasics.fmt"
                         ("Caml_string",
                           [this#lift_CamlinternalFormatBasics_padding
                              this#existential this#arrow x0;
                           this#lift_CamlinternalFormatBasics_fmt
                             this#existential this#existential
                             this#existential this#existential
                             this#existential this#existential x1])
                   | CamlinternalFormatBasics.Int (x0,x1,x2,x3) ->
                       this#constr "CamlinternalFormatBasics.fmt"
                         ("Int",
                           [this#lift_CamlinternalFormatBasics_int_conv x0;
                           this#lift_CamlinternalFormatBasics_padding
                             this#existential this#existential x1;
                           this#lift_CamlinternalFormatBasics_precision
                             this#existential this#arrow x2;
                           this#lift_CamlinternalFormatBasics_fmt
                             this#existential this#existential
                             this#existential this#existential
                             this#existential this#existential x3])
                   | CamlinternalFormatBasics.Int32 (x0,x1,x2,x3) ->
                       this#constr "CamlinternalFormatBasics.fmt"
                         ("Int32",
                           [this#lift_CamlinternalFormatBasics_int_conv x0;
                           this#lift_CamlinternalFormatBasics_padding
                             this#existential this#existential x1;
                           this#lift_CamlinternalFormatBasics_precision
                             this#existential this#arrow x2;
                           this#lift_CamlinternalFormatBasics_fmt
                             this#existential this#existential
                             this#existential this#existential
                             this#existential this#existential x3])
                   | CamlinternalFormatBasics.Nativeint (x0,x1,x2,x3) ->
                       this#constr "CamlinternalFormatBasics.fmt"
                         ("Nativeint",
                           [this#lift_CamlinternalFormatBasics_int_conv x0;
                           this#lift_CamlinternalFormatBasics_padding
                             this#existential this#existential x1;
                           this#lift_CamlinternalFormatBasics_precision
                             this#existential this#arrow x2;
                           this#lift_CamlinternalFormatBasics_fmt
                             this#existential this#existential
                             this#existential this#existential
                             this#existential this#existential x3])
                   | CamlinternalFormatBasics.Int64 (x0,x1,x2,x3) ->
                       this#constr "CamlinternalFormatBasics.fmt"
                         ("Int64",
                           [this#lift_CamlinternalFormatBasics_int_conv x0;
                           this#lift_CamlinternalFormatBasics_padding
                             this#existential this#existential x1;
                           this#lift_CamlinternalFormatBasics_precision
                             this#existential this#arrow x2;
                           this#lift_CamlinternalFormatBasics_fmt
                             this#existential this#existential
                             this#existential this#existential
                             this#existential this#existential x3])
                   | CamlinternalFormatBasics.Float (x0,x1,x2,x3) ->
                       this#constr "CamlinternalFormatBasics.fmt"
                         ("Float",
                           [this#lift_CamlinternalFormatBasics_float_conv x0;
                           this#lift_CamlinternalFormatBasics_padding
                             this#existential this#existential x1;
                           this#lift_CamlinternalFormatBasics_precision
                             this#existential this#arrow x2;
                           this#lift_CamlinternalFormatBasics_fmt
                             this#existential this#existential
                             this#existential this#existential
                             this#existential this#existential x3])
                   | CamlinternalFormatBasics.Bool x0 ->
                       this#constr "CamlinternalFormatBasics.fmt"
                         ("Bool",
                           [this#lift_CamlinternalFormatBasics_fmt
                              this#existential this#existential
                              this#existential this#existential
                              this#existential this#existential x0])
                   | CamlinternalFormatBasics.Flush x0 ->
                       this#constr "CamlinternalFormatBasics.fmt"
                         ("Flush",
                           [this#lift_CamlinternalFormatBasics_fmt
                              this#existential this#existential
                              this#existential this#existential
                              this#existential this#existential x0])
                   | CamlinternalFormatBasics.String_literal (x0,x1) ->
                       this#constr "CamlinternalFormatBasics.fmt"
                         ("String_literal",
                           [this#string x0;
                           this#lift_CamlinternalFormatBasics_fmt
                             this#existential this#existential
                             this#existential this#existential
                             this#existential this#existential x1])
                   | CamlinternalFormatBasics.Char_literal (x0,x1) ->
                       this#constr "CamlinternalFormatBasics.fmt"
                         ("Char_literal",
                           [this#char x0;
                           this#lift_CamlinternalFormatBasics_fmt
                             this#existential this#existential
                             this#existential this#existential
                             this#existential this#existential x1])
                   | CamlinternalFormatBasics.Format_arg (x0,x1,x2) ->
                       this#constr "CamlinternalFormatBasics.fmt"
                         ("Format_arg",
                           [this#lift_CamlinternalFormatBasics_pad_option x0;
                           this#lift_CamlinternalFormatBasics_fmtty
                             this#existential this#existential
                             this#existential this#existential
                             this#existential this#existential x1;
                           this#lift_CamlinternalFormatBasics_fmt
                             this#existential this#existential
                             this#existential this#existential
                             this#existential this#existential x2])
                   | CamlinternalFormatBasics.Format_subst (x0,x1,x2) ->
                       this#constr "CamlinternalFormatBasics.fmt"
                         ("Format_subst",
                           [this#lift_CamlinternalFormatBasics_pad_option x0;
                           this#lift_CamlinternalFormatBasics_fmtty_rel
                             this#existential this#existential
                             this#existential this#existential
                             this#existential this#existential
                             this#existential this#existential
                             this#existential this#existential
                             this#existential this#existential x1;
                           this#lift_CamlinternalFormatBasics_fmt
                             this#existential this#existential
                             this#existential this#existential
                             this#existential this#existential x2])
                   | CamlinternalFormatBasics.Alpha x0 ->
                       this#constr "CamlinternalFormatBasics.fmt"
                         ("Alpha",
                           [this#lift_CamlinternalFormatBasics_fmt
                              this#existential this#existential
                              this#existential this#existential
                              this#existential this#existential x0])
                   | CamlinternalFormatBasics.Theta x0 ->
                       this#constr "CamlinternalFormatBasics.fmt"
                         ("Theta",
                           [this#lift_CamlinternalFormatBasics_fmt
                              this#existential this#existential
                              this#existential this#existential
                              this#existential this#existential x0])
                   | CamlinternalFormatBasics.Formatting_lit (x0,x1) ->
                       this#constr "CamlinternalFormatBasics.fmt"
                         ("Formatting_lit",
                           [this#lift_CamlinternalFormatBasics_formatting_lit
                              x0;
                           this#lift_CamlinternalFormatBasics_fmt
                             this#existential this#existential
                             this#existential this#existential
                             this#existential this#existential x1])
                   | CamlinternalFormatBasics.Formatting_gen (x0,x1) ->
                       this#constr "CamlinternalFormatBasics.fmt"
                         ("Formatting_gen",
                           [this#lift_CamlinternalFormatBasics_formatting_gen
                              this#existential this#existential
                              this#existential this#existential
                              this#existential this#existential x0;
                           this#lift_CamlinternalFormatBasics_fmt
                             this#existential this#existential
                             this#existential this#existential
                             this#existential this#existential x1])
                   | CamlinternalFormatBasics.Reader x0 ->
                       this#constr "CamlinternalFormatBasics.fmt"
                         ("Reader",
                           [this#lift_CamlinternalFormatBasics_fmt
                              this#existential this#existential
                              this#existential this#existential
                              this#existential this#existential x0])
                   | CamlinternalFormatBasics.Scan_char_set (x0,x1,x2) ->
                       this#constr "CamlinternalFormatBasics.fmt"
                         ("Scan_char_set",
                           [this#lift_CamlinternalFormatBasics_pad_option x0;
                           this#lift_CamlinternalFormatBasics_char_set x1;
                           this#lift_CamlinternalFormatBasics_fmt
                             this#existential this#existential
                             this#existential this#existential
                             this#existential this#existential x2])
                   | CamlinternalFormatBasics.Scan_get_counter (x0,x1) ->
                       this#constr "CamlinternalFormatBasics.fmt"
                         ("Scan_get_counter",
                           [this#lift_CamlinternalFormatBasics_counter x0;
                           this#lift_CamlinternalFormatBasics_fmt
                             this#existential this#existential
                             this#existential this#existential
                             this#existential this#existential x1])
                   | CamlinternalFormatBasics.Scan_next_char x0 ->
                       this#constr "CamlinternalFormatBasics.fmt"
                         ("Scan_next_char",
                           [this#lift_CamlinternalFormatBasics_fmt
                              this#existential this#existential
                              this#existential this#existential
                              this#existential this#existential x0])
                   | CamlinternalFormatBasics.Ignored_param (x0,x1) ->
                       this#constr "CamlinternalFormatBasics.fmt"
                         ("Ignored_param",
                           [this#lift_CamlinternalFormatBasics_ignored
                              this#existential this#existential
                              this#existential this#existential
                              this#existential this#existential x0;
                           this#lift_CamlinternalFormatBasics_fmt
                             this#existential this#existential
                             this#existential this#existential
                             this#existential this#existential x1])
                   | CamlinternalFormatBasics.Custom (x0,x1,x2) ->
                       this#constr "CamlinternalFormatBasics.fmt"
                         ("Custom",
                           [this#lift_CamlinternalFormatBasics_custom_arity
                              this#existential this#existential
                              this#existential x0;
                           this#arrow x1;
                           this#lift_CamlinternalFormatBasics_fmt
                             this#existential this#existential
                             this#existential this#existential
                             this#existential this#existential x2])
                   | CamlinternalFormatBasics.End_of_format  ->
                       this#constr "CamlinternalFormatBasics.fmt"
                         ("End_of_format", []) : (f0 -> 'res) ->
                                                   (f1 -> 'res) ->
                                                     (f2 -> 'res) ->
                                                       (f3 -> 'res) ->
                                                         (f4 -> 'res) ->
                                                           (f5 -> 'res) ->
                                                             (f0,f1,f2,
                                                               f3,f4,
                                                               f5)
                                                               CamlinternalFormatBasics.fmt
                                                               -> 'res)
    method lift_CamlinternalFormatBasics_custom_arity :
      'f0 'f1 'f2 .
        ('f0 -> 'res) ->
          ('f1 -> 'res) ->
            ('f2 -> 'res) ->
              ('f0,'f1,'f2) CamlinternalFormatBasics.custom_arity -> 'res=
      fun (type f0) -> fun (type f1) -> fun (type f2) ->
      (fun f0  ->
         fun f1  ->
           fun f2  ->
             function
             | CamlinternalFormatBasics.Custom_zero  ->
                 this#constr "CamlinternalFormatBasics.custom_arity"
                   ("Custom_zero", [])
             | CamlinternalFormatBasics.Custom_succ x0 ->
                 this#constr "CamlinternalFormatBasics.custom_arity"
                   ("Custom_succ",
                     [this#lift_CamlinternalFormatBasics_custom_arity
                        this#existential this#existential this#existential x0]) : 
      (f0 -> 'res) ->
        (f1 -> 'res) ->
          (f2 -> 'res) ->
            (f0,f1,f2) CamlinternalFormatBasics.custom_arity -> 'res)
    method lift_CamlinternalFormatBasics_ignored :
      'f0 'f1 'f2 'f3 'f4 'f5 .
        ('f0 -> 'res) ->
          ('f1 -> 'res) ->
            ('f2 -> 'res) ->
              ('f3 -> 'res) ->
                ('f4 -> 'res) ->
                  ('f5 -> 'res) ->
                    ('f0,'f1,'f2,'f3,'f4,'f5)
                      CamlinternalFormatBasics.ignored -> 'res=
      fun (type f0) -> fun (type f1) -> fun (type f2) -> fun (type f3) -> fun
      (type f4) -> fun (type f5) ->
      (fun f0  ->
         fun f1  ->
           fun f2  ->
             fun f3  ->
               fun f4  ->
                 fun f5  ->
                   function
                   | CamlinternalFormatBasics.Ignored_char  ->
                       this#constr "CamlinternalFormatBasics.ignored"
                         ("Ignored_char", [])
                   | CamlinternalFormatBasics.Ignored_caml_char  ->
                       this#constr "CamlinternalFormatBasics.ignored"
                         ("Ignored_caml_char", [])
                   | CamlinternalFormatBasics.Ignored_string x0 ->
                       this#constr "CamlinternalFormatBasics.ignored"
                         ("Ignored_string",
                           [this#lift_CamlinternalFormatBasics_pad_option x0])
                   | CamlinternalFormatBasics.Ignored_caml_string x0 ->
                       this#constr "CamlinternalFormatBasics.ignored"
                         ("Ignored_caml_string",
                           [this#lift_CamlinternalFormatBasics_pad_option x0])
                   | CamlinternalFormatBasics.Ignored_int (x0,x1) ->
                       this#constr "CamlinternalFormatBasics.ignored"
                         ("Ignored_int",
                           [this#lift_CamlinternalFormatBasics_int_conv x0;
                           this#lift_CamlinternalFormatBasics_pad_option x1])
                   | CamlinternalFormatBasics.Ignored_int32 (x0,x1) ->
                       this#constr "CamlinternalFormatBasics.ignored"
                         ("Ignored_int32",
                           [this#lift_CamlinternalFormatBasics_int_conv x0;
                           this#lift_CamlinternalFormatBasics_pad_option x1])
                   | CamlinternalFormatBasics.Ignored_nativeint (x0,x1) ->
                       this#constr "CamlinternalFormatBasics.ignored"
                         ("Ignored_nativeint",
                           [this#lift_CamlinternalFormatBasics_int_conv x0;
                           this#lift_CamlinternalFormatBasics_pad_option x1])
                   | CamlinternalFormatBasics.Ignored_int64 (x0,x1) ->
                       this#constr "CamlinternalFormatBasics.ignored"
                         ("Ignored_int64",
                           [this#lift_CamlinternalFormatBasics_int_conv x0;
                           this#lift_CamlinternalFormatBasics_pad_option x1])
                   | CamlinternalFormatBasics.Ignored_float (x0,x1) ->
                       this#constr "CamlinternalFormatBasics.ignored"
                         ("Ignored_float",
                           [this#lift_CamlinternalFormatBasics_pad_option x0;
                           this#lift_CamlinternalFormatBasics_prec_option x1])
                   | CamlinternalFormatBasics.Ignored_bool  ->
                       this#constr "CamlinternalFormatBasics.ignored"
                         ("Ignored_bool", [])
                   | CamlinternalFormatBasics.Ignored_format_arg (x0,x1) ->
                       this#constr "CamlinternalFormatBasics.ignored"
                         ("Ignored_format_arg",
                           [this#lift_CamlinternalFormatBasics_pad_option x0;
                           this#lift_CamlinternalFormatBasics_fmtty
                             this#existential this#existential
                             this#existential this#existential
                             this#existential this#existential x1])
                   | CamlinternalFormatBasics.Ignored_format_subst (x0,x1) ->
                       this#constr "CamlinternalFormatBasics.ignored"
                         ("Ignored_format_subst",
                           [this#lift_CamlinternalFormatBasics_pad_option x0;
                           this#lift_CamlinternalFormatBasics_fmtty
                             this#existential this#existential
                             this#existential this#existential
                             this#existential this#existential x1])
                   | CamlinternalFormatBasics.Ignored_reader  ->
                       this#constr "CamlinternalFormatBasics.ignored"
                         ("Ignored_reader", [])
                   | CamlinternalFormatBasics.Ignored_scan_char_set (x0,x1)
                       ->
                       this#constr "CamlinternalFormatBasics.ignored"
                         ("Ignored_scan_char_set",
                           [this#lift_CamlinternalFormatBasics_pad_option x0;
                           this#lift_CamlinternalFormatBasics_char_set x1])
                   | CamlinternalFormatBasics.Ignored_scan_get_counter x0 ->
                       this#constr "CamlinternalFormatBasics.ignored"
                         ("Ignored_scan_get_counter",
                           [this#lift_CamlinternalFormatBasics_counter x0])
                   | CamlinternalFormatBasics.Ignored_scan_next_char  ->
                       this#constr "CamlinternalFormatBasics.ignored"
                         ("Ignored_scan_next_char", []) : (f0 -> 'res) ->
                                                            (f1 -> 'res) ->
                                                              (f2 -> 'res) ->
                                                                (f3 -> 'res)
                                                                  ->
                                                                  (f4 -> 'res)
                                                                    ->
                                                                    (f5 ->
                                                                    'res) ->
                                                                    (f0,
                                                                    f1,
                                                                    f2,
                                                                    f3,
                                                                    f4,
                                                                    f5)
                                                                    CamlinternalFormatBasics.ignored
                                                                    -> 
                                                                    'res)
    method lift_CamlinternalFormatBasics_prec_option :
      CamlinternalFormatBasics.prec_option -> 'res=
      (fun x  -> this#lift_option this#int x : CamlinternalFormatBasics.prec_option
                                                 -> 'res)
    method lift_CamlinternalFormatBasics_counter :
      CamlinternalFormatBasics.counter -> 'res=
      (function
       | CamlinternalFormatBasics.Line_counter  ->
           this#constr "CamlinternalFormatBasics.counter"
             ("Line_counter", [])
       | CamlinternalFormatBasics.Char_counter  ->
           this#constr "CamlinternalFormatBasics.counter"
             ("Char_counter", [])
       | CamlinternalFormatBasics.Token_counter  ->
           this#constr "CamlinternalFormatBasics.counter"
             ("Token_counter", []) : CamlinternalFormatBasics.counter -> 'res)
    method lift_CamlinternalFormatBasics_char_set :
      CamlinternalFormatBasics.char_set -> 'res=
      (this#string : CamlinternalFormatBasics.char_set -> 'res)
    method lift_CamlinternalFormatBasics_formatting_gen :
      'f0 'f1 'f2 'f3 'f4 'f5 .
        ('f0 -> 'res) ->
          ('f1 -> 'res) ->
            ('f2 -> 'res) ->
              ('f3 -> 'res) ->
                ('f4 -> 'res) ->
                  ('f5 -> 'res) ->
                    ('f0,'f1,'f2,'f3,'f4,'f5)
                      CamlinternalFormatBasics.formatting_gen -> 'res=
      fun (type f0) -> fun (type f1) -> fun (type f2) -> fun (type f3) -> fun
      (type f4) -> fun (type f5) ->
      (fun f0  ->
         fun f1  ->
           fun f2  ->
             fun f3  ->
               fun f4  ->
                 fun f5  ->
                   function
                   | CamlinternalFormatBasics.Open_tag x0 ->
                       this#constr "CamlinternalFormatBasics.formatting_gen"
                         ("Open_tag",
                           [this#lift_CamlinternalFormatBasics_format6
                              this#existential this#existential
                              this#existential this#existential
                              this#existential this#existential x0])
                   | CamlinternalFormatBasics.Open_box x0 ->
                       this#constr "CamlinternalFormatBasics.formatting_gen"
                         ("Open_box",
                           [this#lift_CamlinternalFormatBasics_format6
                              this#existential this#existential
                              this#existential this#existential
                              this#existential this#existential x0]) : 
      (f0 -> 'res) ->
        (f1 -> 'res) ->
          (f2 -> 'res) ->
            (f3 -> 'res) ->
              (f4 -> 'res) ->
                (f5 -> 'res) ->
                  (f0,f1,f2,f3,f4,f5) CamlinternalFormatBasics.formatting_gen
                    -> 'res)
    method lift_CamlinternalFormatBasics_formatting_lit :
      CamlinternalFormatBasics.formatting_lit -> 'res=
      (function
       | CamlinternalFormatBasics.Close_box  ->
           this#constr "CamlinternalFormatBasics.formatting_lit"
             ("Close_box", [])
       | CamlinternalFormatBasics.Close_tag  ->
           this#constr "CamlinternalFormatBasics.formatting_lit"
             ("Close_tag", [])
       | CamlinternalFormatBasics.Break (x0,x1,x2) ->
           this#constr "CamlinternalFormatBasics.formatting_lit"
             ("Break", [this#string x0; this#int x1; this#int x2])
       | CamlinternalFormatBasics.FFlush  ->
           this#constr "CamlinternalFormatBasics.formatting_lit"
             ("FFlush", [])
       | CamlinternalFormatBasics.Force_newline  ->
           this#constr "CamlinternalFormatBasics.formatting_lit"
             ("Force_newline", [])
       | CamlinternalFormatBasics.Flush_newline  ->
           this#constr "CamlinternalFormatBasics.formatting_lit"
             ("Flush_newline", [])
       | CamlinternalFormatBasics.Magic_size (x0,x1) ->
           this#constr "CamlinternalFormatBasics.formatting_lit"
             ("Magic_size", [this#string x0; this#int x1])
       | CamlinternalFormatBasics.Escaped_at  ->
           this#constr "CamlinternalFormatBasics.formatting_lit"
             ("Escaped_at", [])
       | CamlinternalFormatBasics.Escaped_percent  ->
           this#constr "CamlinternalFormatBasics.formatting_lit"
             ("Escaped_percent", [])
       | CamlinternalFormatBasics.Scan_indic x0 ->
           this#constr "CamlinternalFormatBasics.formatting_lit"
             ("Scan_indic", [this#char x0]) : CamlinternalFormatBasics.formatting_lit
                                                -> 'res)
    method lift_CamlinternalFormatBasics_fmtty :
      'f0 'f1 'f2 'f3 'f4 'f5 .
        ('f0 -> 'res) ->
          ('f1 -> 'res) ->
            ('f2 -> 'res) ->
              ('f3 -> 'res) ->
                ('f4 -> 'res) ->
                  ('f5 -> 'res) ->
                    ('f0,'f1,'f2,'f3,'f4,'f5) CamlinternalFormatBasics.fmtty
                      -> 'res=
      fun (type f0) -> fun (type f1) -> fun (type f2) -> fun (type f3) -> fun
      (type f4) -> fun (type f5) ->
      (fun f0  ->
         fun f1  ->
           fun f2  ->
             fun f3  ->
               fun f4  ->
                 fun f5  ->
                   fun x  ->
                     this#lift_CamlinternalFormatBasics_fmtty_rel f0 f1 f2 f3
                       f4 f5 f0 f1 f2 f3 f4 f5 x : (f0 -> 'res) ->
                                                     (f1 -> 'res) ->
                                                       (f2 -> 'res) ->
                                                         (f3 -> 'res) ->
                                                           (f4 -> 'res) ->
                                                             (f5 -> 'res) ->
                                                               (f0,f1,
                                                                 f2,f3,
                                                                 f4,f5)
                                                                 CamlinternalFormatBasics.fmtty
                                                                 -> 'res)
    method lift_CamlinternalFormatBasics_fmtty_rel :
      'f0 'f1 'f2 'f3 'f4 'f5 'f6 'f7 'f8 'f9 'f10 'f11 .
        ('f0 -> 'res) ->
          ('f1 -> 'res) ->
            ('f2 -> 'res) ->
              ('f3 -> 'res) ->
                ('f4 -> 'res) ->
                  ('f5 -> 'res) ->
                    ('f6 -> 'res) ->
                      ('f7 -> 'res) ->
                        ('f8 -> 'res) ->
                          ('f9 -> 'res) ->
                            ('f10 -> 'res) ->
                              ('f11 -> 'res) ->
                                ('f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,
                                  'f9,'f10,'f11)
                                  CamlinternalFormatBasics.fmtty_rel -> 
                                  'res=
      fun (type f0) -> fun (type f1) -> fun (type f2) -> fun (type f3) -> fun
      (type f4) -> fun (type f5) -> fun (type f6) -> fun (type f7) -> fun
      (type f8) -> fun (type f9) -> fun (type f10) -> fun (type f11) ->
      (fun f0  ->
         fun f1  ->
           fun f2  ->
             fun f3  ->
               fun f4  ->
                 fun f5  ->
                   fun f6  ->
                     fun f7  ->
                       fun f8  ->
                         fun f9  ->
                           fun f10  ->
                             fun f11  ->
                               function
                               | CamlinternalFormatBasics.Char_ty x0 ->
                                   this#constr
                                     "CamlinternalFormatBasics.fmtty_rel"
                                     ("Char_ty",
                                       [this#lift_CamlinternalFormatBasics_fmtty_rel
                                          this#existential this#existential
                                          this#existential this#existential
                                          this#existential this#existential
                                          this#existential this#existential
                                          this#existential this#existential
                                          this#existential this#existential
                                          x0])
                               | CamlinternalFormatBasics.String_ty x0 ->
                                   this#constr
                                     "CamlinternalFormatBasics.fmtty_rel"
                                     ("String_ty",
                                       [this#lift_CamlinternalFormatBasics_fmtty_rel
                                          this#existential this#existential
                                          this#existential this#existential
                                          this#existential this#existential
                                          this#existential this#existential
                                          this#existential this#existential
                                          this#existential this#existential
                                          x0])
                               | CamlinternalFormatBasics.Int_ty x0 ->
                                   this#constr
                                     "CamlinternalFormatBasics.fmtty_rel"
                                     ("Int_ty",
                                       [this#lift_CamlinternalFormatBasics_fmtty_rel
                                          this#existential this#existential
                                          this#existential this#existential
                                          this#existential this#existential
                                          this#existential this#existential
                                          this#existential this#existential
                                          this#existential this#existential
                                          x0])
                               | CamlinternalFormatBasics.Int32_ty x0 ->
                                   this#constr
                                     "CamlinternalFormatBasics.fmtty_rel"
                                     ("Int32_ty",
                                       [this#lift_CamlinternalFormatBasics_fmtty_rel
                                          this#existential this#existential
                                          this#existential this#existential
                                          this#existential this#existential
                                          this#existential this#existential
                                          this#existential this#existential
                                          this#existential this#existential
                                          x0])
                               | CamlinternalFormatBasics.Nativeint_ty x0 ->
                                   this#constr
                                     "CamlinternalFormatBasics.fmtty_rel"
                                     ("Nativeint_ty",
                                       [this#lift_CamlinternalFormatBasics_fmtty_rel
                                          this#existential this#existential
                                          this#existential this#existential
                                          this#existential this#existential
                                          this#existential this#existential
                                          this#existential this#existential
                                          this#existential this#existential
                                          x0])
                               | CamlinternalFormatBasics.Int64_ty x0 ->
                                   this#constr
                                     "CamlinternalFormatBasics.fmtty_rel"
                                     ("Int64_ty",
                                       [this#lift_CamlinternalFormatBasics_fmtty_rel
                                          this#existential this#existential
                                          this#existential this#existential
                                          this#existential this#existential
                                          this#existential this#existential
                                          this#existential this#existential
                                          this#existential this#existential
                                          x0])
                               | CamlinternalFormatBasics.Float_ty x0 ->
                                   this#constr
                                     "CamlinternalFormatBasics.fmtty_rel"
                                     ("Float_ty",
                                       [this#lift_CamlinternalFormatBasics_fmtty_rel
                                          this#existential this#existential
                                          this#existential this#existential
                                          this#existential this#existential
                                          this#existential this#existential
                                          this#existential this#existential
                                          this#existential this#existential
                                          x0])
                               | CamlinternalFormatBasics.Bool_ty x0 ->
                                   this#constr
                                     "CamlinternalFormatBasics.fmtty_rel"
                                     ("Bool_ty",
                                       [this#lift_CamlinternalFormatBasics_fmtty_rel
                                          this#existential this#existential
                                          this#existential this#existential
                                          this#existential this#existential
                                          this#existential this#existential
                                          this#existential this#existential
                                          this#existential this#existential
                                          x0])
                               | CamlinternalFormatBasics.Format_arg_ty
                                   (x0,x1) ->
                                   this#constr
                                     "CamlinternalFormatBasics.fmtty_rel"
                                     ("Format_arg_ty",
                                       [this#lift_CamlinternalFormatBasics_fmtty
                                          this#existential this#existential
                                          this#existential this#existential
                                          this#existential this#existential
                                          x0;
                                       this#lift_CamlinternalFormatBasics_fmtty_rel
                                         this#existential this#existential
                                         this#existential this#existential
                                         this#existential this#existential
                                         this#existential this#existential
                                         this#existential this#existential
                                         this#existential this#existential x1])
                               | CamlinternalFormatBasics.Format_subst_ty
                                   (x0,x1,x2) ->
                                   this#constr
                                     "CamlinternalFormatBasics.fmtty_rel"
                                     ("Format_subst_ty",
                                       [this#lift_CamlinternalFormatBasics_fmtty_rel
                                          this#existential this#existential
                                          this#existential this#existential
                                          this#existential this#existential
                                          this#existential this#existential
                                          this#existential this#existential
                                          this#existential this#existential
                                          x0;
                                       this#lift_CamlinternalFormatBasics_fmtty_rel
                                         this#existential this#existential
                                         this#existential this#existential
                                         this#existential this#existential
                                         this#existential this#existential
                                         this#existential this#existential
                                         this#existential this#existential x1;
                                       this#lift_CamlinternalFormatBasics_fmtty_rel
                                         this#existential this#existential
                                         this#existential this#existential
                                         this#existential this#existential
                                         this#existential this#existential
                                         this#existential this#existential
                                         this#existential this#existential x2])
                               | CamlinternalFormatBasics.Alpha_ty x0 ->
                                   this#constr
                                     "CamlinternalFormatBasics.fmtty_rel"
                                     ("Alpha_ty",
                                       [this#lift_CamlinternalFormatBasics_fmtty_rel
                                          this#existential this#existential
                                          this#existential this#existential
                                          this#existential this#existential
                                          this#existential this#existential
                                          this#existential this#existential
                                          this#existential this#existential
                                          x0])
                               | CamlinternalFormatBasics.Theta_ty x0 ->
                                   this#constr
                                     "CamlinternalFormatBasics.fmtty_rel"
                                     ("Theta_ty",
                                       [this#lift_CamlinternalFormatBasics_fmtty_rel
                                          this#existential this#existential
                                          this#existential this#existential
                                          this#existential this#existential
                                          this#existential this#existential
                                          this#existential this#existential
                                          this#existential this#existential
                                          x0])
                               | CamlinternalFormatBasics.Any_ty x0 ->
                                   this#constr
                                     "CamlinternalFormatBasics.fmtty_rel"
                                     ("Any_ty",
                                       [this#lift_CamlinternalFormatBasics_fmtty_rel
                                          this#existential this#existential
                                          this#existential this#existential
                                          this#existential this#existential
                                          this#existential this#existential
                                          this#existential this#existential
                                          this#existential this#existential
                                          x0])
                               | CamlinternalFormatBasics.Reader_ty x0 ->
                                   this#constr
                                     "CamlinternalFormatBasics.fmtty_rel"
                                     ("Reader_ty",
                                       [this#lift_CamlinternalFormatBasics_fmtty_rel
                                          this#existential this#existential
                                          this#existential this#existential
                                          this#existential this#existential
                                          this#existential this#existential
                                          this#existential this#existential
                                          this#existential this#existential
                                          x0])
                               | CamlinternalFormatBasics.Ignored_reader_ty
                                   x0 ->
                                   this#constr
                                     "CamlinternalFormatBasics.fmtty_rel"
                                     ("Ignored_reader_ty",
                                       [this#lift_CamlinternalFormatBasics_fmtty_rel
                                          this#existential this#existential
                                          this#existential this#existential
                                          this#existential this#existential
                                          this#existential this#existential
                                          this#existential this#existential
                                          this#existential this#existential
                                          x0])
                               | CamlinternalFormatBasics.End_of_fmtty  ->
                                   this#constr
                                     "CamlinternalFormatBasics.fmtty_rel"
                                     ("End_of_fmtty", []) : (f0 -> 'res) ->
                                                              (f1 -> 'res) ->
                                                                (f2 -> 'res)
                                                                  ->
                                                                  (f3 -> 'res)
                                                                    ->
                                                                    (f4 ->
                                                                    'res) ->
                                                                    (f5 ->
                                                                    'res) ->
                                                                    (f6 ->
                                                                    'res) ->
                                                                    (f7 ->
                                                                    'res) ->
                                                                    (f8 ->
                                                                    'res) ->
                                                                    (f9 ->
                                                                    'res) ->
                                                                    (f10 ->
                                                                    'res) ->
                                                                    (f11 ->
                                                                    'res) ->
                                                                    (f0,
                                                                    f1,
                                                                    f2,
                                                                    f3,
                                                                    f4,
                                                                    f5,
                                                                    f6,
                                                                    f7,
                                                                    f8,
                                                                    f9,
                                                                    f10,
                                                                    f11)
                                                                    CamlinternalFormatBasics.fmtty_rel
                                                                    -> 
                                                                    'res)
    method lift_CamlinternalFormatBasics_pad_option :
      CamlinternalFormatBasics.pad_option -> 'res=
      (fun x  -> this#lift_option this#int x : CamlinternalFormatBasics.pad_option
                                                 -> 'res)
    method lift_option : 'f0 . ('f0 -> 'res) -> 'f0 option -> 'res= fun (type
      f0) ->
      (fun f0  ->
         function
         | None  -> this#constr "option" ("None", [])
         | Some x0 -> this#constr "option" ("Some", [f0 x0]) : (f0 -> 'res)
                                                                 ->
                                                                 f0 option ->
                                                                   'res)
    method lift_CamlinternalFormatBasics_float_conv :
      CamlinternalFormatBasics.float_conv -> 'res=
      (function
       | CamlinternalFormatBasics.Float_f  ->
           this#constr "CamlinternalFormatBasics.float_conv" ("Float_f", [])
       | CamlinternalFormatBasics.Float_pf  ->
           this#constr "CamlinternalFormatBasics.float_conv" ("Float_pf", [])
       | CamlinternalFormatBasics.Float_sf  ->
           this#constr "CamlinternalFormatBasics.float_conv" ("Float_sf", [])
       | CamlinternalFormatBasics.Float_e  ->
           this#constr "CamlinternalFormatBasics.float_conv" ("Float_e", [])
       | CamlinternalFormatBasics.Float_pe  ->
           this#constr "CamlinternalFormatBasics.float_conv" ("Float_pe", [])
       | CamlinternalFormatBasics.Float_se  ->
           this#constr "CamlinternalFormatBasics.float_conv" ("Float_se", [])
       | CamlinternalFormatBasics.Float_E  ->
           this#constr "CamlinternalFormatBasics.float_conv" ("Float_E", [])
       | CamlinternalFormatBasics.Float_pE  ->
           this#constr "CamlinternalFormatBasics.float_conv" ("Float_pE", [])
       | CamlinternalFormatBasics.Float_sE  ->
           this#constr "CamlinternalFormatBasics.float_conv" ("Float_sE", [])
       | CamlinternalFormatBasics.Float_g  ->
           this#constr "CamlinternalFormatBasics.float_conv" ("Float_g", [])
       | CamlinternalFormatBasics.Float_pg  ->
           this#constr "CamlinternalFormatBasics.float_conv" ("Float_pg", [])
       | CamlinternalFormatBasics.Float_sg  ->
           this#constr "CamlinternalFormatBasics.float_conv" ("Float_sg", [])
       | CamlinternalFormatBasics.Float_G  ->
           this#constr "CamlinternalFormatBasics.float_conv" ("Float_G", [])
       | CamlinternalFormatBasics.Float_pG  ->
           this#constr "CamlinternalFormatBasics.float_conv" ("Float_pG", [])
       | CamlinternalFormatBasics.Float_sG  ->
           this#constr "CamlinternalFormatBasics.float_conv" ("Float_sG", [])
       | CamlinternalFormatBasics.Float_F  ->
           this#constr "CamlinternalFormatBasics.float_conv" ("Float_F", []) : 
      CamlinternalFormatBasics.float_conv -> 'res)
    method lift_CamlinternalFormatBasics_precision :
      'f0 'f1 .
        ('f0 -> 'res) ->
          ('f1 -> 'res) ->
            ('f0,'f1) CamlinternalFormatBasics.precision -> 'res=
      fun (type f0) -> fun (type f1) ->
      (fun f0  ->
         fun f1  ->
           function
           | CamlinternalFormatBasics.No_precision  ->
               this#constr "CamlinternalFormatBasics.precision"
                 ("No_precision", [])
           | CamlinternalFormatBasics.Lit_precision x0 ->
               this#constr "CamlinternalFormatBasics.precision"
                 ("Lit_precision", [this#int x0])
           | CamlinternalFormatBasics.Arg_precision  ->
               this#constr "CamlinternalFormatBasics.precision"
                 ("Arg_precision", []) : (f0 -> 'res) ->
                                           (f1 -> 'res) ->
                                             (f0,f1)
                                               CamlinternalFormatBasics.precision
                                               -> 'res)
    method lift_CamlinternalFormatBasics_int_conv :
      CamlinternalFormatBasics.int_conv -> 'res=
      (function
       | CamlinternalFormatBasics.Int_d  ->
           this#constr "CamlinternalFormatBasics.int_conv" ("Int_d", [])
       | CamlinternalFormatBasics.Int_pd  ->
           this#constr "CamlinternalFormatBasics.int_conv" ("Int_pd", [])
       | CamlinternalFormatBasics.Int_sd  ->
           this#constr "CamlinternalFormatBasics.int_conv" ("Int_sd", [])
       | CamlinternalFormatBasics.Int_i  ->
           this#constr "CamlinternalFormatBasics.int_conv" ("Int_i", [])
       | CamlinternalFormatBasics.Int_pi  ->
           this#constr "CamlinternalFormatBasics.int_conv" ("Int_pi", [])
       | CamlinternalFormatBasics.Int_si  ->
           this#constr "CamlinternalFormatBasics.int_conv" ("Int_si", [])
       | CamlinternalFormatBasics.Int_x  ->
           this#constr "CamlinternalFormatBasics.int_conv" ("Int_x", [])
       | CamlinternalFormatBasics.Int_Cx  ->
           this#constr "CamlinternalFormatBasics.int_conv" ("Int_Cx", [])
       | CamlinternalFormatBasics.Int_X  ->
           this#constr "CamlinternalFormatBasics.int_conv" ("Int_X", [])
       | CamlinternalFormatBasics.Int_CX  ->
           this#constr "CamlinternalFormatBasics.int_conv" ("Int_CX", [])
       | CamlinternalFormatBasics.Int_o  ->
           this#constr "CamlinternalFormatBasics.int_conv" ("Int_o", [])
       | CamlinternalFormatBasics.Int_Co  ->
           this#constr "CamlinternalFormatBasics.int_conv" ("Int_Co", [])
       | CamlinternalFormatBasics.Int_u  ->
           this#constr "CamlinternalFormatBasics.int_conv" ("Int_u", []) : 
      CamlinternalFormatBasics.int_conv -> 'res)
    method lift_CamlinternalFormatBasics_padding :
      'f0 'f1 .
        ('f0 -> 'res) ->
          ('f1 -> 'res) -> ('f0,'f1) CamlinternalFormatBasics.padding -> 'res=
      fun (type f0) -> fun (type f1) ->
      (fun f0  ->
         fun f1  ->
           function
           | CamlinternalFormatBasics.No_padding  ->
               this#constr "CamlinternalFormatBasics.padding"
                 ("No_padding", [])
           | CamlinternalFormatBasics.Lit_padding (x0,x1) ->
               this#constr "CamlinternalFormatBasics.padding"
                 ("Lit_padding",
                   [this#lift_CamlinternalFormatBasics_padty x0; this#int x1])
           | CamlinternalFormatBasics.Arg_padding x0 ->
               this#constr "CamlinternalFormatBasics.padding"
                 ("Arg_padding",
                   [this#lift_CamlinternalFormatBasics_padty x0]) : (f0 ->
                                                                    'res) ->
                                                                    (f1 ->
                                                                    'res) ->
                                                                    (f0,
                                                                    f1)
                                                                    CamlinternalFormatBasics.padding
                                                                    -> 
                                                                    'res)
    method lift_CamlinternalFormatBasics_padty :
      CamlinternalFormatBasics.padty -> 'res=
      (function
       | CamlinternalFormatBasics.Left  ->
           this#constr "CamlinternalFormatBasics.padty" ("Left", [])
       | CamlinternalFormatBasics.Right  ->
           this#constr "CamlinternalFormatBasics.padty" ("Right", [])
       | CamlinternalFormatBasics.Zeros  ->
           this#constr "CamlinternalFormatBasics.padty" ("Zeros", []) : 
      CamlinternalFormatBasics.padty -> 'res)
  end
