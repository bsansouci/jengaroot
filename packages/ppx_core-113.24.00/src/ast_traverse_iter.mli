open Parsetree
class t :
  object
    method  attribute : attribute -> unit
    method  attributes : attributes -> unit
    method  bool : bool -> unit
    method  case : case -> unit
    method  char : char -> unit
    method  class_declaration : class_declaration -> unit
    method  class_description : class_description -> unit
    method  class_expr : class_expr -> unit
    method  class_expr_desc : class_expr_desc -> unit
    method  class_field : class_field -> unit
    method  class_field_desc : class_field_desc -> unit
    method  class_field_kind : class_field_kind -> unit
    method  class_infos : 'a . ('a -> unit) -> 'a class_infos -> unit
    method  class_signature : class_signature -> unit
    method  class_structure : class_structure -> unit
    method  class_type : class_type -> unit
    method  class_type_declaration : class_type_declaration -> unit
    method  class_type_desc : class_type_desc -> unit
    method  class_type_field : class_type_field -> unit
    method  class_type_field_desc : class_type_field_desc -> unit
    method  closed_flag : Asttypes.closed_flag -> unit
    method  constant : Asttypes.constant -> unit
    method  constructor_declaration : constructor_declaration -> unit
    method  core_type : core_type -> unit
    method  core_type_desc : core_type_desc -> unit
    method  direction_flag : Asttypes.direction_flag -> unit
    method  directive_argument : directive_argument -> unit
    method  expression : expression -> unit
    method  expression_desc : expression_desc -> unit
    method  extension : extension -> unit
    method  extension_constructor : extension_constructor -> unit
    method  extension_constructor_kind : extension_constructor_kind -> unit
    method  include_declaration : include_declaration -> unit
    method  include_description : include_description -> unit
    method  include_infos : 'a . ('a -> unit) -> 'a include_infos -> unit
    method  int : int -> unit
    method  int32 : int32 -> unit
    method  int64 : int64 -> unit
    method  label : Asttypes.label -> unit
    method  label_declaration : label_declaration -> unit
    method  lexing_position : Lexing.position -> unit
    method  list : 'a . ('a -> unit) -> 'a list -> unit
    method  loc : 'a . ('a -> unit) -> 'a Asttypes.loc -> unit
    method  location : Location.t -> unit
    method  longident : Longident.t -> unit
    method  module_binding : module_binding -> unit
    method  module_declaration : module_declaration -> unit
    method  module_expr : module_expr -> unit
    method  module_expr_desc : module_expr_desc -> unit
    method  module_type : module_type -> unit
    method  module_type_declaration : module_type_declaration -> unit
    method  module_type_desc : module_type_desc -> unit
    method  mutable_flag : Asttypes.mutable_flag -> unit
    method  nativeint : nativeint -> unit
    method  open_description : open_description -> unit
    method  option : 'a . ('a -> unit) -> 'a option -> unit
    method  override_flag : Asttypes.override_flag -> unit
    method  package_type : package_type -> unit
    method  pattern : pattern -> unit
    method  pattern_desc : pattern_desc -> unit
    method  payload : payload -> unit
    method  private_flag : Asttypes.private_flag -> unit
    method  rec_flag : Asttypes.rec_flag -> unit
    method  row_field : row_field -> unit
    method  signature : signature -> unit
    method  signature_item : signature_item -> unit
    method  signature_item_desc : signature_item_desc -> unit
    method  string : string -> unit
    method  structure : structure -> unit
    method  structure_item : structure_item -> unit
    method  structure_item_desc : structure_item_desc -> unit
    method  toplevel_phrase : toplevel_phrase -> unit
    method  type_declaration : type_declaration -> unit
    method  type_extension : type_extension -> unit
    method  type_kind : type_kind -> unit
    method  value_binding : value_binding -> unit
    method  value_description : value_description -> unit
    method  variance : Asttypes.variance -> unit
    method  virtual_flag : Asttypes.virtual_flag -> unit
    method  with_constraint : with_constraint -> unit
  end
