open Parsetree
class ['acc] t :
  object
    method  attribute : attribute -> 'acc -> 'acc
    method  attributes : attributes -> 'acc -> 'acc
    method  bool : bool -> 'acc -> 'acc
    method  case : case -> 'acc -> 'acc
    method  char : char -> 'acc -> 'acc
    method  class_declaration : class_declaration -> 'acc -> 'acc
    method  class_description : class_description -> 'acc -> 'acc
    method  class_expr : class_expr -> 'acc -> 'acc
    method  class_expr_desc : class_expr_desc -> 'acc -> 'acc
    method  class_field : class_field -> 'acc -> 'acc
    method  class_field_desc : class_field_desc -> 'acc -> 'acc
    method  class_field_kind : class_field_kind -> 'acc -> 'acc
    method  class_infos :
      'a . ('a -> 'acc -> 'acc) -> 'a class_infos -> 'acc -> 'acc
    method  class_signature : class_signature -> 'acc -> 'acc
    method  class_structure : class_structure -> 'acc -> 'acc
    method  class_type : class_type -> 'acc -> 'acc
    method  class_type_declaration : class_type_declaration -> 'acc -> 'acc
    method  class_type_desc : class_type_desc -> 'acc -> 'acc
    method  class_type_field : class_type_field -> 'acc -> 'acc
    method  class_type_field_desc : class_type_field_desc -> 'acc -> 'acc
    method  closed_flag : Asttypes.closed_flag -> 'acc -> 'acc
    method  constant : Asttypes.constant -> 'acc -> 'acc
    method  constructor_declaration : constructor_declaration -> 'acc -> 'acc
    method  core_type : core_type -> 'acc -> 'acc
    method  core_type_desc : core_type_desc -> 'acc -> 'acc
    method  direction_flag : Asttypes.direction_flag -> 'acc -> 'acc
    method  directive_argument : directive_argument -> 'acc -> 'acc
    method  expression : expression -> 'acc -> 'acc
    method  expression_desc : expression_desc -> 'acc -> 'acc
    method  extension : extension -> 'acc -> 'acc
    method  extension_constructor : extension_constructor -> 'acc -> 'acc
    method  extension_constructor_kind :
      extension_constructor_kind -> 'acc -> 'acc
    method  include_declaration : include_declaration -> 'acc -> 'acc
    method  include_description : include_description -> 'acc -> 'acc
    method  include_infos :
      'a . ('a -> 'acc -> 'acc) -> 'a include_infos -> 'acc -> 'acc
    method  int : int -> 'acc -> 'acc
    method  int32 : int32 -> 'acc -> 'acc
    method  int64 : int64 -> 'acc -> 'acc
    method  label : Asttypes.label -> 'acc -> 'acc
    method  label_declaration : label_declaration -> 'acc -> 'acc
    method  lexing_position : Lexing.position -> 'acc -> 'acc
    method  list : 'a . ('a -> 'acc -> 'acc) -> 'a list -> 'acc -> 'acc
    method  loc :
      'a . ('a -> 'acc -> 'acc) -> 'a Asttypes.loc -> 'acc -> 'acc
    method  location : Location.t -> 'acc -> 'acc
    method  longident : Longident.t -> 'acc -> 'acc
    method  module_binding : module_binding -> 'acc -> 'acc
    method  module_declaration : module_declaration -> 'acc -> 'acc
    method  module_expr : module_expr -> 'acc -> 'acc
    method  module_expr_desc : module_expr_desc -> 'acc -> 'acc
    method  module_type : module_type -> 'acc -> 'acc
    method  module_type_declaration : module_type_declaration -> 'acc -> 'acc
    method  module_type_desc : module_type_desc -> 'acc -> 'acc
    method  mutable_flag : Asttypes.mutable_flag -> 'acc -> 'acc
    method  nativeint : nativeint -> 'acc -> 'acc
    method  open_description : open_description -> 'acc -> 'acc
    method  option : 'a . ('a -> 'acc -> 'acc) -> 'a option -> 'acc -> 'acc
    method  override_flag : Asttypes.override_flag -> 'acc -> 'acc
    method  package_type : package_type -> 'acc -> 'acc
    method  pattern : pattern -> 'acc -> 'acc
    method  pattern_desc : pattern_desc -> 'acc -> 'acc
    method  payload : payload -> 'acc -> 'acc
    method  private_flag : Asttypes.private_flag -> 'acc -> 'acc
    method  rec_flag : Asttypes.rec_flag -> 'acc -> 'acc
    method  row_field : row_field -> 'acc -> 'acc
    method  signature : signature -> 'acc -> 'acc
    method  signature_item : signature_item -> 'acc -> 'acc
    method  signature_item_desc : signature_item_desc -> 'acc -> 'acc
    method  string : string -> 'acc -> 'acc
    method  structure : structure -> 'acc -> 'acc
    method  structure_item : structure_item -> 'acc -> 'acc
    method  structure_item_desc : structure_item_desc -> 'acc -> 'acc
    method  toplevel_phrase : toplevel_phrase -> 'acc -> 'acc
    method  type_declaration : type_declaration -> 'acc -> 'acc
    method  type_extension : type_extension -> 'acc -> 'acc
    method  type_kind : type_kind -> 'acc -> 'acc
    method  value_binding : value_binding -> 'acc -> 'acc
    method  value_description : value_description -> 'acc -> 'acc
    method  variance : Asttypes.variance -> 'acc -> 'acc
    method  virtual_flag : Asttypes.virtual_flag -> 'acc -> 'acc
    method  with_constraint : with_constraint -> 'acc -> 'acc
  end
