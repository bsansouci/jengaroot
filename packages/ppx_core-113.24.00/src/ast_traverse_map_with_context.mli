open Parsetree
class ['ctx] t :
  object
    method  attribute : 'ctx -> attribute -> attribute
    method  attributes : 'ctx -> attributes -> attributes
    method  bool : 'ctx -> bool -> bool
    method  case : 'ctx -> case -> case
    method  char : 'ctx -> char -> char
    method  class_declaration :
      'ctx -> class_declaration -> class_declaration
    method  class_description :
      'ctx -> class_description -> class_description
    method  class_expr : 'ctx -> class_expr -> class_expr
    method  class_expr_desc : 'ctx -> class_expr_desc -> class_expr_desc
    method  class_field : 'ctx -> class_field -> class_field
    method  class_field_desc : 'ctx -> class_field_desc -> class_field_desc
    method  class_field_kind : 'ctx -> class_field_kind -> class_field_kind
    method  class_infos :
      'a . ('ctx -> 'a -> 'a) -> 'ctx -> 'a class_infos -> 'a class_infos
    method  class_signature : 'ctx -> class_signature -> class_signature
    method  class_structure : 'ctx -> class_structure -> class_structure
    method  class_type : 'ctx -> class_type -> class_type
    method  class_type_declaration :
      'ctx -> class_type_declaration -> class_type_declaration
    method  class_type_desc : 'ctx -> class_type_desc -> class_type_desc
    method  class_type_field : 'ctx -> class_type_field -> class_type_field
    method  class_type_field_desc :
      'ctx -> class_type_field_desc -> class_type_field_desc
    method  closed_flag :
      'ctx -> Asttypes.closed_flag -> Asttypes.closed_flag
    method  constant : 'ctx -> Asttypes.constant -> Asttypes.constant
    method  constructor_declaration :
      'ctx -> constructor_declaration -> constructor_declaration
    method  core_type : 'ctx -> core_type -> core_type
    method  core_type_desc : 'ctx -> core_type_desc -> core_type_desc
    method  direction_flag :
      'ctx -> Asttypes.direction_flag -> Asttypes.direction_flag
    method  directive_argument :
      'ctx -> directive_argument -> directive_argument
    method  expression : 'ctx -> expression -> expression
    method  expression_desc : 'ctx -> expression_desc -> expression_desc
    method  extension : 'ctx -> extension -> extension
    method  extension_constructor :
      'ctx -> extension_constructor -> extension_constructor
    method  extension_constructor_kind :
      'ctx -> extension_constructor_kind -> extension_constructor_kind
    method  include_declaration :
      'ctx -> include_declaration -> include_declaration
    method  include_description :
      'ctx -> include_description -> include_description
    method  include_infos :
      'a . ('ctx -> 'a -> 'a) -> 'ctx -> 'a include_infos -> 'a include_infos
    method  int : 'ctx -> int -> int
    method  int32 : 'ctx -> int32 -> int32
    method  int64 : 'ctx -> int64 -> int64
    method  label : 'ctx -> Asttypes.label -> Asttypes.label
    method  label_declaration :
      'ctx -> label_declaration -> label_declaration
    method  lexing_position : 'ctx -> Lexing.position -> Lexing.position
    method  list : 'a . ('ctx -> 'a -> 'a) -> 'ctx -> 'a list -> 'a list
    method  loc :
      'a . ('ctx -> 'a -> 'a) -> 'ctx -> 'a Asttypes.loc -> 'a Asttypes.loc
    method  location : 'ctx -> Location.t -> Location.t
    method  longident : 'ctx -> Longident.t -> Longident.t
    method  module_binding : 'ctx -> module_binding -> module_binding
    method  module_declaration :
      'ctx -> module_declaration -> module_declaration
    method  module_expr : 'ctx -> module_expr -> module_expr
    method  module_expr_desc : 'ctx -> module_expr_desc -> module_expr_desc
    method  module_type : 'ctx -> module_type -> module_type
    method  module_type_declaration :
      'ctx -> module_type_declaration -> module_type_declaration
    method  module_type_desc : 'ctx -> module_type_desc -> module_type_desc
    method  mutable_flag :
      'ctx -> Asttypes.mutable_flag -> Asttypes.mutable_flag
    method  nativeint : 'ctx -> nativeint -> nativeint
    method  open_description : 'ctx -> open_description -> open_description
    method  option :
      'a . ('ctx -> 'a -> 'a) -> 'ctx -> 'a option -> 'a option
    method  override_flag :
      'ctx -> Asttypes.override_flag -> Asttypes.override_flag
    method  package_type : 'ctx -> package_type -> package_type
    method  pattern : 'ctx -> pattern -> pattern
    method  pattern_desc : 'ctx -> pattern_desc -> pattern_desc
    method  payload : 'ctx -> payload -> payload
    method  private_flag :
      'ctx -> Asttypes.private_flag -> Asttypes.private_flag
    method  rec_flag : 'ctx -> Asttypes.rec_flag -> Asttypes.rec_flag
    method  row_field : 'ctx -> row_field -> row_field
    method  signature : 'ctx -> signature -> signature
    method  signature_item : 'ctx -> signature_item -> signature_item
    method  signature_item_desc :
      'ctx -> signature_item_desc -> signature_item_desc
    method  string : 'ctx -> string -> string
    method  structure : 'ctx -> structure -> structure
    method  structure_item : 'ctx -> structure_item -> structure_item
    method  structure_item_desc :
      'ctx -> structure_item_desc -> structure_item_desc
    method  toplevel_phrase : 'ctx -> toplevel_phrase -> toplevel_phrase
    method  type_declaration : 'ctx -> type_declaration -> type_declaration
    method  type_extension : 'ctx -> type_extension -> type_extension
    method  type_kind : 'ctx -> type_kind -> type_kind
    method  value_binding : 'ctx -> value_binding -> value_binding
    method  value_description :
      'ctx -> value_description -> value_description
    method  variance : 'ctx -> Asttypes.variance -> Asttypes.variance
    method  virtual_flag :
      'ctx -> Asttypes.virtual_flag -> Asttypes.virtual_flag
    method  with_constraint : 'ctx -> with_constraint -> with_constraint
  end
