open Parsetree
class t :
  object
    method  attribute : attribute -> attribute
    method  attributes : attributes -> attributes
    method  bool : bool -> bool
    method  case : case -> case
    method  char : char -> char
    method  class_declaration : class_declaration -> class_declaration
    method  class_description : class_description -> class_description
    method  class_expr : class_expr -> class_expr
    method  class_expr_desc : class_expr_desc -> class_expr_desc
    method  class_field : class_field -> class_field
    method  class_field_desc : class_field_desc -> class_field_desc
    method  class_field_kind : class_field_kind -> class_field_kind
    method  class_infos : 'a . ('a -> 'a) -> 'a class_infos -> 'a class_infos
    method  class_signature : class_signature -> class_signature
    method  class_structure : class_structure -> class_structure
    method  class_type : class_type -> class_type
    method  class_type_declaration :
      class_type_declaration -> class_type_declaration
    method  class_type_desc : class_type_desc -> class_type_desc
    method  class_type_field : class_type_field -> class_type_field
    method  class_type_field_desc :
      class_type_field_desc -> class_type_field_desc
    method  closed_flag : Asttypes.closed_flag -> Asttypes.closed_flag
    method  constant : Asttypes.constant -> Asttypes.constant
    method  constructor_declaration :
      constructor_declaration -> constructor_declaration
    method  core_type : core_type -> core_type
    method  core_type_desc : core_type_desc -> core_type_desc
    method  direction_flag :
      Asttypes.direction_flag -> Asttypes.direction_flag
    method  directive_argument : directive_argument -> directive_argument
    method  expression : expression -> expression
    method  expression_desc : expression_desc -> expression_desc
    method  extension : extension -> extension
    method  extension_constructor :
      extension_constructor -> extension_constructor
    method  extension_constructor_kind :
      extension_constructor_kind -> extension_constructor_kind
    method  include_declaration : include_declaration -> include_declaration
    method  include_description : include_description -> include_description
    method  include_infos :
      'a . ('a -> 'a) -> 'a include_infos -> 'a include_infos
    method  int : int -> int
    method  int32 : int32 -> int32
    method  int64 : int64 -> int64
    method  label : Asttypes.label -> Asttypes.label
    method  label_declaration : label_declaration -> label_declaration
    method  lexing_position : Lexing.position -> Lexing.position
    method  list : 'a . ('a -> 'a) -> 'a list -> 'a list
    method  loc : 'a . ('a -> 'a) -> 'a Asttypes.loc -> 'a Asttypes.loc
    method  location : Location.t -> Location.t
    method  longident : Longident.t -> Longident.t
    method  module_binding : module_binding -> module_binding
    method  module_declaration : module_declaration -> module_declaration
    method  module_expr : module_expr -> module_expr
    method  module_expr_desc : module_expr_desc -> module_expr_desc
    method  module_type : module_type -> module_type
    method  module_type_declaration :
      module_type_declaration -> module_type_declaration
    method  module_type_desc : module_type_desc -> module_type_desc
    method  mutable_flag : Asttypes.mutable_flag -> Asttypes.mutable_flag
    method  nativeint : nativeint -> nativeint
    method  open_description : open_description -> open_description
    method  option : 'a . ('a -> 'a) -> 'a option -> 'a option
    method  override_flag : Asttypes.override_flag -> Asttypes.override_flag
    method  package_type : package_type -> package_type
    method  pattern : pattern -> pattern
    method  pattern_desc : pattern_desc -> pattern_desc
    method  payload : payload -> payload
    method  private_flag : Asttypes.private_flag -> Asttypes.private_flag
    method  rec_flag : Asttypes.rec_flag -> Asttypes.rec_flag
    method  row_field : row_field -> row_field
    method  signature : signature -> signature
    method  signature_item : signature_item -> signature_item
    method  signature_item_desc : signature_item_desc -> signature_item_desc
    method  string : string -> string
    method  structure : structure -> structure
    method  structure_item : structure_item -> structure_item
    method  structure_item_desc : structure_item_desc -> structure_item_desc
    method  toplevel_phrase : toplevel_phrase -> toplevel_phrase
    method  type_declaration : type_declaration -> type_declaration
    method  type_extension : type_extension -> type_extension
    method  type_kind : type_kind -> type_kind
    method  value_binding : value_binding -> value_binding
    method  value_description : value_description -> value_description
    method  variance : Asttypes.variance -> Asttypes.variance
    method  virtual_flag : Asttypes.virtual_flag -> Asttypes.virtual_flag
    method  with_constraint : with_constraint -> with_constraint
  end
