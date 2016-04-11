open Parsetree
class ['acc] t :
  object
    method  attribute : attribute -> 'acc -> (attribute* 'acc)
    method  attributes : attributes -> 'acc -> (attributes* 'acc)
    method  bool : bool -> 'acc -> (bool* 'acc)
    method  case : case -> 'acc -> (case* 'acc)
    method  char : char -> 'acc -> (char* 'acc)
    method  class_declaration :
      class_declaration -> 'acc -> (class_declaration* 'acc)
    method  class_description :
      class_description -> 'acc -> (class_description* 'acc)
    method  class_expr : class_expr -> 'acc -> (class_expr* 'acc)
    method  class_expr_desc :
      class_expr_desc -> 'acc -> (class_expr_desc* 'acc)
    method  class_field : class_field -> 'acc -> (class_field* 'acc)
    method  class_field_desc :
      class_field_desc -> 'acc -> (class_field_desc* 'acc)
    method  class_field_kind :
      class_field_kind -> 'acc -> (class_field_kind* 'acc)
    method  class_infos :
      'a .
        ('a -> 'acc -> ('a* 'acc)) ->
          'a class_infos -> 'acc -> ('a class_infos* 'acc)
    method  class_signature :
      class_signature -> 'acc -> (class_signature* 'acc)
    method  class_structure :
      class_structure -> 'acc -> (class_structure* 'acc)
    method  class_type : class_type -> 'acc -> (class_type* 'acc)
    method  class_type_declaration :
      class_type_declaration -> 'acc -> (class_type_declaration* 'acc)
    method  class_type_desc :
      class_type_desc -> 'acc -> (class_type_desc* 'acc)
    method  class_type_field :
      class_type_field -> 'acc -> (class_type_field* 'acc)
    method  class_type_field_desc :
      class_type_field_desc -> 'acc -> (class_type_field_desc* 'acc)
    method  closed_flag :
      Asttypes.closed_flag -> 'acc -> (Asttypes.closed_flag* 'acc)
    method  constant : Asttypes.constant -> 'acc -> (Asttypes.constant* 'acc)
    method  constructor_declaration :
      constructor_declaration -> 'acc -> (constructor_declaration* 'acc)
    method  core_type : core_type -> 'acc -> (core_type* 'acc)
    method  core_type_desc : core_type_desc -> 'acc -> (core_type_desc* 'acc)
    method  direction_flag :
      Asttypes.direction_flag -> 'acc -> (Asttypes.direction_flag* 'acc)
    method  directive_argument :
      directive_argument -> 'acc -> (directive_argument* 'acc)
    method  expression : expression -> 'acc -> (expression* 'acc)
    method  expression_desc :
      expression_desc -> 'acc -> (expression_desc* 'acc)
    method  extension : extension -> 'acc -> (extension* 'acc)
    method  extension_constructor :
      extension_constructor -> 'acc -> (extension_constructor* 'acc)
    method  extension_constructor_kind :
      extension_constructor_kind ->
        'acc -> (extension_constructor_kind* 'acc)
    method  include_declaration :
      include_declaration -> 'acc -> (include_declaration* 'acc)
    method  include_description :
      include_description -> 'acc -> (include_description* 'acc)
    method  include_infos :
      'a .
        ('a -> 'acc -> ('a* 'acc)) ->
          'a include_infos -> 'acc -> ('a include_infos* 'acc)
    method  int : int -> 'acc -> (int* 'acc)
    method  int32 : int32 -> 'acc -> (int32* 'acc)
    method  int64 : int64 -> 'acc -> (int64* 'acc)
    method  label : Asttypes.label -> 'acc -> (Asttypes.label* 'acc)
    method  label_declaration :
      label_declaration -> 'acc -> (label_declaration* 'acc)
    method  lexing_position :
      Lexing.position -> 'acc -> (Lexing.position* 'acc)
    method  list :
      'a . ('a -> 'acc -> ('a* 'acc)) -> 'a list -> 'acc -> ('a list* 'acc)
    method  loc :
      'a .
        ('a -> 'acc -> ('a* 'acc)) ->
          'a Asttypes.loc -> 'acc -> ('a Asttypes.loc* 'acc)
    method  location : Location.t -> 'acc -> (Location.t* 'acc)
    method  longident : Longident.t -> 'acc -> (Longident.t* 'acc)
    method  module_binding : module_binding -> 'acc -> (module_binding* 'acc)
    method  module_declaration :
      module_declaration -> 'acc -> (module_declaration* 'acc)
    method  module_expr : module_expr -> 'acc -> (module_expr* 'acc)
    method  module_expr_desc :
      module_expr_desc -> 'acc -> (module_expr_desc* 'acc)
    method  module_type : module_type -> 'acc -> (module_type* 'acc)
    method  module_type_declaration :
      module_type_declaration -> 'acc -> (module_type_declaration* 'acc)
    method  module_type_desc :
      module_type_desc -> 'acc -> (module_type_desc* 'acc)
    method  mutable_flag :
      Asttypes.mutable_flag -> 'acc -> (Asttypes.mutable_flag* 'acc)
    method  nativeint : nativeint -> 'acc -> (nativeint* 'acc)
    method  open_description :
      open_description -> 'acc -> (open_description* 'acc)
    method  option :
      'a .
        ('a -> 'acc -> ('a* 'acc)) -> 'a option -> 'acc -> ('a option* 'acc)
    method  override_flag :
      Asttypes.override_flag -> 'acc -> (Asttypes.override_flag* 'acc)
    method  package_type : package_type -> 'acc -> (package_type* 'acc)
    method  pattern : pattern -> 'acc -> (pattern* 'acc)
    method  pattern_desc : pattern_desc -> 'acc -> (pattern_desc* 'acc)
    method  payload : payload -> 'acc -> (payload* 'acc)
    method  private_flag :
      Asttypes.private_flag -> 'acc -> (Asttypes.private_flag* 'acc)
    method  rec_flag : Asttypes.rec_flag -> 'acc -> (Asttypes.rec_flag* 'acc)
    method  row_field : row_field -> 'acc -> (row_field* 'acc)
    method  signature : signature -> 'acc -> (signature* 'acc)
    method  signature_item : signature_item -> 'acc -> (signature_item* 'acc)
    method  signature_item_desc :
      signature_item_desc -> 'acc -> (signature_item_desc* 'acc)
    method  string : string -> 'acc -> (string* 'acc)
    method  structure : structure -> 'acc -> (structure* 'acc)
    method  structure_item : structure_item -> 'acc -> (structure_item* 'acc)
    method  structure_item_desc :
      structure_item_desc -> 'acc -> (structure_item_desc* 'acc)
    method  toplevel_phrase :
      toplevel_phrase -> 'acc -> (toplevel_phrase* 'acc)
    method  type_declaration :
      type_declaration -> 'acc -> (type_declaration* 'acc)
    method  type_extension : type_extension -> 'acc -> (type_extension* 'acc)
    method  type_kind : type_kind -> 'acc -> (type_kind* 'acc)
    method  value_binding : value_binding -> 'acc -> (value_binding* 'acc)
    method  value_description :
      value_description -> 'acc -> (value_description* 'acc)
    method  variance : Asttypes.variance -> 'acc -> (Asttypes.variance* 'acc)
    method  virtual_flag :
      Asttypes.virtual_flag -> 'acc -> (Asttypes.virtual_flag* 'acc)
    method  with_constraint :
      with_constraint -> 'acc -> (with_constraint* 'acc)
  end
