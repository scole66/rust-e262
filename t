source $HOME/*/rust-e262/funcs.sh

all=false
name=
while [ $# -gt 0 ]; do
  case "$1" in
    --all) all=true ;;
    *) name="$1" ;;
  esac
  shift
done

# id ) TypeName test_mod_name file
case $name in 
  PrivateName) data=($name private_name:: values) ;;
  ECMAScriptValue) data=($name ecmascript_value:: values) ;;
  PropertyKey) data=($name property_key values) ;;
  SymbolInternals) data=($name symbol_internals values) ;;
  Symbol) data=($name symbol values) ;;
  PrivateElementKind) data=($name private_element_kind values) ;;
  PrivateElement) data=($name private_element:: values) ;;
  Numeric) data=($name numeric:: values) ;;
  ArrayIndex) data=($name array_index values) ;;
  ValuesJSString) data=(strings::JSString jsstring values) ;;
  ValuesOption) data=(core::option::Option option_object values) ;;
  SymbolObject) data=($name symbol_object symbol_object) ;;
  SymbolRegistry) data=($name symbol_registry symbol_object) ;;
  ( create_symbol_object \
  | provision_symbol_intrinsic \
  | symbol_constructor_function \
  | symbol_for \
  | symbol_key_for \
  | this_symbol_value \
  | symbol_to_string \
  | symbol_value_of \
  | symbol_description \
  ) data=($name $name symbol_object) ;;

  # compiler
  Insn) data=($name insn compiler) ;;
  CompilerStatusFlags) data=($name compiler_status_flags compiler) ;;
  CompilerIdentifierReference) data=(parser::identifier::IdentifierReference identifier_reference compiler) ;;
  CompilerPrimaryExpression) data=(parser::primary_expressions::PrimaryExpression primary_expression compiler) ;;
  CompilerLiteral) data=(parser::primary_expressions::Literal literal compiler) ;;
  CompilerParenthesizedExpression) data=(parser::primary_expressions::ParenthesizedExpression parenthesized_expression compiler) ;;
  CompilerObjectLiteral) data=(parser::primary_expressions::ObjectLiteral object_literal compiler) ;;
  CompilerPropertyDefinitionList) data=(parser::primary_expressions::PropertyDefinitionList property_definition_list compiler) ;;
  CompilerPropertyDefinition) data=(parser::primary_expressions::PropertyDefinition property_definition:: compiler) ;;
  CompilerPropertyName) data=(parser::primary_expressions::PropertyName property_name:: compiler) ;;
  CompilerLiteralPropertyName) data=(parser::primary_expressions::LiteralPropertyName literal_property_name compiler) ;;
  CompilerComputedPropertyName) data=(parser::primary_expressions::ComputedPropertyName computed_property_name compiler) ;;
  CompilerMemberExpression) data=(parser::left_hand_side_expressions::MemberExpression member_expression compiler) ;;
  CompilerNewExpression) data=(parser::left_hand_side_expressions::NewExpression new_expression compiler) ;;
  CompilerCallExpression) data=(parser::left_hand_side_expressions::CallExpression call_expression compiler) ;;
  CompilerCallMemberExpression) data=(parser::left_hand_side_expressions::CallMemberExpression call_member_expression compiler) ;;
  CompilerLeftHandSideExpression) data=(parser::left_hand_side_expressions::LeftHandSideExpression left_hand_side_expression compiler) ;;
  CompilerArguments) data=(parser::left_hand_side_expressions::Arguments arguments compiler) ;;
  CompilerArgumentList) data=(parser::left_hand_side_expressions::ArgumentList argument_list compiler) ;;
  CompilerUpdateExpression) data=(parser::update_expressions::UpdateExpression update_expression compiler) ;;
  CompilerUnaryExpression) data=(parser::unary_operators::UnaryExpression unary_expression compiler) ;;
  CompilerExponentiationExpression) data=(parser::exponentiation_operator::ExponentiationExpression exponentiation_expression compiler) ;;
  CompilerMultiplicativeExpression) data=(parser::multiplicative_operators::MultiplicativeExpression multiplicative_expression compiler) ;;
  CompilerAdditiveExpression) data=(parser::additive_operators::AdditiveExpression additive_expression compiler) ;;
  CompilerShiftExpression) data=(parser::bitwise_shift_operators::ShiftExpression shift_expression compiler) ;;
  CompilerRelationalExpression) data=(parser::relational_operators::RelationalExpression relational_expression compiler) ;;
  CompilerEqualityExpression) data=(parser::equality_operators::EqualityExpression equality_expression compiler) ;;
  CompilerBitwiseANDExpression) data=(parser::bitwise_bitwise_operators::BitwiseANDExpression bitwise_and_expression compiler) ;;
  CompilerBitwiseXORExpression) data=(parser::bitwise_bitwise_operators::BitwiseXORExpression bitwise_xor_expression compiler) ;;
  CompilerBitwiseORExpression) data=(parser::bitwise_bitwise_operators::BitwiseORExpression bitwise_or_expression compiler) ;;
  CompilerLogicalANDExpression) data=(parser::bitwise_logical_operators::LogicalANDExpression logical_and_expression compiler) ;;
  CompilerLogicalORExpression) data=(parser::bitwise_logical_operators::LogicalORExpression logical_or_expression compiler) ;;
  CompilerShortCircuitExpression) data=(parser::bitwise_logical_operators::ShortCircuitExpression short_circuit_expression compiler) ;;
  CompilerConditionalExpression) data=(parser::conditional_operator::ConditionalExpression conditional_expression compiler) ;;
  CompilerAssignmentExpression) data=(parser::assignment_operators::AssignmentExpression assignment_expression compiler) ;;
  CompilerExpression) data=(parser::comma_operator::Expression expression compiler) ;;
  CompilerExpressionStatement) data=(parser::expression_statement::ExpressionStatement expression_statement compiler) ;;
  CompilerStatementList) data=(parser::block::StatementList statement_list:: compiler) ;;
  CompilerStatementListItem) data=(parser::block::StatementListItem statement_list_item compiler) ;;
  CompilerStatement) data=(parser::statements_and_declarations::Statement statement compiler) ;;
  CompilerDeclaration) data=(parser::statements_and_declarations::Declaration declaration compiler) ;;
  CompilerLexicalDeclaration) data=(parser::declarations_and_variables::LexicalDeclaration lexical_declaration compiler) ;;
  CompilerBindingList) data=(parser::declarations_and_variables::BindingList binding_list compiler) ;;
  CompilerLexicalBinding) data=(parser::declarations_and_variables::LexicalBinding lexical_binding compiler) ;;
  CompilerInitializer) data=(parser::primary_expressions::Initializer initializer compiler) ;;
  CompilerScript) data=(parser::scripts::Script script compiler) ;;
  CompilerScriptBody) data=(parser::scripts::ScriptBody scriptbody compiler) ;;
  ArrowParameters) data=($name arrow_parameters parser::arrow_function_definitions) ;;
  ExpressionBody) data=($name expression_body parser::arrow_function_definitions) ;;
  ConciseBody) data=($name concise_body parser::arrow_function_definitions) ;;
  ArrowFormalParameters) data=($name arrow_formal_parameters parser::arrow_function_definitions) ;;
  ArrowFunction) data=($name arrow_function parser::arrow_function_definitions) ;;
  AssignmentExpression) data=($name assignment_expression parser::assignment_operators) ;;
  AssignmentOperator) data=($name assignment_operator parser::assignment_operators) ;;
  AssignmentPattern) data=($name assignment_pattern parser::assignment_operators) ;;
  ObjectAssignmentPattern) data=($name object_assignment_pattern parser::assignment_operators) ;;
  ArrayAssignmentPattern) data=($name array_assignment_pattern parser::assignment_operators) ;;
  AssignmentRestProperty) data=($name assignment_rest_property parser::assignment_operators) ;;
  AssignmentPropertyList) data=($name assignment_property_list parser::assignment_operators) ;;
  AssignmentElementList) data=($name assignment_element_list parser::assignment_operators) ;;
  AssignmentElisionElement) data=($name assignment_elision_element parser::assignment_operators) ;;
  AssignmentProperty) data=($name assignment_property parser::assignment_operators) ;;
  AssignmentElement) data=($name assignment_element parser::assignment_operators) ;;
  AssignmentRestElement) data=($name assignment_rest_element parser::assignment_operators) ;;
  DestructuringAssignmentTarget) data=($name destructuring_assignment_target parser::assignment_operators) ;;
  AsyncArrowFunction) data=($name async_arrow_function parser::async_arrow_function_definitions) ;;
  AsyncArrowHead) data=($name async_arrow_head parser::async_arrow_function_definitions) ;;
  AsyncConciseBody) data=($name async_concise_body parser::async_arrow_function_definitions) ;;
  AsyncArrowBindingIdentifier) data=($name async_arrow_binding_identifier parser::async_arrow_function_definitions) ;;
  CoverCallExpressionAndAsyncArrowHead) data=($name cceaaah parser::async_arrow_function_definitions) ;;
  AsyncFunctionDeclaration) data=($name async_function_declaration parser::async_function_definitions) ;;
  AsyncFunctionExpression) data=($name async_function_expression parser::async_function_definitions) ;;
  AsyncMethod) data=($name async_method parser::async_function_definitions) ;;
  AsyncFunctionBody) data=($name async_function_body parser::async_function_definitions) ;;
  AwaitExpression) data=($name await_expression parser::async_function_definitions) ;;
  AsyncGeneratorMethod) data=($name async_generator_method parser::async_generator_function_definitions) ;;
  AsyncGeneratorDeclaration) data=($name async_generator_declaration parser::async_generator_function_definitions) ;;
  AsyncGeneratorExpression) data=($name async_generator_expression parser::async_generator_function_definitions) ;;
  AsyncGeneratorBody) data=($name async_generator_body parser::async_generator_function_definitions) ;;
  BitwiseANDExpression) data=($name bitwise_and_expression parser::binary_bitwise_operators) ;;
  BitwiseXORExpression) data=($name bitwise_xor_expression parser::binary_bitwise_operators) ;;
  BitwiseORExpression) data=($name bitwise_or_expression parser::binary_bitwise_operators) ;;
  LogicalANDExpression) data=($name logical_and_expression parser::binary_logical_operators) ;;
  LogicalORExpression) data=($name logical_or_expression parser::binary_logical_operators) ;;
  CoalesceExpression) data=($name coalesce_expression parser::binary_logical_operators) ;;
  CoalesceExpressionHead) data=($name coalesce_expression_head parser::binary_logical_operators) ;;
  ShortCircuitExpression) data=($name short_circuit_expression parser::binary_logical_operators) ;;
  ShiftExpression) data=($name shift_expression parser::bitwise_shift_operators) ;;
  BlockStatement) data=($name block_statement parser::block) ;;
  Block) data=($name block parser::block) ;;
  StatementList) data=($name statement_list parser::block) ;;
  StatementListItem) data=($name statement_list_item parser::block) ;;
  BreakStatement) data=($name break_statement parser::break_statement) ;;
  ClassDeclaration) data=($name class_declaration parser::class_definitions) ;;
  ClassExpression) data=($name class_expression parser::class_definitions) ;;
  ClassTail) data=($name class_tail parser::class_definitions) ;;
  ClassHeritage) data=($name class_heritage parser::class_definitions) ;;
  ClassBody) data=($name class_body parser::class_definitions) ;;
  ClassElementList) data=($name class_element_list parser::class_definitions) ;;
  ClassElement) data=($name class_element parser::class_definitions) ;;
  FieldDefinition) data=($name field_definition parser::class_definitions) ;;
  ClassElementName) data=($name class_element_name parser::class_definitions) ;;
  ClassStaticBlock) data=($name class_static_block parser::class_definitions) ;;
  ClassStaticBlockBody) data=($name class_static_block_body parser::class_definitions) ;;
  ClassStaticBlockStatementList) data=($name class_static_block_statement_list parser::class_definitions) ;;
  Expression) data=($name expression parser::comma_operator) ;;
  ConditionalExpression) data=($name conditional_expression parser::conditional_operator) ;;
  ContinueStatement) data=($name continue_statement parser::continue_statement) ;;
  DebuggerStatement) data=($name debugger_statement parser::debugger_statement) ;;
  LexicalDeclaration) data=($name lexical_declaration parser::declarations_and_variables) ;;
  LetOrConst) data=($name let_or_const parser::declarations_and_variables) ;;
  BindingList) data=($name binding_list parser::declarations_and_variables) ;;
  LexicalBinding) data=($name lexical_binding parser::declarations_and_variables) ;;
  VariableStatement) data=($name variable_statement parser::declarations_and_variables) ;;
  VariableDeclarationList) data=($name variable_declaration_list parser::declarations_and_variables) ;;
  VariableDeclaration) data=($name variable_declaration parser::declarations_and_variables) ;;
  BindingPattern) data=($name binding_pattern parser::declarations_and_variables) ;;
  ObjectBindingPattern) data=($name object_binding_pattern parser::declarations_and_variables) ;;
  ArrayBindingPattern) data=($name array_binding_pattern parser::declarations_and_variables) ;;
  BindingRestProperty) data=($name binding_rest_property parser::declarations_and_variables) ;;
  BindingPropertyList) data=($name binding_property_list parser::declarations_and_variables) ;;
  BindingElementList) data=($name binding_element_list parser::declarations_and_variables) ;;
  BindingElisionElement) data=($name binding_elision_element parser::declarations_and_variables) ;;
  BindingProperty) data=($name binding_property parser::declarations_and_variables) ;;
  BindingElement) data=($name binding_element parser::declarations_and_variables) ;;
  SingleNameBinding) data=($name single_name_binding parser::declarations_and_variables) ;;
  BindingRestElement) data=($name binding_rest_element parser::declarations_and_variables) ;;
  EmptyStatement) data=($name empty_statement parser::empty_statement) ;;
  EqualityExpression) data=($name equality_expression parser::equality_operators) ;;
  ExponentiationExpression) data=($name exponentiation_expression parser::exponentiation_operator) ;;
  ExpressionStatement) data=($name expression_statement parser::expression_statement) ;;
  FunctionDeclaration) data=($name function_declaration parser::function_definitions) ;;
  FunctionExpression) data=($name function_expression parser::function_definitions) ;;
  FunctionBody) data=($name function_body parser::function_definitions) ;;
  FunctionStatementList) data=($name function_statement_list parser::function_definitions) ;;
  GeneratorMethod) data=($name generator_method parser::generator_function_definitions) ;;
  GeneratorDeclaration) data=($name generator_declaration parser::generator_function_definitions) ;;
  GeneratorExpression) data=($name generator_expression parser::generator_function_definitions) ;;
  GeneratorBody) data=($name generator_body parser::generator_function_definitions) ;;
  YieldExpression) data=($name yield_expression parser::generator_function_definitions) ;;
  Identifier) data=($name identifier parser::identifiers) ;;
  IdentifierReference) data=($name identifier_reference parser::identifiers) ;;
  BindingIdentifier) data=($name binding_identifier parser::identifiers) ;;
  LabelIdentifier) data=($name label_identifier parser::identifiers) ;;
  IfStatement) data=($name if_statement parser::if_statement) ;;
  IterationStatement) data=($name iteration_statement parser::iteration_statements) ;;
  DoWhileStatement) data=($name do_while_statement parser::iteration_statements) ;;
  WhileStatement) data=($name while_statement parser::iteration_statements) ;;
  ForStatement) data=($name for_statement parser::iteration_statements) ;;
  ForInOfStatement) data=($name for_in_of_statement parser::iteration_statements) ;;
  ForDeclaration) data=($name for_declaration parser::iteration_statements) ;;
  ForBinding) data=($name for_binding parser::iteration_statements) ;;
  LabelledStatement) data=($name labelled_statement parser::labelled_statements) ;;
  LabelledItem) data=($name labelled_item parser::labelled_statements) ;;
  MemberExpression) data=($name member_expression parser::left_hand_side_expressions) ;;
  SuperProperty) data=($name super_property parser::left_hand_side_expressions) ;;
  MetaProperty) data=($name meta_property parser::left_hand_side_expressions) ;;
  Arguments) data=($name arguments parser::left_hand_side_expressions) ;;
  ArgumentList) data=($name argument_list parser::left_hand_side_expressions) ;;
  NewExpression) data=($name new_expression parser::left_hand_side_expressions) ;;
  CallMemberExpression) data=($name call_member_expression parser::left_hand_side_expressions) ;;
  SuperCall) data=($name super_call parser::left_hand_side_expressions) ;;
  ImportCall) data=($name import_call parser::left_hand_side_expressions) ;;
  CallExpression) data=($name call_expression parser::left_hand_side_expressions) ;;
  LeftHandSideExpression) data=($name left_hand_side_expression parser::left_hand_side_expressions) ;;
  OptionalExpression) data=($name optional_expression parser::left_hand_side_expressions) ;;
  OptionalChain) data=($name optional_chain parser::left_hand_side_expressions) ;;
  MethodDefinition) data=($name method_definition parser::method_definitions) ;;
  PropertySetParameterList) data=($name property_set_parameter_list parser::method_definitions) ;;
  MultiplicativeExpression) data=($name multiplicative_expression parser::multiplicative_operators) ;;
  MultiplicativeOperator) data=($name multiplicative_operator parser::multiplicative_operators) ;;
  UniqueFormalParameters) data=($name unique_formal_parameters parser::parameter_lists) ;;
  FormalParameters) data=($name formal_parameters parser::parameter_lists) ;;
  FormalParameterList) data=($name formal_parameter_list parser::parameter_lists) ;;
  FunctionRestParameter) data=($name function_rest_parameter parser::parameter_lists) ;;
  FormalParameter) data=($name formal_parameter:: parser::parameter_lists) ;;
  PrimaryExpression) data=($name primary_expression parser::primary_expressions) ;;
  Elisions) data=($name elision parser::primary_expressions) ;;
  SpreadElement) data=($name spread_element parser::primary_expressions) ;;
  ElementList) data=($name element_list parser::primary_expressions) ;;
  ArrayLiteral) data=($name array_literal parser::primary_expressions) ;;
  Initializer) data=($name initializer parser::primary_expressions) ;;
  CoverInitializedName) data=($name cover_initialized_name parser::primary_expressions) ;;
  ComputedPropertyName) data=($name computed_property_name parser::primary_expressions) ;;
  LiteralPropertyName) data=($name literal_property_name parser::primary_expressions) ;;
  PropertyName) data=($name property_name parser::primary_expressions) ;;
  PropertyDefinition) data=($name property_definition parser::primary_expressions) ;;
  PropertyDefinitionList) data=($name property_definition_list parser::primary_expressions) ;;
  ObjectLiteral) data=($name object_literal parser::primary_expressions) ;;
  Numeric) data=($name numeric parser::primary_expressions) ;;
  Literal) data=($name literal parser::primary_expressions) ;;
  TemplateLiteral) data=($name template_literal parser::primary_expressions) ;;
  SubstitutionTemplate) data=($name substitution_template parser::primary_expressions) ;;
  TemplateSpans) data=($name template_spans parser::primary_expressions) ;;
  TemplateMiddleList) data=($name template_middle_list parser::primary_expressions) ;;
  ParenthesizedExpression) data=($name parenthesized_expression parser::primary_expressions) ;;
  CoverParenthesizedExpressionAndArrowParameterList) data=($name cover_parenthesized_expression_and_arrow_parameter_list parser::primary_expressions) ;;
  RelationalExpression) data=($name relational_expression parser::relational_operators) ;;
  ReturnStatement) data=($name return_statement parser::return_statement) ;;
  Script) data=($name script parser::scripts) ;;
  ScriptBody) data=($name script_body parser::scripts) ;;
  Statement) data=($name statement parser::statements_and_declarations) ;;
  Declaration) data=($name declaration parser::statements_and_declarations) ;;
  HoistableDeclaration) data=($name hoistable_declaration parser::statements_and_declarations) ;;
  BreakableStatement) data=($name breakable_statement parser::statements_and_declarations) ;;
  SwitchStatement) data=($name switch_statement parser::switch_statement) ;;
  CaseBlock) data=($name case_block parser::switch_statement) ;;
  CaseClauses) data=($name case_clauses parser::switch_statement) ;;
  CaseClause) data=($name case_clause parser::switch_statement) ;;
  DefaultClause) data=($name default_clause parser::switch_statement) ;;
  ThrowStatement) data=($name throw_statement parser::throw_statement) ;;
  TryStatement) data=($name try_statement parser::try_statement) ;;
  Catch) data=($name catch parser::try_statement) ;;
  Finally) data=($name finally parser::try_statement) ;;
  CatchParameter) data=($name catch_parameter parser::try_statement) ;;
  UnaryExpression) data=($name unary_expression parser::unary_operators) ;;
  UpdateExpression) data=($name update_expression parser::update_expressions) ;;
  WithStatement) data=($name with_statement parser::with_statement) ;;

  Agent) data=($name agent agent) ;;
  WellKnownSymbols) data=($name well_known_symbols agent) ;;
  parse_script) data=($name $name agent) ;;
  TopLevelLexDecl) data=($name top_level_lex_decl agent) ;;
  TopLevelFcnDef) data=($name top_level_fcn_def agent) ;;
  global_declaration_instantiation) data=($name $name agent) ;;
  script_evaluation) data=($name $name agent) ;;
  ProcessError) data=($name process_error agent) ;;
  process_ecmascript) data=($name $name agent) ;;

  *) echo "No type called $name"; exit ;;
esac

# Make sure we compile
if ! cargo check --tests; then
  exit
fi

file=${data[2]}
modname=${data[1]}
typename=${data[0]}

typeparts=($(echo $typename | tr : ' '))
mangled=
for part in ${typeparts[@]}; do
  mangled=${mangled}${#part}${part}
done

fileparts=($(echo $file | tr : ' '))
filemangled=
for part in ${fileparts[@]}; do
  filemangled=${filemangled}${#part}${part}
done

regex="_3res${filemangled}([^0-9][^_]+_)?${mangled}"

namelist=$(mktemp)
report --no-color --name-regex=".+" | grep -E "^_.*:$" | grep -E "$regex" | grep -vE "concise_with|pprint" | sed -E 's/(.*):$/allowlist_fun:\1/' >> $namelist


echo "Testing ${file}::tests::${modname}, and rendering"
rustfilt < $namelist | sed "s/allowlist_fun:/  * /"

tst ${file}::tests::${modname}
if [ $? -ne 0 ]; then
  rm -f $namelist
  exit
fi

if $all; then
  uncovered=
else
  uncovered=--uncovered
fi
report --name-allowlist=$namelist $uncovered --demangled
rm -f $namelist
