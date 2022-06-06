source $HOME/*/rust-e262/funcs.sh

# id ) TypeName test_mod_name file
case $1 in 
  PrivateName) data=($1 private_name:: values) ;;
  ECMAScriptValue) data=($1 ecmascript_value:: values) ;;
  PropertyKey) data=($1 property_key values) ;;
  SymbolInternals) data=($1 symbol_internals values) ;;
  Symbol) data=($1 symbol values) ;;
  PrivateElementKind) data=($1 private_element_kind values) ;;
  PrivateElement) data=($1 private_element:: values) ;;
  Numeric) data=($1 numeric:: values) ;;
  ArrayIndex) data=($1 array_index values) ;;
  ValuesJSString) data=(strings::JSString jsstring values) ;;
  ValuesOption) data=(core::option::Option option_object values) ;;
  SymbolObject) data=($1 symbol_object symbol_object) ;;
  SymbolRegistry) data=($1 symbol_registry symbol_object) ;;
  ( create_symbol_object \
  | provision_symbol_intrinsic \
  | symbol_constructor_function \
  | symbol_for \
  | symbol_key_for \
  | this_symbol_value \
  | symbol_to_string \
  | symbol_value_of \
  | symbol_description \
  ) data=($1 $1 symbol_object) ;;

  # compiler
  Insn) data=($1 insn compiler) ;;
  CompilerStatusFlags) data=($1 compiler_status_flags compiler) ;;
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
  CompilerStatementList) data=(parser::block::StatementList statement_list compiler) ;;
  CompilerStatementListItem) data=(parser::block::StatementListItem statement_list_item compiler) ;;
  CompilerStatement) data=(parser::statements_and_declarations::Statement statement compiler) ;;
  CompilerDeclaration) data=(parser::statements_and_declarations::Declaration declaration compiler) ;;
  CompilerLexicalDeclaration) data=(parser::statements_and_declarations::LexicalDeclaration lexical_declaration compiler) ;;
  CompilerBindingList) data=(parser::statements_and_declarations::BindingList binding_list compiler) ;;
  CompilerLexicalBinding) data=(parser::statements_and_declarations::LexicalBinding lexical_binding compiler) ;;
  CompilerInitializer) data=(parser::primary_expressions::Initializer initializer compiler) ;;
  CompilerScript) data=(parser::scripts::Script script compiler) ;;
  CompilerScriptBody) data=(parser::scripts::ScriptBody scriptbody compiler) ;;
  ArrowParameters) data=($1 arrow_parameters parser::arrow_function_definitions) ;;
  ExpressionBody) data=($1 expression_body parser::arrow_function_definitions) ;;
  ConciseBody) data=($1 concise_body parser::arrow_function_definitions) ;;
  ArrowFormalParameters) data=($1 arrow_formal_parameters parser::arrow_function_definitions) ;;
  ArrowFunction) data=($1 arrow_function parser::arrow_function_definitions) ;;
  AssignmentExpression) data=($1 assignment_expression parser::assignment_operators) ;;
  AssignmentOperator) data=($1 assignment_operator parser::assignment_operators) ;;
  AssignmentPattern) data=($1 assignment_pattern parser::assignment_operators) ;;
  ObjectAssignmentPattern) data=($1 object_assignment_pattern parser::assignment_operators) ;;
  ArrayAssignmentPattern) data=($1 array_assignment_pattern parser::assignment_operators) ;;
  AssignmentRestProperty) data=($1 assignment_rest_property parser::assignment_operators) ;;
  AssignmentPropertyList) data=($1 assignment_property_list parser::assignment_operators) ;;
  AssignmentElementList) data=($1 assignment_element_list parser::assignment_operators) ;;
  AssignmentElisionElement) data=($1 assignment_elision_element parser::assignment_operators) ;;
  AssignmentProperty) data=($1 assignment_property parser::assignment_operators) ;;
  AssignmentElement) data=($1 assignment_element parser::assignment_operators) ;;
  AssignmentRestElement) data=($1 assignment_rest_element parser::assignment_operators) ;;
  DestructuringAssignmentTarget) data=($1 destructuring_assignment_target parser::assignment_operators) ;;
  AsyncArrowFunction) data=($1 async_arrow_function parser::async_arrow_function_definitions) ;;
  AsyncArrowHead) data=($1 async_arrow_head parser::async_arrow_function_definitions) ;;
  AsyncConciseBody) data=($1 async_concise_body parser::async_arrow_function_definitions) ;;
  AsyncArrowBindingIdentifier) data=($1 async_arrow_binding_identifier parser::async_arrow_function_definitions) ;;
  CoverCallExpressionAndAsyncArrowHead) data=($1 cceaaah parser::async_arrow_function_definitions) ;;
  AsyncFunctionDeclaration) data=($1 async_function_declaration parser::async_function_definitions) ;;
  AsyncFunctionExpression) data=($1 async_function_expression parser::async_function_definitions) ;;
  AsyncMethod) data=($1 async_method parser::async_function_definitions) ;;
  AsyncFunctionBody) data=($1 async_function_body parser::async_function_definitions) ;;
  AwaitExpression) data=($1 await_expression parser::async_function_definitions) ;;
  AsyncGeneratorMethod) data=($1 async_generator_method parser::async_generator_function_definitions) ;;
  AsyncGeneratorDeclaration) data=($1 async_generator_declaration parser::async_generator_function_definitions) ;;
  AsyncGeneratorExpression) data=($1 async_generator_expression parser::async_generator_function_definitions) ;;
  AsyncGeneratorBody) data=($1 async_generator_body parser::async_generator_function_definitions) ;;
  BitwiseANDExpression) data=($1 bitwise_and_expression parser::binary_bitwise_operators) ;;
  BitwiseXORExpression) data=($1 bitwise_xor_expression parser::binary_bitwise_operators) ;;
  BitwiseORExpression) data=($1 bitwise_or_expression parser::binary_bitwise_operators) ;;
  LogicalANDExpression) data=($1 logical_and_expression parser::binary_logical_operators) ;;
  LogicalORExpression) data=($1 logical_or_expression parser::binary_logical_operators) ;;
  CoalesceExpression) data=($1 coalesce_expression parser::binary_logical_operators) ;;
  CoalesceExpressionHead) data=($1 coalesce_expression_head parser::binary_logical_operators) ;;
  ShortCircuitExpression) data=($1 short_circuit_expression parser::binary_logical_operators) ;;
  ShiftExpression) data=($1 shift_expression parser::bitwise_shift_operators) ;;
  BlockStatement) data=($1 block_statement parser::block) ;;
  Block) data=($1 block parser::block) ;;
  StatementList) data=($1 statement_list parser::block) ;;
  StatementListItem) data=($1 statement_list_item parser::block) ;;
  BreakStatement) data=($1 break_statement parser::break_statement) ;;
  ClassDeclaration) data=($1 class_declaration parser::class_definitions) ;;
  ClassExpression) data=($1 class_expression parser::class_definitions) ;;
  ClassTail) data=($1 class_tail parser::class_definitions) ;;
  ClassHeritage) data=($1 class_heritage parser::class_definitions) ;;
  ClassBody) data=($1 class_body parser::class_definitions) ;;
  ClassElementList) data=($1 class_element_list parser::class_definitions) ;;
  ClassElement) data=($1 class_element parser::class_definitions) ;;
  FieldDefinition) data=($1 field_definition parser::class_definitions) ;;
  ClassElementName) data=($1 class_element_name parser::class_definitions) ;;
  ClassStaticBlock) data=($1 class_static_block parser::class_definitions) ;;
  ClassStaticBlockBody) data=($1 class_static_block_body parser::class_definitions) ;;
  ClassStaticBlockStatementList) data=($1 class_static_block_statement_list parser::class_definitions) ;;
  Expression) data=($1 expression parser::comma_operator) ;;
  ConditionalExpression) data=($1 conditional_expression parser::conditional_operator) ;;
  ContinueStatement) data=($1 continue_statement parser::continue_statement) ;;
  DebuggerStatement) data=($1 debugger_statement parser::debugger_statement) ;;
  LexicalDeclaration) data=($1 lexical_declaration parser::declarations_and_variables) ;;
  LetOrConst) data=($1 let_or_const parser::declarations_and_variables) ;;
  BindingList) data=($1 binding_list parser::declarations_and_variables) ;;
  LexicalBinding) data=($1 lexical_binding parser::declarations_and_variables) ;;
  VariableStatement) data=($1 variable_statement parser::declarations_and_variables) ;;
  VariableDeclarationList) data=($1 variable_declaration_list parser::declarations_and_variables) ;;
  VariableDeclaration) data=($1 variable_declaration parser::declarations_and_variables) ;;
  BindingPattern) data=($1 binding_pattern parser::declarations_and_variables) ;;
  ObjectBindingPattern) data=($1 object_binding_pattern parser::declarations_and_variables) ;;
  ArrayBindingPattern) data=($1 array_binding_pattern parser::declarations_and_variables) ;;
  BindingRestProperty) data=($1 binding_rest_property parser::declarations_and_variables) ;;
  BindingPropertyList) data=($1 binding_property_list parser::declarations_and_variables) ;;
  BindingElementList) data=($1 binding_element_list parser::declarations_and_variables) ;;
  BindingElisionElement) data=($1 binding_elision_element parser::declarations_and_variables) ;;
  BindingProperty) data=($1 binding_property parser::declarations_and_variables) ;;
  BindingElement) data=($1 binding_element parser::declarations_and_variables) ;;
  SingleNameBinding) data=($1 single_name_binding parser::declarations_and_variables) ;;
  BindingRestElement) data=($1 binding_rest_element parser::declarations_and_variables) ;;
  EmptyStatement) data=($1 empty_statement parser::empty_statement) ;;
  EqualityExpression) data=($1 equality_expression parser::equality_operators) ;;
  ExponentiationExpression) data=($1 exponentiation_expression parser::exponentiation_operator) ;;
  ExpressionStatement) data=($1 expression_statement parser::expression_statement) ;;
  FunctionDeclaration) data=($1 function_declaration parser::function_definitions) ;;
  FunctionExpression) data=($1 function_expression parser::function_definitions) ;;
  FunctionBody) data=($1 function_body parser::function_definitions) ;;
  FunctionStatementList) data=($1 function_statement_list parser::function_definitions) ;;
  GeneratorMethod) data=($1 generator_method parser::generator_function_definitions) ;;
  GeneratorDeclaration) data=($1 generator_declaration parser::generator_function_definitions) ;;
  GeneratorExpression) data=($1 generator_expression parser::generator_function_definitions) ;;
  GeneratorBody) data=($1 generator_body parser::generator_function_definitions) ;;
  YieldExpression) data=($1 yield_expression parser::generator_function_definitions) ;;
  Identifier) data=($1 identifier parser::identifiers) ;;
  IdentifierReference) data=($1 identifier_reference parser::identifiers) ;;
  BindingIdentifier) data=($1 binding_identifier parser::identifiers) ;;
  LabelIdentifier) data=($1 label_identifier parser::identifiers) ;;
  IfStatement) data=($1 if_statement parser::if_statement) ;;
  IterationStatement) data=($1 iteration_statement parser::iteration_statements) ;;
  DoWhileStatement) data=($1 do_while_statement parser::iteration_statements) ;;
  WhileStatement) data=($1 while_statement parser::iteration_statements) ;;
  ForStatement) data=($1 for_statement parser::iteration_statements) ;;
  ForInOfStatement) data=($1 for_in_of_statement parser::iteration_statements) ;;
  ForDeclaration) data=($1 for_declaration parser::iteration_statements) ;;
  ForBinding) data=($1 for_binding parser::iteration_statements) ;;
  LabelledStatement) data=($1 labelled_statement parser::labelled_statements) ;;
  LabelledItem) data=($1 labelled_item parser::labelled_statements) ;;
  MemberExpression) data=($1 member_expression parser::left_hand_side_expressions) ;;
  SuperProperty) data=($1 super_property parser::left_hand_side_expressions) ;;
  MetaProperty) data=($1 meta_property parser::left_hand_side_expressions) ;;
  Arguments) data=($1 arguments parser::left_hand_side_expressions) ;;
  ArgumentList) data=($1 argument_list parser::left_hand_side_expressions) ;;
  NewExpression) data=($1 new_expression parser::left_hand_side_expressions) ;;
  CallMemberExpression) data=($1 call_member_expression parser::left_hand_side_expressions) ;;
  SuperCall) data=($1 super_call parser::left_hand_side_expressions) ;;
  ImportCall) data=($1 import_call parser::left_hand_side_expressions) ;;
  CallExpression) data=($1 call_expression parser::left_hand_side_expressions) ;;
  LeftHandSideExpression) data=($1 left_hand_side_expression parser::left_hand_side_expressions) ;;
  OptionalExpression) data=($1 optional_expression parser::left_hand_side_expressions) ;;
  OptionalChain) data=($1 optional_chain parser::left_hand_side_expressions) ;;
  MethodDefinition) data=($1 method_definition parser::method_definitions) ;;
  PropertySetParameterList) data=($1 property_set_parameter_list parser::method_definitions) ;;
  MultiplicativeExpression) data=($1 multiplicative_expression parser::multiplicative_operators) ;;
  MultiplicativeOperator) data=($1 multiplicative_operator parser::multiplicative_operators) ;;
  UniqueFormalParameters) data=($1 unique_formal_parameters parser::parameter_lists) ;;
  FormalParameters) data=($1 formal_parameters parser::parameter_lists) ;;
  FormalParameterList) data=($1 formal_parameter_list parser::parameter_lists) ;;
  FunctionRestParameter) data=($1 function_rest_parameter parser::parameter_lists) ;;
  FormalParameter) data=($1 formal_parameter parser::parameter_lists) ;;

  *) echo "No type called $1"; exit ;;
esac

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

echo "Testing ${file}::tests::${modname}"

tst ${file}::tests::${modname}
if [ $? -ne 0 ]; then
  rm -f $namelist
  exit
fi
report --name-allowlist=$namelist --uncovered --demangled
rm -f $namelist
