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
  CompilerPropertyName) data=(parser::primary_expressions::PropertyName ::property_name:: compiler) ;;
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

tst ${file}::tests::${modname} || exit
report --name-regex="_3res${#file}${file}([^0-9][^_]+_)?${mangled}" --uncovered --demangled
