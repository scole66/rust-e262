source $HOME/*/rust-e262/funcs.sh

results=()
uncovered=--uncovered
everything=false
names=()
while [ $# -gt 0 ]; do
  case "$1" in
    --all) uncovered= ;;
    --every) everything=true ;;
    *) names=("${names[@]}" "$1") ;;
  esac
  shift
done

declare -A test_defs
  # id ) TypeName test_mod_name file

test_defs[PrivateName]="PrivateName private_name:: values"
test_defs[ECMAScriptValue]="ECMAScriptValue ecmascript_value:: values"
test_defs[PropertyKey]="PropertyKey property_key values"
test_defs[SymbolInternals]="SymbolInternals symbol_internals values"
test_defs[Symbol]="Symbol symbol values"
test_defs[PrivateElementKind]="PrivateElementKind private_element_kind values"
test_defs[PrivateElement]="PrivateElement private_element:: values"
test_defs[Numeric]="Numeric numeric:: values"
test_defs[ArrayIndex]="ArrayIndex array_index values"
test_defs[ValuesJSString]="strings::JSString jsstring values"
test_defs[ValuesOption]="core::option::Option option_object values"
test_defs[Valuesf64]="f64 f64ish values"
test_defs[ValuesRc]="alloc::rc::Rc bigintish values"
test_defs[same_value_non_numeric]="ECMAScriptValue::same_value_non_numeric ecmascript_value::same_value_non_numeric values"
test_defs[same_value_zero]="ECMAScriptValue::same_value_zero ecmascript_value::same_value_zero values"
test_defs[same_value]="ECMAScriptValue::same_value ecmascript_value::same_value:: values"
test_defs[is_strictly_equal]="ECMAScriptValue::is_strictly_equal ecmascript_value::is_strictly_equal values"
test_defs[is_loosely_equal]="agent::Agent::is_loosely_equal agent::is_loosely_equal values"
test_defs[PropertyKey_is_array_index]="PropertyKey::is_array_index property_key::is_array_index values"
test_defs[to_number_agentless]="to_number_agentless to_number_agentless values"
test_defs[to_core_int_agentless]="to_core_int_agentless to_core_int_agentless values"
test_defs[to_uint32_agentless]="to_uint32_agentless to_uint32_agentless values"
test_defs[to_string_agentless]="to_string_agentless to_string_agentless values"
test_defs[to_object]="to_object to_object values"
test_defs[canonical_numeric_index_string]="canonical_numeric_index_string canonical_numeric_index_string values"

test_defs[SymbolObject]="SymbolObject symbol_object symbol_object"
test_defs[SymbolRegistry]="SymbolRegistry symbol_registry symbol_object"
test_defs[create_symbol_object]="create_symbol_object create_symbol_object symbol_object"
test_defs[provision_symbol_intrinsic]="provision_symbol_intrinsic provision_symbol_intrinsic symbol_object"
test_defs[symbol_constructor_function]="symbol_constructor_function symbol_constructor_function symbol_object"
test_defs[symbol_for]="symbol_for symbol_for symbol_object"
test_defs[symbol_key_for]="symbol_key_for symbol_key_for symbol_object"
test_defs[this_symbol_value]="this_symbol_value this_symbol_value symbol_object"
test_defs[symbol_to_string]="symbol_to_string symbol_to_string symbol_object"
test_defs[symbol_value_of]="symbol_value_of symbol_value_of symbol_object"
test_defs[symbol_description]="symbol_description symbol_description symbol_object"
test_defs[SymbolObject_own_property_keys]="SymbolObject@ObjectInterface::own_property_keys symbol_object::own_keys symbol_object"

test_defs[Insn]="Insn insn compiler"
test_defs[CompilerStatusFlags]="CompilerStatusFlags compiler_status_flags compiler"
test_defs[CompilerIdentifierReference]="parser::identifier::IdentifierReference identifier_reference compiler"
test_defs[CompilerPrimaryExpression]="parser::primary_expressions::PrimaryExpression primary_expression compiler"
test_defs[CompilerLiteral]="parser::primary_expressions::Literal literal compiler"
test_defs[CompilerParenthesizedExpression]="parser::primary_expressions::ParenthesizedExpression parenthesized_expression compiler"
test_defs[CompilerObjectLiteral]="parser::primary_expressions::ObjectLiteral object_literal compiler"
test_defs[CompilerPropertyDefinitionList]="parser::primary_expressions::PropertyDefinitionList property_definition_list compiler"
test_defs[CompilerPropertyDefinition]="parser::primary_expressions::PropertyDefinition property_definition:: compiler"
test_defs[CompilerPropertyName]="parser::primary_expressions::PropertyName property_name:: compiler"
test_defs[CompilerLiteralPropertyName]="parser::primary_expressions::LiteralPropertyName literal_property_name compiler"
test_defs[CompilerComputedPropertyName]="parser::primary_expressions::ComputedPropertyName computed_property_name compiler"
test_defs[CompilerMemberExpression]="parser::left_hand_side_expressions::MemberExpression member_expression compiler"
test_defs[CompilerNewExpression]="parser::left_hand_side_expressions::NewExpression new_expression compiler"
test_defs[CompilerCallExpression]="parser::left_hand_side_expressions::CallExpression call_expression compiler"
test_defs[CompilerCallMemberExpression]="parser::left_hand_side_expressions::CallMemberExpression call_member_expression compiler"
test_defs[CompilerLeftHandSideExpression]="parser::left_hand_side_expressions::LeftHandSideExpression left_hand_side_expression compiler"
test_defs[CompilerArguments]="parser::left_hand_side_expressions::Arguments arguments compiler"
test_defs[CompilerArgumentList]="parser::left_hand_side_expressions::ArgumentList argument_list compiler"
test_defs[CompilerUpdateExpression]="parser::update_expressions::UpdateExpression update_expression compiler"
test_defs[CompilerUnaryExpression]="parser::unary_operators::UnaryExpression unary_expression compiler"
test_defs[CompilerExponentiationExpression]="parser::exponentiation_operator::ExponentiationExpression exponentiation_expression compiler"
test_defs[CompilerMultiplicativeExpression]="parser::multiplicative_operators::MultiplicativeExpression multiplicative_expression compiler"
test_defs[CompilerAdditiveExpression]="parser::additive_operators::AdditiveExpression additive_expression compiler"
test_defs[CompilerShiftExpression]="parser::bitwise_shift_operators::ShiftExpression shift_expression compiler"
test_defs[CompilerRelationalExpression]="parser::relational_operators::RelationalExpression relational_expression compiler"
test_defs[CompilerEqualityExpression]="parser::equality_operators::EqualityExpression equality_expression compiler"
test_defs[CompilerBitwiseANDExpression]="parser::binary_bitwise_operators::BitwiseANDExpression bitwise_and_expression compiler"
test_defs[CompilerBitwiseXORExpression]="parser::binary_bitwise_operators::BitwiseXORExpression bitwise_xor_expression compiler"
test_defs[CompilerBitwiseORExpression]="parser::binary_bitwise_operators::BitwiseORExpression bitwise_or_expression compiler"
test_defs[CompilerLogicalANDExpression]="parser::binary_logical_operators::LogicalANDExpression logical_and_expression compiler"
test_defs[CompilerLogicalORExpression]="parser::binary_logical_operators::LogicalORExpression logical_or_expression compiler"
test_defs[CompilerCoalesceExpression]="parser::binary_logical_operators::CoalesceExpression coalesce_expression:: compiler"
test_defs[CompilerCoalesceExpressionHead]="parser::binary_logical_operators::CoalesceExpressionHead coalesce_expression_head compiler"
test_defs[CompilerShortCircuitExpression]="parser::binary_logical_operators::ShortCircuitExpression short_circuit_expression compiler"
test_defs[CompilerConditionalExpression]="parser::conditional_operator::ConditionalExpression conditional_expression compiler"
test_defs[CompilerAssignmentExpression]="parser::assignment_operators::AssignmentExpression assignment_expression compiler"
test_defs[CompilerExpression]="parser::comma_operator::Expression expression:: compiler"
test_defs[CompilerExpressionStatement]="parser::expression_statement::ExpressionStatement expression_statement compiler"
test_defs[CompilerStatementList]="parser::block::StatementList statement_list:: compiler"
test_defs[CompilerStatementListItem]="parser::block::StatementListItem statement_list_item compiler"
test_defs[CompilerStatement]="parser::statements_and_declarations::Statement statement:: compiler"
test_defs[CompilerStatement]="data=(parser::statements_and_declarations::Statement::compile statement::compile compiler"
test_defs[CompilerStatement]="data=(parser::statements_and_declarations::Statement::labelled_compile statement::labelled_compile compiler"
test_defs[CompilerDeclaration]="parser::statements_and_declarations::Declaration declaration compiler"
test_defs[CompilerLexicalDeclaration]="parser::declarations_and_variables::LexicalDeclaration lexical_declaration compiler"
test_defs[CompilerBindingList]="parser::declarations_and_variables::BindingList binding_list compiler"
test_defs[CompilerLexicalBinding]="parser::declarations_and_variables::LexicalBinding lexical_binding compiler"
test_defs[CompilerBlockStatement]="parser::block::BlockStatement block_statement compiler"
test_defs[CompilerBlock]="parser::block::Block block:: compiler"
test_defs[CompilerInitializer]="parser::primary_expressions::Initializer initializer compiler"
test_defs[CompilerVariableStatement]="parser::declarations_and_variables::VariableStatement variable_statement compiler"
test_defs[CompilerVariableDeclarationList]="parser::declarations_and_variables::VariableDeclarationList variable_declaration_list compiler"
test_defs[CompilerVariableDeclaration]="parser::declarations_and_variables::VariableDeclaration variable_declaration:: compiler"
test_defs[CompilerThrowStatement]="parser::throw_statement::ThrowStatement throw_statement compiler"
test_defs[CompilerScript]="parser::scripts::Script script compiler"
test_defs[CompilerScriptBody]="parser::scripts::ScriptBody script_body compiler"
test_defs[CompilerEmptyStatement]="parser::empty_statement::EmptyStatement empty_statement compiler"
test_defs[CompilerIfStatement]="parser::if_statement::IfStatement if_statement compiler"
test_defs[CompilerBreakStatement]="parser::break_statement::BreakStatement break_statement compiler"
test_defs[CompilerContinueStatement]="parser::continue_statement::ContinueStatement continue_statement compiler"
test_defs[CompilerBreakableStatement]="parser::statements_and_declarations::BreakableStatement breakable_statement compiler"
test_defs[CompilerBreakableStatement]="data=(parser::statements_and_declarations::BreakableStatement::compile breakable_statement::compile compiler"
test_defs[CompilerBreakableStatement]="data=(parser::statements_and_declarations::BreakableStatement::labelled_compile breakable_statement::labelled_compile compiler"
test_defs[CompilerIterationStatement]="parser::iteration_statements::IterationStatement iteration_statement compiler"
test_defs[CompilerDoWhileStatement]="parser::iteration_statements::DoWhileStatement do_while_statement compiler"
test_defs[CompilerSwitchStatement]="parser::switch_statement::SwitchStatement switch_statement compiler"
test_defs[CompilerLabelledStatement]="parser::labelled_statements::LabelledStatement labelled_statement compiler"
test_defs[CompilerLabelledItem]="parser::labelled_statements::LabelledItem labelled_item compiler"
test_defs[CompilerFunctionDeclaration]="parser::function_definitions::FunctionDeclaration function_declaration compiler"
test_defs[RefResult]="RefResult ref_result compiler"
test_defs[AbruptResult]="AbruptResult abrupt_result compiler"
test_defs[AlwaysAbruptResult]="AlwaysAbruptResult always_abrupt_result compiler"
test_defs[AlwaysRefResult]="AlwaysRefResult always_ref_result compiler"
test_defs[AlwaysAbruptRefResult]="AlwaysAbruptRefResult always_abrupt_ref_result compiler"
test_defs[NeverAbruptRefResult]="NeverAbruptRefResult never_abrupt_ref_result compiler"
test_defs[CompilerFcnDef]="agent::FcnDef fcn_def compiler"
test_defs[NameableProduction]="NameableProduction nameable_production compiler"
test_defs[CompilerBindingIdentifier]="parser::identifiers::BindingIdentifier binding_identifier compiler"
test_defs[CompilerBindingElement]="parser::declarations_and_variables::BindingElement binding_element compiler"
test_defs[CompilerBindingPattern]="parser::declarations_and_variables::BindingPattern binding_pattern compiler"
test_defs[CompilerReturnStatement]="parser::return_statement::ReturnStatement return_statement compiler"
test_defs[CompilerFunctionExpression]="parser::function_definitions::FunctionExpression function_expression compiler"
test_defs[CompilerFunctionExpression_instantiate_ordinary_function_expression]="parser::function_definitions::FunctionExpression::instantiate_ordinary_function_expression function_expression::instantiate_ordinary_function_expression compiler"
test_defs[compile_fdi]="compile_fdi compile_fdi compiler"
test_defs[CompilerArrowFunction]="parser::arrow_function_definitions::ArrowFunction arrow_function compiler"
test_defs[CompilerArrowFunction_instantiate_arrow_function_expression]="parser::arrow_function_definitions::ArrowFunction::instantiate_arrow_function_expression arrow_function::instantiate_arrow_function_expression compiler"
test_defs[CompilerConciseBody]="parser::arrow_function_definitions::ConciseBody concise_body compiler"
test_defs[CompilerExpressionBody]="parser::arrow_function_definitions::ExpressionBody expression_body compiler"
test_defs[CompilerParamSource]="function_object::ParamSource param_source compiler"
test_defs[CompilerFormalParameters]="parser::parameter_lists::FormalParameters formal_parameters compiler"
test_defs[CompilerArrowParameters]="parser::arrow_function_definitions::ArrowParameters arrow_parameters compiler"
test_defs[CompilerArrowFormalParameters]="parser::arrow_function_definitions::ArrowFormalParameters arrow_formal_parameters compiler"
test_defs[CompilerUniqueFormalParameters]="parser::parameter_lists::UniqueFormalParameters unique_formal_parameters compiler"
test_defs[compile_initialize_bound_name]="compile_initialize_bound_name compile_initialize_bound_name compiler"
test_defs[CompilerFormalParameterList]="parser::parameter_lists::FormalParameterList formal_parameter_list compiler"
test_defs[CompilerFormalParameter]="parser::parameter_lists::FormalParameter formal_parameter:: compiler"
test_defs[CompilerSingleNameBinding]="parser::declarations_and_variables::SingleNameBinding single_name_binding compiler"
test_defs[CompilerFunctionRestParameter]="parser::parameter_lists::FunctionRestParameter function_rest_parameter compiler"
test_defs[CompilerFunctionBody]="parser::function_definitions::FunctionBody function_body compiler"
test_defs[CompilerFunctionStatementList]="parser::function_definitions::FunctionStatementList function_statement_list compiler"
test_defs[ConstructExpr]="ConstructExpr construct_expr compiler"
test_defs[CompilerAsyncArrowFunction]="parser::async_arrow_function_definitions::AsyncArrowFunction async_arrow_function compiler"
test_defs[compile_new_evaluator]="compile_new_evaluator compile_new_evaluator compiler"
test_defs[CompilerCatchParameter]="parser::try_statement::CatchParameter catch_parameter compiler"
test_defs[CompilerTryStatement]="parser::try_statement::TryStatement try_statement compiler"
test_defs[CompilerFinally]="parser::try_statement::Finally finally compiler"
test_defs[CompilerCatch]="parser::try_statement::Catch catch:: compiler"

test_defs[ArrowParameters]="ArrowParameters arrow_parameters parser::arrow_function_definitions"
test_defs[ExpressionBody]="ExpressionBody expression_body parser::arrow_function_definitions"
test_defs[ConciseBody]="ConciseBody concise_body parser::arrow_function_definitions"
test_defs[ArrowFormalParameters]="ArrowFormalParameters arrow_formal_parameters parser::arrow_function_definitions"
test_defs[ArrowFunction]="ArrowFunction arrow_function parser::arrow_function_definitions"
test_defs[AssignmentExpression]="AssignmentExpression assignment_expression parser::assignment_operators"
test_defs[AssignmentOperator]="AssignmentOperator assignment_operator parser::assignment_operators"
test_defs[AssignmentPattern]="AssignmentPattern assignment_pattern parser::assignment_operators"
test_defs[ObjectAssignmentPattern]="ObjectAssignmentPattern object_assignment_pattern parser::assignment_operators"
test_defs[ArrayAssignmentPattern]="ArrayAssignmentPattern array_assignment_pattern parser::assignment_operators"
test_defs[AssignmentRestProperty]="AssignmentRestProperty assignment_rest_property parser::assignment_operators"
test_defs[AssignmentPropertyList]="AssignmentPropertyList assignment_property_list parser::assignment_operators"
test_defs[AssignmentElementList]="AssignmentElementList assignment_element_list parser::assignment_operators"
test_defs[AssignmentElisionElement]="AssignmentElisionElement assignment_elision_element parser::assignment_operators"
test_defs[AssignmentProperty]="AssignmentProperty assignment_property parser::assignment_operators"
test_defs[AssignmentElement]="AssignmentElement assignment_element parser::assignment_operators"
test_defs[AssignmentRestElement]="AssignmentRestElement assignment_rest_element parser::assignment_operators"
test_defs[DestructuringAssignmentTarget]="DestructuringAssignmentTarget destructuring_assignment_target parser::assignment_operators"
test_defs[AsyncArrowFunction]="AsyncArrowFunction async_arrow_function parser::async_arrow_function_definitions"
test_defs[AsyncArrowHead]="AsyncArrowHead async_arrow_head parser::async_arrow_function_definitions"
test_defs[AsyncConciseBody]="AsyncConciseBody async_concise_body parser::async_arrow_function_definitions"
test_defs[AsyncArrowBindingIdentifier]="AsyncArrowBindingIdentifier async_arrow_binding_identifier parser::async_arrow_function_definitions"
test_defs[CoverCallExpressionAndAsyncArrowHead]="CoverCallExpressionAndAsyncArrowHead cceaaah parser::async_arrow_function_definitions"
test_defs[AsyncFunctionDeclaration]="AsyncFunctionDeclaration async_function_declaration parser::async_function_definitions"
test_defs[AsyncFunctionExpression]="AsyncFunctionExpression async_function_expression parser::async_function_definitions"
test_defs[AsyncMethod]="AsyncMethod async_method parser::async_function_definitions"
test_defs[AsyncFunctionBody]="AsyncFunctionBody async_function_body parser::async_function_definitions"
test_defs[AwaitExpression]="AwaitExpression await_expression parser::async_function_definitions"
test_defs[AsyncGeneratorMethod]="AsyncGeneratorMethod async_generator_method parser::async_generator_function_definitions"
test_defs[AsyncGeneratorDeclaration]="AsyncGeneratorDeclaration async_generator_declaration parser::async_generator_function_definitions"
test_defs[AsyncGeneratorExpression]="AsyncGeneratorExpression async_generator_expression parser::async_generator_function_definitions"
test_defs[AsyncGeneratorBody]="AsyncGeneratorBody async_generator_body parser::async_generator_function_definitions"
test_defs[BitwiseANDExpression]="BitwiseANDExpression bitwise_and_expression parser::binary_bitwise_operators"
test_defs[BitwiseXORExpression]="BitwiseXORExpression bitwise_xor_expression parser::binary_bitwise_operators"
test_defs[BitwiseORExpression]="BitwiseORExpression bitwise_or_expression parser::binary_bitwise_operators"
test_defs[LogicalANDExpression]="LogicalANDExpression logical_and_expression parser::binary_logical_operators"
test_defs[LogicalORExpression]="LogicalORExpression logical_or_expression parser::binary_logical_operators"
test_defs[CoalesceExpression]="CoalesceExpression coalesce_expression parser::binary_logical_operators"
test_defs[CoalesceExpressionHead]="CoalesceExpressionHead coalesce_expression_head parser::binary_logical_operators"
test_defs[ShortCircuitExpression]="ShortCircuitExpression short_circuit_expression parser::binary_logical_operators"
test_defs[ShiftExpression]="ShiftExpression shift_expression parser::bitwise_shift_operators"
test_defs[BlockStatement]="BlockStatement block_statement parser::block"
test_defs[Block]="Block block parser::block"
test_defs[StatementList]="StatementList statement_list parser::block"
test_defs[StatementListItem]="StatementListItem statement_list_item parser::block"
test_defs[BreakStatement]="BreakStatement break_statement parser::break_statement"
test_defs[ClassDeclaration]="ClassDeclaration class_declaration parser::class_definitions"
test_defs[ClassExpression]="ClassExpression class_expression parser::class_definitions"
test_defs[ClassTail]="ClassTail class_tail parser::class_definitions"
test_defs[ClassHeritage]="ClassHeritage class_heritage parser::class_definitions"
test_defs[ClassBody]="ClassBody class_body parser::class_definitions"
test_defs[ClassElementList]="ClassElementList class_element_list parser::class_definitions"
test_defs[ClassElement]="ClassElement class_element parser::class_definitions"
test_defs[FieldDefinition]="FieldDefinition field_definition parser::class_definitions"
test_defs[ClassElementName]="ClassElementName class_element_name parser::class_definitions"
test_defs[ClassStaticBlock]="ClassStaticBlock class_static_block parser::class_definitions"
test_defs[ClassStaticBlockBody]="ClassStaticBlockBody class_static_block_body parser::class_definitions"
test_defs[ClassStaticBlockStatementList]="ClassStaticBlockStatementList class_static_block_statement_list parser::class_definitions"
test_defs[Expression]="Expression expression parser::comma_operator"
test_defs[ConditionalExpression]="ConditionalExpression conditional_expression parser::conditional_operator"
test_defs[ContinueStatement]="ContinueStatement continue_statement parser::continue_statement"
test_defs[DebuggerStatement]="DebuggerStatement debugger_statement parser::debugger_statement"
test_defs[LexicalDeclaration]="LexicalDeclaration lexical_declaration parser::declarations_and_variables"
test_defs[LetOrConst]="LetOrConst let_or_const parser::declarations_and_variables"
test_defs[BindingList]="BindingList binding_list parser::declarations_and_variables"
test_defs[LexicalBinding]="LexicalBinding lexical_binding parser::declarations_and_variables"
test_defs[VariableStatement]="VariableStatement variable_statement parser::declarations_and_variables"
test_defs[VariableDeclarationList]="VariableDeclarationList variable_declaration_list parser::declarations_and_variables"
test_defs[VariableDeclaration]="VariableDeclaration variable_declaration parser::declarations_and_variables"
test_defs[BindingPattern]="BindingPattern binding_pattern parser::declarations_and_variables"
test_defs[ObjectBindingPattern]="ObjectBindingPattern object_binding_pattern parser::declarations_and_variables"
test_defs[ArrayBindingPattern]="ArrayBindingPattern array_binding_pattern parser::declarations_and_variables"
test_defs[BindingRestProperty]="BindingRestProperty binding_rest_property parser::declarations_and_variables"
test_defs[BindingPropertyList]="BindingPropertyList binding_property_list parser::declarations_and_variables"
test_defs[BindingElementList]="BindingElementList binding_element_list parser::declarations_and_variables"
test_defs[BindingElisionElement]="BindingElisionElement binding_elision_element parser::declarations_and_variables"
test_defs[BindingProperty]="BindingProperty binding_property parser::declarations_and_variables"
test_defs[BindingElement]="BindingElement binding_element parser::declarations_and_variables"
test_defs[SingleNameBinding]="SingleNameBinding single_name_binding parser::declarations_and_variables"
test_defs[BindingRestElement]="BindingRestElement binding_rest_element parser::declarations_and_variables"
test_defs[EmptyStatement]="EmptyStatement empty_statement parser::empty_statement"
test_defs[EqualityExpression]="EqualityExpression equality_expression parser::equality_operators"
test_defs[ExponentiationExpression]="ExponentiationExpression exponentiation_expression parser::exponentiation_operator"
test_defs[ExpressionStatement]="ExpressionStatement expression_statement parser::expression_statement"
test_defs[FunctionDeclaration]="FunctionDeclaration function_declaration parser::function_definitions"
test_defs[FunctionExpression]="FunctionExpression function_expression parser::function_definitions"
test_defs[FunctionBody]="FunctionBody function_body parser::function_definitions"
test_defs[FunctionStatementList]="FunctionStatementList function_statement_list parser::function_definitions"
test_defs[GeneratorMethod]="GeneratorMethod generator_method parser::generator_function_definitions"
test_defs[GeneratorDeclaration]="GeneratorDeclaration generator_declaration parser::generator_function_definitions"
test_defs[GeneratorExpression]="GeneratorExpression generator_expression parser::generator_function_definitions"
test_defs[GeneratorBody]="GeneratorBody generator_body parser::generator_function_definitions"
test_defs[YieldExpression]="YieldExpression yield_expression parser::generator_function_definitions"
test_defs[Identifier]="Identifier identifier parser::identifiers"
test_defs[IdentifierReference]="IdentifierReference identifier_reference parser::identifiers"
test_defs[BindingIdentifier]="BindingIdentifier binding_identifier parser::identifiers"
test_defs[LabelIdentifier]="LabelIdentifier label_identifier parser::identifiers"
test_defs[IfStatement]="IfStatement if_statement parser::if_statement"
test_defs[IfStatement]="data=(IfStatement::expression if_statement::expression parser::if_statement"
test_defs[IfStatement]="data=(IfStatement::first_statement if_statement::first_statement parser::if_statement"
test_defs[IterationStatement]="IterationStatement iteration_statement parser::iteration_statements"
test_defs[DoWhileStatement]="DoWhileStatement do_while_statement parser::iteration_statements"
test_defs[WhileStatement]="WhileStatement while_statement parser::iteration_statements"
test_defs[ForStatement]="ForStatement for_statement parser::iteration_statements"
test_defs[ForInOfStatement]="ForInOfStatement for_in_of_statement parser::iteration_statements"
test_defs[ForDeclaration]="ForDeclaration for_declaration parser::iteration_statements"
test_defs[ForBinding]="ForBinding for_binding parser::iteration_statements"
test_defs[LabelledStatement]="LabelledStatement labelled_statement parser::labelled_statements"
test_defs[LabelledItem]="LabelledItem labelled_item parser::labelled_statements"
test_defs[MemberExpression]="MemberExpression member_expression parser::left_hand_side_expressions"
test_defs[SuperProperty]="SuperProperty super_property parser::left_hand_side_expressions"
test_defs[MetaProperty]="MetaProperty meta_property parser::left_hand_side_expressions"
test_defs[Arguments]="Arguments arguments parser::left_hand_side_expressions"
test_defs[ArgumentList]="ArgumentList argument_list parser::left_hand_side_expressions"
test_defs[NewExpression]="NewExpression new_expression parser::left_hand_side_expressions"
test_defs[CallMemberExpression]="CallMemberExpression call_member_expression parser::left_hand_side_expressions"
test_defs[SuperCall]="SuperCall super_call parser::left_hand_side_expressions"
test_defs[ImportCall]="ImportCall import_call parser::left_hand_side_expressions"
test_defs[CallExpression]="CallExpression call_expression parser::left_hand_side_expressions"
test_defs[LeftHandSideExpression]="LeftHandSideExpression left_hand_side_expression parser::left_hand_side_expressions"
test_defs[OptionalExpression]="OptionalExpression optional_expression parser::left_hand_side_expressions"
test_defs[OptionalChain]="OptionalChain optional_chain parser::left_hand_side_expressions"
test_defs[MethodDefinition]="MethodDefinition method_definition parser::method_definitions"
test_defs[PropertySetParameterList]="PropertySetParameterList property_set_parameter_list parser::method_definitions"
test_defs[MultiplicativeExpression]="MultiplicativeExpression multiplicative_expression parser::multiplicative_operators"
test_defs[MultiplicativeOperator]="MultiplicativeOperator multiplicative_operator parser::multiplicative_operators"
test_defs[UniqueFormalParameters]="UniqueFormalParameters unique_formal_parameters parser::parameter_lists"
test_defs[FormalParameters]="FormalParameters formal_parameters parser::parameter_lists"
test_defs[FormalParameterList]="FormalParameterList formal_parameter_list parser::parameter_lists"
test_defs[FunctionRestParameter]="FunctionRestParameter function_rest_parameter parser::parameter_lists"
test_defs[FormalParameter]="FormalParameter formal_parameter:: parser::parameter_lists"
test_defs[PrimaryExpression]="PrimaryExpression primary_expression parser::primary_expressions"
test_defs[Elisions]="Elisions elision parser::primary_expressions"
test_defs[SpreadElement]="SpreadElement spread_element parser::primary_expressions"
test_defs[ElementList]="ElementList element_list parser::primary_expressions"
test_defs[ArrayLiteral]="ArrayLiteral array_literal parser::primary_expressions"
test_defs[Initializer]="Initializer initializer parser::primary_expressions"
test_defs[CoverInitializedName]="CoverInitializedName cover_initialized_name parser::primary_expressions"
test_defs[ComputedPropertyName]="ComputedPropertyName computed_property_name parser::primary_expressions"
test_defs[LiteralPropertyName]="LiteralPropertyName literal_property_name parser::primary_expressions"
test_defs[PropertyName]="PropertyName property_name parser::primary_expressions"
test_defs[PropertyDefinition]="PropertyDefinition property_definition parser::primary_expressions"
test_defs[PropertyDefinitionList]="PropertyDefinitionList property_definition_list parser::primary_expressions"
test_defs[ObjectLiteral]="ObjectLiteral object_literal parser::primary_expressions"
test_defs[Numeric]="Numeric numeric parser::primary_expressions"
test_defs[Literal]="Literal literal parser::primary_expressions"
test_defs[TemplateLiteral]="TemplateLiteral template_literal parser::primary_expressions"
test_defs[SubstitutionTemplate]="SubstitutionTemplate substitution_template parser::primary_expressions"
test_defs[TemplateSpans]="TemplateSpans template_spans parser::primary_expressions"
test_defs[TemplateMiddleList]="TemplateMiddleList template_middle_list parser::primary_expressions"
test_defs[ParenthesizedExpression]="ParenthesizedExpression parenthesized_expression parser::primary_expressions"
test_defs[CoverParenthesizedExpressionAndArrowParameterList]="CoverParenthesizedExpressionAndArrowParameterList cover_parenthesized_expression_and_arrow_parameter_list parser::primary_expressions"
test_defs[RelationalExpression]="RelationalExpression relational_expression parser::relational_operators"
test_defs[ReturnStatement]="ReturnStatement return_statement parser::return_statement"
test_defs[Script]="Script script parser::scripts"
test_defs[ScriptBody]="ScriptBody script_body parser::scripts"
test_defs[Statement]="Statement statement parser::statements_and_declarations"
test_defs[Declaration]="Declaration declaration parser::statements_and_declarations"
test_defs[HoistableDeclaration]="HoistableDeclaration hoistable_declaration parser::statements_and_declarations"
test_defs[BreakableStatement]="BreakableStatement breakable_statement parser::statements_and_declarations"
test_defs[DeclPart]="DeclPart decl_part parser::statements_and_declarations"
test_defs[SwitchStatement]="SwitchStatement switch_statement parser::switch_statement"
test_defs[CaseBlock]="CaseBlock case_block parser::switch_statement"
test_defs[CaseClauses]="CaseClauses case_clauses parser::switch_statement"
test_defs[CaseClause]="CaseClause case_clause parser::switch_statement"
test_defs[DefaultClause]="DefaultClause default_clause parser::switch_statement"
test_defs[ThrowStatement]="ThrowStatement throw_statement parser::throw_statement"
test_defs[TryStatement]="TryStatement try_statement parser::try_statement"
test_defs[Catch]="Catch catch parser::try_statement"
test_defs[Finally]="Finally finally parser::try_statement"
test_defs[CatchParameter]="CatchParameter catch_parameter parser::try_statement"
test_defs[UnaryExpression]="UnaryExpression unary_expression parser::unary_operators"
test_defs[UpdateExpression]="UpdateExpression update_expression parser::update_expressions"
test_defs[WithStatement]="WithStatement with_statement parser::with_statement"

test_defs[Agent]="Agent agent agent"
test_defs[Agent_global_symbol_registry]="Agent::global_symbol_registry agent::global_symbol_registry agent"
test_defs[Agent_new]="Agent::new agent::new agent"
test_defs[Agent_typeof_operator]="Agent::typeof_operator agent::typeof_operator agent"
test_defs[Agent_current_realm_record]="Agent::current_realm_record agent::current_realm_record agent"
test_defs[Agent_delete_ref]="Agent::delete_ref agent::delete_ref agent"
test_defs[Agent_next_object_id]="Agent::next_object_id agent::next_object_id agent"
test_defs[Agent_push_execution_context]="Agent::push_execution_context agent::push_execution_context agent"
test_defs[Agent_apply_string_or_numeric_binary_operator]="Agent::apply_string_or_numeric_binary_operator agent::apply_string_or_numeric_binary_operator agent"
test_defs[Agent_initialize_host_defined_realm]="Agent::initialize_host_defined_realm agent::initialize_host_defined_realm agent"
test_defs[Agent_next_symbol_id]="Agent::next_symbol_id agent::next_symbol_id agent"
test_defs[Agent_prepare_for_execution]="Agent::prepare_for_execution agent::prepare_for_execution agent"
test_defs[Agent_prefix_decrement]="Agent::prefix_decrement agent::prefix_decrement agent"
test_defs[Agent_void_operator]="Agent::void_operator agent::void_operator agent"
test_defs[Agent_intrinsic]="Agent::intrinsic agent::intrinsic agent"
test_defs[Agent_active_function_object]="Agent::active_function_object agent::active_function_object agent"
test_defs[Agent_set_default_global_bindings]="Agent::set_default_global_bindings agent::set_default_global_bindings agent"
test_defs[Agent_binary_operation]="Agent::binary_operation agent::binary_operation agent"
test_defs[Agent_evaluate_call]="Agent::evaluate_call agent::evaluate_call agent"
test_defs[Agent_evaluate]="Agent::evaluate agent::evaluate agent"
test_defs[Agent_execute]="Agent::execute agent::execute agent"
test_defs[Agent_prefix_increment]="Agent::prefix_increment agent::prefix_increment agent"
test_defs[Agent_wks]="Agent::wks agent::wks agent"
test_defs[Agent_current_lexical_environment]="Agent::current_lexical_environment agent::current_lexical_environment agent"
test_defs[Agent_set_realm_global_object]="Agent::set_realm_global_object agent::set_realm_global_object agent"
test_defs[Agent_pop_execution_context]="Agent::pop_execution_context agent::pop_execution_context agent"
test_defs[Agent_two_values]="Agent::two_values agent::two_values agent"
test_defs[Agent_is_less_than]="Agent::is_less_than agent::is_less_than agent"
test_defs[Agent_instanceof_operator]="Agent::instanceof_operator agent::instanceof_operator agent"
test_defs[Agent_create_unmapped_arguments_object]="Agent::create_unmapped_arguments_object agent::create_unmapped_arguments_object agent"
test_defs[Agent_create_mapped_arguments_object]="Agent::create_mapped_arguments_object agent::create_mapped_arguments_object agent"
test_defs[Agent_attach_mapped_arg]="Agent::attach_mapped_arg agent::attach_mapped_arg agent"
test_defs[WellKnownSymbols]="WellKnownSymbols well_known_symbols agent"
test_defs[parse_script]="parse_script parse_script agent"
test_defs[TopLevelLexDecl]="TopLevelLexDecl top_level_lex_decl agent"
test_defs[FcnDef]="FcnDef fcn_def agent"
test_defs[TopLevelVarDecl]="TopLevelVarDecl top_level_var_decl agent"
test_defs[global_declaration_instantiation]="global_declaration_instantiation global_declaration_instantiation agent"
test_defs[script_evaluation]="script_evaluation script_evaluation agent"
test_defs[ProcessError]="ProcessError process_error agent"
test_defs[process_ecmascript]="process_ecmascript process_ecmascript agent"
test_defs[bigint_leftshift]="bigint_leftshift bigint_leftshift agent"
test_defs[bigint_rightshift]="bigint_rightshift bigint_rightshift agent"

test_defs[Removability]="Removability removability environment_record"
test_defs[Strictness]="Strictness strictness environment_record"
test_defs[Mutability]="Mutability mutability environment_record"
test_defs[Binding]="Binding binding:: environment_record"
test_defs[DeclarativeEnvironmentRecord]="DeclarativeEnvironmentRecord declarative_environment_record environment_record"
test_defs[ObjectEnvironmentRecord]="ObjectEnvironmentRecord object_environment_record environment_record"
test_defs[BindingStatus]="BindingStatus binding_status environment_record"
test_defs[FunctionEnvironmentRecord]="FunctionEnvironmentRecord function_environment_record environment_record"
test_defs[GlobalEnvironmentRecord]="GlobalEnvironmentRecord global_environment_record environment_record"
test_defs[get_identifier_reference]="get_identifier_reference get_identifier_reference environment_record"
test_defs[PrivateEnvironmentRecord]="PrivateEnvironmentRecord private_environment_record environment_record"
test_defs[FunctionEnvironmentRecord_has_binding]="FunctionEnvironmentRecord@EnvironmentRecord::has_binding function_environment_record::has_binding environment_record"
test_defs[FunctionEnvironmentRecord_create_mutable_binding]="FunctionEnvironmentRecord@EnvironmentRecord::create_mutable_binding function_environment_record::create_mutable_binding environment_record"
test_defs[FunctionEnvironmentRecord_create_immutable_binding]="FunctionEnvironmentRecord@EnvironmentRecord::create_immutable_binding function_environment_record::create_immutable_binding environment_record"
test_defs[FunctionEnvironmentRecord_initialize_binding]="FunctionEnvironmentRecord@EnvironmentRecord::initialize_binding function_environment_record::initialize_binding environment_record"
test_defs[FunctionEnvironmentRecord_set_mutable_binding]="FunctionEnvironmentRecord@EnvironmentRecord::set_mutable_binding function_environment_record::set_mutable_binding environment_record"
test_defs[FunctionEnvironmentRecord_get_binding_value]="FunctionEnvironmentRecord@EnvironmentRecord::get_binding_value function_environment_record::get_binding_value environment_record"
test_defs[FunctionEnvironmentRecord_delete_binding]="FunctionEnvironmentRecord@EnvironmentRecord::delete_binding function_environment_record::delete_binding environment_record"
test_defs[FunctionEnvironmentRecord_with_base_object]="FunctionEnvironmentRecord@EnvironmentRecord::with_base_object function_environment_record::with_base_object environment_record"
test_defs[FunctionEnvironmentRecord_has_this_binding]="FunctionEnvironmentRecord@EnvironmentRecord::has_this_binding function_environment_record::has_this_binding environment_record"
test_defs[FunctionEnvironmentRecord_has_super_binding]="FunctionEnvironmentRecord@EnvironmentRecord::has_super_binding function_environment_record::has_super_binding environment_record"
test_defs[FunctionEnvironmentRecord_get_outer_env]="FunctionEnvironmentRecord@EnvironmentRecord::get_outer_env function_environment_record::get_outer_env environment_record"
test_defs[FunctionEnvironmentRecord_get_this_binding]="FunctionEnvironmentRecord@EnvironmentRecord::get_this_binding function_environment_record::get_this_binding environment_record"
test_defs[FunctionEnvironmentRecord_bind_this_value]="FunctionEnvironmentRecord@EnvironmentRecord::bind_this_value function_environment_record::bind_this_value environment_record"
test_defs[FunctionEnvironmentRecord_name]="FunctionEnvironmentRecord@EnvironmentRecord::name function_environment_record::name environment_record"
test_defs[FunctionEnvironmentRecord_binding_names]="FunctionEnvironmentRecord@EnvironmentRecord::binding_names function_environment_record::binding_names environment_record"
test_defs[FunctionEnvironmentRecord_get_super_base]="FunctionEnvironmentRecord::get_super_base function_environment_record::get_super_base environment_record"
test_defs[FunctionEnvironmentRecord_new]="FunctionEnvironmentRecord::new function_environment_record::new environment_record"

test_defs[ordinary_has_instance]="agent::Agent::ordinary_has_instance ordinary_has_instance object"
test_defs[PropertyDescriptor]="PropertyDescriptor property_descriptor object"
test_defs[InternalSlotName]="InternalSlotName internal_slot_name object"
test_defs[make_basic_object]="make_basic_object make_basic_object object"
test_defs[call]="call object::tests::call object"
test_defs[initiate_call]="initiate_call initiate_call object"
test_defs[construct]="construct object::tests::construct:: object"
test_defs[to_constructor]="to_constructor to_constructor object"
test_defs[is_compatible_property_descriptor]="is_compatible_property_descriptor is_compatible_property_descriptor object"
test_defs[ordinary_own_property_keys]="ordinary_own_property_keys ordinary_own_property_keys object"
test_defs[OrdinaryObject_own_property_keys]="OrdinaryObject@ObjectInterface::own_property_keys ordinary_object::own_property_keys object"
test_defs[ImmutablePrototypeExoticObject_own_property_keys]="ImmutablePrototypeExoticObject@ObjectInterface::own_property_keys immutable_prototype_exotic_object::own_property_keys object"

test_defs[Chunk]="Chunk chunk chunk"
test_defs[Chunk_add_to_func_stash]="chunk::Chunk::add_to_func_stash chunk::add_to_func_stash chunk"
test_defs[StashedFunctionData]="StashedFunctionData stashed_function_data chunk"

test_defs[ParameterMap]="ParameterMap parameter_map arguments_object"
test_defs[ArgumentsObject]="ArgumentsObject arguments_object arguments_object"
test_defs[ArgumentsObject_get_own_property]="ArgumentsObject@object::ObjectInterface::get_own_property arguments_object::get_own_property arguments_object"
test_defs[ArgumentsObject_define_own_property]="ArgumentsObject@object::ObjectInterface::define_own_property arguments_object::define_own_property arguments_object"
test_defs[ArgumentsObject_set]="ArgumentsObject@object::ObjectInterface::set arguments_object::set arguments_object"
test_defs[ArgumentsObject_get]="ArgumentsObject@object::ObjectInterface::get arguments_object::get:: arguments_object"
test_defs[ArgumentsObject_delete]="ArgumentsObject@object::ObjectInterface::delete arguments_object::delete arguments_object"
test_defs[ArgumentsObject_own_property_keys]="ArgumentsObject@object::ObjectInterface::own_property_keys arguments_object::own_property_keys arguments_object"

test_defs[IntrinsicId]="IntrinsicId intrinsic_id realm"
test_defs[Intrinsics]="Intrinsics intrinsics realm"

test_defs[ArrayObject_own_property_keys]="ArrayObject@object::ObjectInterface::own_property_keys array_object::own_property_keys arrays"
test_defs[ArrayObject_define_own_property]="ArrayObject@object::ObjectInterface::define_own_property array_object::define_own_property arrays"

test_defs[BooleanObject_own_property_keys]="BooleanObject@object::ObjectInterface::own_property_keys own_keys_01 boolean_object"

test_defs[ErrorObject_own_property_keys]="ErrorObject@object::ObjectInterface::own_property_keys error_object_own_property_keys errors"

test_defs[NumberObject_own_property_keys]="NumberObject@object::ObjectInterface::own_property_keys number_object_own_property_keys number_object"

test_defs[IntrinsicId]="IntrinsicId intrinsic_id realm"
test_defs[Intrinsics]="Intrinsics intrinsics realm"
test_defs[create_intrinsics]="create_intrinsics create_intrinsics realm"

test_defs[FunctionDeclaration_instantiate_function_object]="parser::function_definitions::FunctionDeclaration::instantiate_function_object function_declaration::instantiate_function_object function_object"
test_defs[ThisLexicality]="ThisLexicality this_lexicality function_object"
test_defs[ConstructorKind]="ConstructorKind constructor_kind function_object"
test_defs[ordinary_function_create]="ordinary_function_create ordinary_function_create function_object"
test_defs[make_constructor]="make_constructor make_constructor function_object"
test_defs[ClassName]="ClassName class_name function_object"
test_defs[ClassFieldDefinitionRecord]="ClassFieldDefinitionRecord class_field_definition_record function_object"
test_defs[BodySource]="BodySource body_source function_object"
test_defs[ParamSource]="ParamSource param_source function_object"
test_defs[FunctionSource]="FunctionSource function_source function_object"
test_defs[FunctionObjectData]="FunctionObjectData function_object_data function_object"
test_defs[FunctionObject_call]="FunctionObject@CallableObject::call function_object::call function_object"
test_defs[FunctionObject_construct]="FunctionObject@CallableObject::construct function_object::construct function_object"
test_defs[FunctionObject_object]="FunctionObject::object function_object::object function_object"
test_defs[FunctionObject_Agent_prepare_for_ordinary_call]="agent::Agent::prepare_for_ordinary_call agent::prepare_for_ordinary_call function_object"
test_defs[FunctionObject_Agent_ordinary_call_bind_this]="agent::Agent::ordinary_call_bind_this agent::ordinary_call_bind_this function_object"
test_defs[FunctionObject_Agent_ordinary_call_evaluate_body]="agent::Agent::ordinary_call_evaluate_body agent::ordinary_call_evaluate_body function_object"
test_defs[nameify]="nameify nameify function_object"
test_defs[BuiltInFunctionObject]="BuiltInFunctionObject built_in_function_object function_object"
test_defs[FunctionObject_GeneratorDeclaration_instantiate_function_object]="parser::generator_function_definitions::GeneratorDeclaration generator_declaration::instantiate_function_object function_object"
test_defs[FunctionObject_AsyncFunctionDeclaration_instantiate_function_object]="parser::async_function_definitions::AsyncFunctionDeclaration async_function_declaration::instantiate_function_object function_object"
test_defs[FunctionObject_AsyncGeneratorDeclaration_instantiate_function_object]="parser::async_generator_function_definitions::AsyncGeneratorDeclaration async_generator_declaration::instantiate_function_object function_object"
test_defs[function_prototype_call]="function_prototype_call function_prototype_call function_object"
test_defs[FunctionObject_own_property_keys]="FunctionObject@ObjectInterface::own_property_keys function_object::own_property_keys function_object"
test_defs[BuiltInFunctionObject_own_property_keys]="BuiltInFunctionObject@ObjectInterface::own_property_keys built_in_function_object::own_property_keys function_object"

test_defs[ScriptRecord]="ScriptRecord script_record execution_context"
test_defs[ScriptOrModule]="ScriptOrModule script_or_module execution_context"
test_defs[ExecutionContext]="ExecutionContext execution_context execution_context"

if $everything; then
  names=("${!test_defs[@]}")
fi

for name in ${names[@]}; do

  data=(${test_defs[$name]})
  if [ -z "$data" ]; then
    echo "No type called $name"
    exit
  fi

  file=${data[2]}
  modname=${data[1]}
  typename=${data[0]}

  fileparts=($(echo $file | tr : ' '))
  filemangled=
  for part in ${fileparts[@]}; do
    filemangled=${filemangled}${#part}${part}
  done

  case $typename in
    f64)
      regex="_3res${filemangled}d"
      ;;
    *@*)
      front_back=($(echo $typename | tr @ ' '))
      frontparts=($(echo ${front_back[0]} | tr : ' '))
      frontmangled=
      for part in ${frontparts[@]}; do
        frontmangled=${frontmangled}${#part}${part}
      done
      backparts=($(echo ${front_back[1]} | tr : ' '))
      backmangled=
      for part in ${backparts[@]}; do
        backmangled=${backmangled}${#part}${part}
      done
      regex="_3res${filemangled}([^0-9][^_]+_)?${frontmangled}.*${backmangled}"
      ;;
    *)
      typeparts=($(echo $typename | tr : ' '))
      mangled=
      for part in ${typeparts[@]}; do
        mangled=${mangled}${#part}${part}
      done

      regex="_3res${filemangled}([^0-9][^_]+_)?${mangled}"
      ;;
  esac

  echo "Testing ${file}::tests::${modname}"

  location=$(mktemp -d)
  results=("${results[@]}" $location)

  tst -o=${location}/coverage.profdata ${file}::tests::${modname}
  if [ $? -ne 0 ]; then
    for d in ${results[@]}; do
      rm -rf $d
    done
    exit
  fi

  namelist=${location}/namelist
  report --no-color --name-regex=".+" --profile=${location}/coverage.profdata | grep -E "^_.*:$" | grep -E "$regex" | grep -vE "concise_with|pprint" | sed -E 's/(.*):$/allowlist_fun:\1/' > $namelist

  echo "$name" > ${location}/name.txt
done

has_uncovered=()
for d in ${results[@]}; do
  namelist=${d}/namelist
  echo "Rendering:"
  rustfilt < $namelist | sed "s/allowlist_fun:/  * /"
  report --name-allowlist=$namelist $uncovered --demangled --profile=${d}/coverage.profdata | tee ${d}/lines.txt
  if [ $(wc -l < ${d}/lines.txt) -gt 0 ]; then
    has_uncovered=("${has_uncovered[@]}" "$(< ${d}/name.txt)")
  fi
done

for d in ${results[@]}; do
  rm -rf $d
done

if [ ${#has_uncovered[@]} -gt 0 -a ${#results[@]} -gt 1 ]; then
  echo
  echo "Uncovered Items:"
  for item in ${has_uncovered[@]}; do
    echo " * $item"
  done
fi
