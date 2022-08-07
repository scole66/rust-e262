source $HOME/*/rust-e262/funcs.sh

results=()
uncovered=--uncovered
names=()
while [ $# -gt 0 ]; do
  case "$1" in
    --all) uncovered= ;;
    *) names=("${names[@]}" "$1") ;;
  esac
  shift
done

for name in ${names[@]}; do
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
    Valuesf64) data=(f64 f64ish values) ;;
    ValuesRc) data=(alloc::rc::Rc bigintish values) ;;
    same_value_non_numeric) data=(ECMAScriptValue::$name ecmascript_value::$name values) ;;
    same_value_zero) data=(ECMAScriptValue::$name ecmascript_value::$name values) ;;
    same_value) data=(ECMAScriptValue::$name ecmascript_value::${name}:: values) ;;
    is_strictly_equal) data=(ECMAScriptValue::$name ecmascript_value::${name} values) ;;
    is_loosely_equal) data=(agent::Agent::$name agent::$name values) ;;

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
    CompilerBitwiseANDExpression) data=(parser::binary_bitwise_operators::BitwiseANDExpression bitwise_and_expression compiler) ;;
    CompilerBitwiseXORExpression) data=(parser::binary_bitwise_operators::BitwiseXORExpression bitwise_xor_expression compiler) ;;
    CompilerBitwiseORExpression) data=(parser::binary_bitwise_operators::BitwiseORExpression bitwise_or_expression compiler) ;;
    CompilerLogicalANDExpression) data=(parser::binary_logical_operators::LogicalANDExpression logical_and_expression compiler) ;;
    CompilerLogicalORExpression) data=(parser::binary_logical_operators::LogicalORExpression logical_or_expression compiler) ;;
    CompilerCoalesceExpression) data=(parser::binary_logical_operators::CoalesceExpression coalesce_expression:: compiler) ;;
    CompilerCoalesceExpressionHead) data=(parser::binary_logical_operators::CoalesceExpressionHead coalesce_expression_head compiler) ;;
    CompilerShortCircuitExpression) data=(parser::binary_logical_operators::ShortCircuitExpression short_circuit_expression compiler) ;;
    CompilerConditionalExpression) data=(parser::conditional_operator::ConditionalExpression conditional_expression compiler) ;;
    CompilerAssignmentExpression) data=(parser::assignment_operators::AssignmentExpression assignment_expression compiler) ;;
    CompilerExpression) data=(parser::comma_operator::Expression expression:: compiler) ;;
    CompilerExpressionStatement) data=(parser::expression_statement::ExpressionStatement expression_statement compiler) ;;
    CompilerStatementList) data=(parser::block::StatementList statement_list:: compiler) ;;
    CompilerStatementListItem) data=(parser::block::StatementListItem statement_list_item compiler) ;;
    CompilerStatement) data=(parser::statements_and_declarations::Statement statement:: compiler) ;;
    CompilerStatement-compile) data=(parser::statements_and_declarations::Statement::compile statement::compile compiler) ;;
    CompilerStatement-labelled_compile) data=(parser::statements_and_declarations::Statement::labelled_compile statement::labelled_compile compiler) ;;
    CompilerDeclaration) data=(parser::statements_and_declarations::Declaration declaration compiler) ;;
    CompilerLexicalDeclaration) data=(parser::declarations_and_variables::LexicalDeclaration lexical_declaration compiler) ;;
    CompilerBindingList) data=(parser::declarations_and_variables::BindingList binding_list compiler) ;;
    CompilerLexicalBinding) data=(parser::declarations_and_variables::LexicalBinding lexical_binding compiler) ;;
    CompilerBlockStatement) data=(parser::block::BlockStatement block_statement compiler) ;;
    CompilerBlock) data=(parser::block::Block block:: compiler) ;;
    CompilerInitializer) data=(parser::primary_expressions::Initializer initializer compiler) ;;
    CompilerVariableStatement) data=(parser::declarations_and_variables::VariableStatement variable_statement compiler) ;;
    CompilerVariableDeclarationList) data=(parser::declarations_and_variables::VariableDeclarationList variable_declaration_list compiler) ;;
    CompilerVariableDeclaration) data=(parser::declarations_and_variables::VariableDeclaration variable_declaration:: compiler) ;;
    CompilerThrowStatement) data=(parser::throw_statement::ThrowStatement throw_statement compiler) ;;
    CompilerScript) data=(parser::scripts::Script script compiler) ;;
    CompilerScriptBody) data=(parser::scripts::ScriptBody script_body compiler) ;;
    CompilerEmptyStatement) data=(parser::empty_statement::EmptyStatement empty_statement compiler) ;;
    CompilerIfStatement) data=(parser::if_statement::IfStatement if_statement compiler) ;;
    CompilerBreakStatement) data=(parser::break_statement::BreakStatement break_statement compiler) ;;
    CompilerContinueStatement) data=(parser::continue_statement::ContinueStatement continue_statement compiler) ;;
    CompilerBreakableStatement) data=(parser::statements_and_declarations::BreakableStatement breakable_statement compiler) ;;
    CompilerBreakableStatement-compile) data=(parser::statements_and_declarations::BreakableStatement::compile breakable_statement::compile compiler) ;;
    CompilerBreakableStatement-labelled_compile) data=(parser::statements_and_declarations::BreakableStatement::labelled_compile breakable_statement::labelled_compile compiler) ;;
    CompilerIterationStatement) data=(parser::iteration_statements::IterationStatement iteration_statement compiler) ;;
    CompilerDoWhileStatement) data=(parser::iteration_statements::DoWhileStatement do_while_statement compiler) ;;
    CompilerSwitchStatement) data=(parser::switch_statement::SwitchStatement switch_statement compiler) ;;
    CompilerLabelledStatement) data=(parser::labelled_statements::LabelledStatement labelled_statement compiler) ;;
    CompilerLabelledItem) data=(parser::labelled_statements::LabelledItem labelled_item compiler) ;;
    CompilerFunctionDeclaration) data=(parser::function_definitions::FunctionDeclaration function_declaration compiler) ;;
    RefResult) data=($name ref_result compiler) ;;
    AbruptResult) data=($name abrupt_result compiler) ;;
    AlwaysAbruptResult) data=($name always_abrupt_result compiler) ;;
    AlwaysRefResult) data=($name always_ref_result compiler) ;;
    AlwaysAbruptRefResult) data=($name always_abrupt_ref_result compiler) ;;
    NeverAbruptRefResult) data=($name never_abrupt_ref_result compiler) ;;
    CompilerFcnDef) data=(agent::FcnDef fcn_def compiler) ;;
    NameableProduction) data=($name nameable_production compiler) ;;
    CompilerBindingIdentifier) data=(parser::identifiers::BindingIdentifier binding_identifier compiler) ;;
    CompilerBindingElement) data=(parser::declarations_and_variables::BindingElement binding_element compiler) ;;
    CompilerBindingPattern) data=(parser::declarations_and_variables::BindingPattern binding_pattern compiler) ;;
    CompilerReturnStatement) data=(parser::return_statement::ReturnStatement return_statement compiler) ;;
    CompilerFunctionExpression) data=(parser::function_definitions::FunctionExpression function_expression compiler) ;;
    CompilerFunctionExpression_instantiate_ordinary_function_expression) data=(parser::function_definitions::FunctionExpression::instantiate_ordinary_function_expression function_expression::instantiate_ordinary_function_expression compiler) ;;
    compile_fdi) data=($name $name compiler) ;;
    CompilerArrowFunction) data=(parser::arrow_function_definitions::ArrowFunction arrow_function compiler) ;;
    CompilerArrowFunction_instantiate_arrow_function_expression) data=(parser::arrow_function_definitions::ArrowFunction::instantiate_arrow_function_expression arrow_function::instantiate_arrow_function_expression compiler) ;;
    CompilerConciseBody) data=(parser::arrow_function_definitions::ConciseBody concise_body compiler) ;;
    CompilerExpressionBody) data=(parser::arrow_function_definitions::ExpressionBody expression_body compiler) ;;
    CompilerParamSource) data=(function_object::ParamSource param_source compiler) ;;
    CompilerFormalParameters) data=(parser::parameter_lists::FormalParameters formal_parameters compiler) ;;
    CompilerArrowParameters) data=(parser::arrow_function_definitions::ArrowParameters arrow_parameters compiler) ;;
    CompilerArrowFormalParameters) data=(parser::arrow_function_definitions::ArrowFormalParameters arrow_formal_parameters compiler) ;;
    CompilerUniqueFormalParameters) data=(parser::parameter_lists::UniqueFormalParameters unique_formal_parameters compiler) ;;
    compile_initialize_bound_name) data=($name $name compiler) ;;
    CompilerFormalParameterList) data=(parser::parameter_lists::FormalParameterList formal_parameter_list compiler) ;;
    CompilerFormalParameter) data=(parser::parameter_lists::FormalParameter formal_parameter:: compiler) ;;
    CompilerSingleNameBinding) data=(parser::declarations_and_variables::SingleNameBinding single_name_binding compiler) ;;
    CompilerFunctionRestParameter) data=(parser::parameter_lists::FunctionRestParameter function_rest_parameter compiler) ;;
    CompilerFunctionBody) data=(parser::function_definitions::FunctionBody function_body compiler) ;;
    CompilerFunctionStatementList) data=(parser::function_definitions::FunctionStatementList function_statement_list compiler) ;;

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
    IfStatement-expression) data=(IfStatement::expression if_statement::expression parser::if_statement) ;;
    IfStatement-first_statement) data=(IfStatement::first_statement if_statement::first_statement parser::if_statement) ;;
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
    DeclPart) data=($name decl_part parser::statements_and_declarations) ;;
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
    Agent_global_symbol_registry) data=(Agent::global_symbol_registry agent::global_symbol_registry agent) ;;
    Agent_new) data=(Agent::new agent::new agent) ;;
    Agent_typeof_operator) data=(Agent::typeof_operator agent::typeof_operator agent) ;;
    Agent_current_realm_record) data=(Agent::current_realm_record agent::current_realm_record agent) ;;
    Agent_delete_ref) data=(Agent::delete_ref agent::delete_ref agent) ;;
    Agent_next_object_id) data=(Agent::next_object_id agent::next_object_id agent) ;;
    Agent_push_execution_context) data=(Agent::push_execution_context agent::push_execution_context agent) ;;
    Agent_apply_string_or_numeric_binary_operator) data=(Agent::apply_string_or_numeric_binary_operator agent::apply_string_or_numeric_binary_operator agent) ;;
    Agent_initialize_host_defined_realm) data=(Agent::initialize_host_defined_realm agent::initialize_host_defined_realm agent) ;;
    Agent_next_symbol_id) data=(Agent::next_symbol_id agent::next_symbol_id agent) ;;
    Agent_prepare_for_execution) data=(Agent::prepare_for_execution agent::prepare_for_execution agent) ;;
    Agent_prefix_decrement) data=(Agent::prefix_decrement agent::prefix_decrement agent) ;;
    Agent_void_operator) data=(Agent::void_operator agent::void_operator agent) ;;
    Agent_intrinsic) data=(Agent::intrinsic agent::intrinsic agent) ;;
    Agent_active_function_object) data=(Agent::active_function_object agent::active_function_object agent) ;;
    Agent_set_default_global_bindings) data=(Agent::set_default_global_bindings agent::set_default_global_bindings agent) ;;
    Agent_binary_operation) data=(Agent::binary_operation agent::binary_operation agent) ;;
    Agent_evaluate_call) data=(Agent::evaluate_call agent::evaluate_call agent) ;;
    Agent_evaluate) data=(Agent::evaluate agent::evaluate agent) ;;
    Agent_execute) data=(Agent::execute agent::execute agent) ;;
    Agent_prefix_increment) data=(Agent::prefix_increment agent::prefix_increment agent) ;;
    Agent_wks) data=(Agent::wks agent::wks agent) ;;
    Agent_current_lexical_environment) data=(Agent::current_lexical_environment agent::current_lexical_environment agent) ;;
    Agent_set_realm_global_object) data=(Agent::set_realm_global_object agent::set_realm_global_object agent) ;;
    Agent_pop_execution_context) data=(Agent::pop_execution_context agent::pop_execution_context agent) ;;
    Agent_two_values) data=(Agent::two_values agent::two_values agent) ;;
    Agent_is_less_than) data=(Agent::is_less_than agent::is_less_than agent) ;;
    Agent_instanceof_operator) data=(Agent::instanceof_operator agent::instanceof_operator agent) ;;
    Agent_create_unmapped_arguments_object) data=(Agent::create_unmapped_arguments_object agent::create_unmapped_arguments_object agent) ;;
    Agent_create_mapped_arguments_object) data=(Agent::create_mapped_arguments_object agent::create_mapped_arguments_object agent) ;;
    Agent_attach_mapped_arg) data=(Agent::attach_mapped_arg agent::attach_mapped_arg agent) ;;
    WellKnownSymbols) data=($name well_known_symbols agent) ;;
    parse_script) data=($name $name agent) ;;
    TopLevelLexDecl) data=($name top_level_lex_decl agent) ;;
    FcnDef) data=($name fcn_def agent) ;;
    TopLevelVarDecl) data=($name top_level_var_decl agent) ;;
    global_declaration_instantiation) data=($name $name agent) ;;
    script_evaluation) data=($name $name agent) ;;
    ProcessError) data=($name process_error agent) ;;
    process_ecmascript) data=($name $name agent) ;;
    bigint_leftshift) data=($name $name agent) ;;
    bigint_rightshift) data=($name $name agent) ;;

    Removability) data=($name removability environment_record) ;;
    Strictness) data=($name strictness environment_record) ;;
    Mutability) data=($name mutability environment_record) ;;
    Binding) data=($name binding:: environment_record) ;;
    DeclarativeEnvironmentRecord) data=($name declarative_environment_record environment_record) ;;
    ObjectEnvironmentRecord) data=($name object_environment_record environment_record) ;;
    BindingStatus) data=($name binding_status environment_record) ;;
    FunctionEnvironmentRecord) data=($name function_environment_record environment_record) ;;
    GlobalEnvironmentRecord) data=($name global_environment_record environment_record) ;;
    get_identifier_reference) data=($name $name environment_record) ;;
    PrivateEnvironmentRecord) data=($name private_environment_record environment_record) ;;

    ordinary_has_instance) data=(agent::Agent::ordinary_has_instance ordinary_has_instance object) ;;
    PropertyDescriptor) data=($name property_descriptor object) ;;
    InternalSlotName) data=($name internal_slot_name object) ;;
    make_basic_object) data=($name $name object) ;;

    Chunk) data=($name chunk chunk) ;;
    Chunk_add_to_func_stash) data=(chunk::Chunk::add_to_func_stash chunk::add_to_func_stash chunk) ;;
    StashedFunctionData) data=($name stashed_function_data chunk) ;;

    ParameterMap) data=($name parameter_map arguments_object) ;;
    ArgumentsObject) data=($name arguments_object arguments_object) ;;
    ArgumentsObject_get_own_property) data=(ArgumentsObject@object::ObjectInterface::get_own_property arguments_object::get_own_property arguments_object) ;;
    ArgumentsObject_define_own_property) data=(ArgumentsObject@object::ObjectInterface::define_own_property arguments_object::define_own_property arguments_object) ;;
    ArgumentsObject_set) data=(ArgumentsObject@object::ObjectInterface::set arguments_object::set arguments_object) ;;
    ArgumentsObject_get) data=(ArgumentsObject@object::ObjectInterface::get arguments_object::get:: arguments_object) ;;
    ArgumentsObject_delete) data=(ArgumentsObject@object::ObjectInterface::delete arguments_object::delete arguments_object) ;;

    IntrinsicId) data=($name intrinsic_id realm) ;;
    Intrinsics) data=($name intrinsics realm) ;;

    FunctionDeclaration_instantiate_function_object) data=(parser::function_definitions::FunctionDeclaration::instantiate_function_object function_declaration::instantiate_function_object function_object) ;;
    ThisLexicality) data=($name this_lexicality function_object) ;;
    ConstructorKind) data=($name constructor_kind function_object) ;;

    ScriptRecord) data=($name script_record execution_context) ;;
    ScriptOrModule) data=($name script_or_module execution_context) ;;
    ExecutionContext) data=($name execution_context execution_context) ;;

    *) echo "No type called $name"; exit ;;
  esac

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
