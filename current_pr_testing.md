# Current PR Tests

Note: This file should be removed prior to merge. It should exist _only_ on the feature branch.

## Done

* `./t CompilerPrimaryExpression`
* `./t parse_script`
* `./t FunctionDeclaration_instantiate_function_object`
* `./t GeneratorBody`
* `./t Insn`
* `./t BindingStatus`
* `./t ConciseBody`
* `./t InternalSlotName`
* `./t make_basic_object`
* `./t Chunk`
* `./t Chunk_add_to_func_stash`
* `./t NameableProduction`
* `./t StashedFunctionData`
* `./t FunctionBody`
* `./t FunctionStatementList`
* `./t CompilerBindingIdentifier`
* `./t CompilerBindingElement`
* `./t CompilerAssignmentExpression`
* `./t CompilerFcnDef`
* `./t CompilerBindingPattern`
* `./t CompilerFunctionDeclaration`
* `./t CompilerPropertyDefinition`
* `./t CompilerLexicalBinding`
* `./t CompilerBlock`
* `./t CompilerVariableDeclaration`
* `./t CompilerStatement`
* `./t CompilerDeclaration`
* `./t CompilerReturnStatement`
* `./t CompilerLabelledItem`
* `./t AssignmentExpression`
* `./t FcnDef`
* `./t AsyncFunctionBody`
* `./t AsyncGeneratorBody`
* `./t LabelledItem`
* `./t MemberExpression`
* `./t NewExpression`
* `./t LeftHandSideExpression`
* `./t PrimaryExpression`
* `./t Initializer`
* `./t ObjectEnvironmentRecord`
* `./t CompilerFunctionExpression`
* `./t CompilerFunctionExpression_instantiate_ordinary_function_expression`
* `./t GlobalEnvironmentRecord`
* `./t compile_fdi`
* `./t CompilerArrowFunction`
* `./t CompilerArrowFunction_instantiate_arrow_function_expression`
* `./t CompilerConciseBody`
* `./t CompilerExpressionBody`
* `./t CompilerParamSource`
* `./t CompilerFormalParameters`
* `./t CompilerArrowParameters`
* `./t CompilerArrowFormalParameters`
* `./t CompilerUniqueFormalParameters`
* `./t compile_initialize_bound_name`
* `./t CompilerFormalParameterList`
* `./t CompilerFormalParameter`
* `./t CompilerSingleNameBinding`
* `./t CompilerFunctionRestParameter`
* `./t CompilerFunctionBody`
* `./t CompilerFunctionStatementList`
* `./t ThisLexicality`
* `./t ConstructorKind`
* `./t ScriptRecord`
* `./t ScriptOrModule`
* `./t ExecutionContext`
* `./t SymbolObject`
* `./t SymbolRegistry`
* `./t FunctionEnvironmentRecord_new`
* `./t FunctionEnvironmentRecord_name`
* `./t FunctionEnvironmentRecord_create_immutable_binding`
* `FunctionObject_GeneratorDeclaration_instantiate_function_object`
* `FunctionObject_AsyncFunctionDeclaration_instantiate_function_object`
* `FunctionObject_AsyncGeneratorDeclaration_instantiate_function_object`

```bash
./t CompilerPrimaryExpression parse_script FunctionDeclaration_instantiate_function_object GeneratorBody \
    Insn BindingStatus ConciseBody InternalSlotName make_basic_object Chunk Chunk_add_to_func_stash \
    NameableProduction StashedFunctionData FunctionBody FunctionStatementList CompilerBindingIdentifier \
    CompilerBindingElement CompilerAssignmentExpression CompilerFcnDef CompilerBindingPattern SymbolObject \
    CompilerFunctionDeclaration CompilerPropertyDefinition CompilerLexicalBinding CompilerBlock \
    CompilerVariableDeclaration CompilerStatement CompilerDeclaration CompilerReturnStatement \
    CompilerLabelledItem AssignmentExpression FcnDef AsyncFunctionBody AsyncGeneratorBody LabelledItem \
    MemberExpression NewExpression LeftHandSideExpression PrimaryExpression Initializer compile_fdi \
    ObjectEnvironmentRecord CompilerFunctionExpression GlobalEnvironmentRecord CompilerFunctionStatementList \
    CompilerFunctionExpression_instantiate_ordinary_function_expression CompilerConciseBody ThisLexicality \
    CompilerArrowFunction_instantiate_arrow_function_expression CompilerExpressionBody CompilerParamSource \
    CompilerFormalParameters CompilerArrowParameters CompilerArrowFormalParameters CompilerFormalParameter \
    CompilerUniqueFormalParameters compile_initialize_bound_name CompilerFormalParameterList ScriptRecord \
    CompilerSingleNameBinding CompilerFunctionRestParameter CompilerFunctionBody CompilerArrowFunction \
    ConstructorKind ScriptOrModule ExecutionContext SymbolRegistry FunctionEnvironmentRecord_new \
    FunctionEnvironmentRecord_name FunctionEnvironmentRecord_create_immutable_binding
```

## Coverage Blocks Established

### `src/environment_record/mod.rs`

* `FunctionEnvironmentRecord`
* `FunctionEnvironmentRecord_has_binding`
* `FunctionEnvironmentRecord_create_mutable_binding`
* `FunctionEnvironmentRecord_initialize_binding`
* `FunctionEnvironmentRecord_set_mutable_binding`
* `FunctionEnvironmentRecord_get_binding_value`
* `FunctionEnvironmentRecord_delete_binding`
* `FunctionEnvironmentRecord_with_base_object`
* `FunctionEnvironmentRecord_has_this_binding`
* `FunctionEnvironmentRecord_has_super_binding`
* `FunctionEnvironmentRecord_get_outer_env`
* `FunctionEnvironmentRecord_get_this_binding`
* `FunctionEnvironmentRecord_binding_names`
* `FunctionEnvironmentRecord_get_super_base`

### `src/function_object/mod.rs`

* `ordinary_function_create`
* `make_constructor`
* `ClassName`
* `ClassFieldDefinitionRecord`
* `BodySource`
* `ParamSource`
* `FunctionSource`
* `FunctionObjectData`
* `FunctionObject_call`
* `FunctionObject_construct`
* `FunctionObject_object`
* `FunctionObject_Agent_prepare_for_ordinary_call`
* `FunctionObject_Agent_ordinary_call_bind_this`
* `FunctionObject_Agent_ordinary_call_evaluate_body`
* `nameify`
* `BuiltInFunctionObject`

### `src/object/mod.rs`

* `call`
* `initiate_call`
* `construct`
* `to_constructor`

## To Check

### `src/agent/mod.rs`

* `Agent::get_active_script_or_module`
* `Agent::ec_push`
* `Agent::ec_pop`
* `Agent::current_variable_environment`
* `Agent::current_private_environment`
* `Agent::set_variable_environment`
* `Agent::evaluate`
* `Agent::prepare_running_ec_for_execution`
* `Agent::execute`
* `Agent::begin_call_evaluation`
* `Agent::instantiate_ordinary_function_expression_without_binding_id`
* `Agent::instantiate_ordinary_function_expression_with_binding_id`
* `Agent::instantiate_arrow_function_expression`
* `Agent::instantiate_ordinary_function_object`
* `FcnDef::instantiate_function_object`
* `global_declaration_instantiation`
* `script_evaluation`


### Existing Test Framework Coverage Misses

Uncovered Items:

* provision_symbol_intrinsic
* Agent
* Agent_initialize_host_defined_realm
* Agent_prefix_decrement
* Agent_intrinsic
* Agent_set_default_global_bindings
* Agent_binary_operation
* Agent_evaluate
* Agent_execute
* Agent_prefix_increment
* Agent_current_lexical_environment
* Agent_set_realm_global_object
* global_declaration_instantiation
* FunctionEnvironmentRecord
