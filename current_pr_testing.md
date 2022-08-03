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

```bash
./t CompilerPrimaryExpression parse_script FunctionDeclaration_instantiate_function_object GeneratorBody \
    Insn BindingStatus ConciseBody InternalSlotName make_basic_object Chunk Chunk_add_to_func_stash \
    NameableProduction StashedFunctionData FunctionBody FunctionStatementList CompilerBindingIdentifier \
    CompilerBindingElement CompilerAssignmentExpression CompilerFcnDef CompilerBindingPattern \
    CompilerFunctionDeclaration CompilerPropertyDefinition CompilerLexicalBinding CompilerBlock \
    CompilerVariableDeclaration CompilerStatement CompilerDeclaration CompilerReturnStatement \
    CompilerLabelledItem AssignmentExpression FcnDef AsyncFunctionBody AsyncGeneratorBody LabelledItem \
    MemberExpression NewExpression LeftHandSideExpression PrimaryExpression Initializer
```

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

### `src/object/mod.rs`

* `call`
* `initiate_call`
* `construct`
* `to_constructor`

### `src/function_object/mod.rs`

* `ConstructorKind`
* `ThisLexicality`
* `ClassName`
* `ClassFieldDefinitionRecord`
* `BodySource`
* `ParamSource`
* `FunctionSource`
* `FunctionObjectData`
* `FunctionObject::call`
* `FunctionObject::construct`
* `FunctionObject::object`
* `Agent::prepare_for_ordinary_call`
* `Agent::ordinary_call_bind_this`
* `Agent::ordinary_call_evaluate_body`
* `namify`
* `BuiltInFunctionObject`
* `GeneratorDeclaration::instantiate_function_object`
* `AsyncFunctionDeclaration::instantiate_function_object`
* `AsyncGeneratorDeclaration::instantiate_function_object`
* `ordinary_function_create`
* `make_constructor`

### `src/execution_context/mod.rs`

* `ScriptRecord`
* `ScriptOrModule`
* `ExecutionContext`

### `src/environment_record/mod.rs`

* `FunctionEnvironmentRecord`
* `FunctionEnvironmentRecord::bind_this_value`

### `src/compiler/mod.rs`

* `FunctionExpression`
* `compile_fdi`
* `ArrowFunction`
* `ConciseBody`
* `ExpressionBody`
* `ParamSource`
* `FormalParameters`
* `ArrowParameters`
* `ArrowFormalParameters`
* `UniqueFormalParameters`
* `compile_initialize_bound_name`
* `FormalParameterList`
* `FormalParameter`
* `SingleNameBinding`
* `FunctionRestParameter`
* `FunctionBody`
* `FunctionStatementList`

### Existing Test Framework Coverage Misses

Uncovered Items:

* SymbolObject
* SymbolRegistry
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
* ObjectEnvironmentRecord
* FunctionEnvironmentRecord
* GlobalEnvironmentRecord
