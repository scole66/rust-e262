# Current PR Tests

Note: This file should be removed prior to merge. It should exist _only_ on the feature branch.

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
* `parse_script`
* `FcnDef::instantiate_function_object`
* `global_declaration_instantiation`
* `script_evaluation`

### `src/chunk/mod.rs`

* `StashedFunctionData`
* `Chunk`
* `Chunk::add_to_func_stash`
* `Chunk::disassemble`

### `src/parser/generator_function_definitions/mod.rs`

* `GeneratorBody::lexically_scoped_declarations`

### `src/parser/function_definitions/mod.rs`

* `FunctionBody::lexically_scoped_declarations`
* `FunctionStatementList::lexically_scoped_declarations`

### `src/parser/async_generator_function_definitions/mod.rs`

* `AsyncGeneratorBody::lexically_declared_names`
* `AsyncGeneratorBody::lexically_scoped_declarations`

### `src/parser/async_function_definitions/mod.rs`

* `AsyncFunctionBody::lexically_scoped_declarations`

### `src/parser/arrow_function_definitions/mod.rs`

* `ConciseBody::lexically_scoped_declarations`

### `src/object/mod.rs`

* `InternalSlotName`
* `make_basic_object`
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
* `FunctionDeclaration::instantiate_ordinary_function_object`
* `GeneratorDeclaration::instantiate_ordinary_function_object`
* `AsyncFunctionDeclaration::instantiate_ordinary_function_object`
* `AsyncGeneratorDeclaration::instantiate_ordinary_function_object`
* `ordinary_function_create`
* `make_constructor`

### Files to Scan

* `src/compiler/mod.rs`
* `src/environment_record/mod.rs`
* `src/execution_context/mod.rs`