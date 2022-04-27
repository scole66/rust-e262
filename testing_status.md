# Testing Status for current feature branch

Files in the commit chain:

| File path | Test Completion State | Functions to Test |
| --- | --- | --- |
| agent
| arrays
| chunk
| compiler
| environment_record
| errors
| execution_context
| function_object | | `FunctionDeclaration::instantiate_function_object`, `GeneratorDeclaration::instantiate_function_object`, `AsyncFunctionDeclaration::instantiate_function_object`, `AsyncGeneratorDeclaration::instantiate_function_object`
| object | ok
| opcodes
| parser/additive_operators
| parser/assignment_operators
| parser/binary_bitwise_operators
| parser/binary_logical_operators
| parser/bitwise_shift_operators
| parser/block
| parser/comma_operator
| parser/conditional_operator
| parser/equality_operators
| parser/exponentiation_operator
| parser/expression_statement
| parser/identifiers
| parser/left_hand_side_expressions
| parser/multiplicative_operators
| parser/primary_expressions
| parser/relational_operators
| parser/scripts
| parser/statements_and_declarations
| parser/unary_operators
| parser/update_expressions
| realm | ok
| reference | ok
| values | ok
