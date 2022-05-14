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
  CompilerPropertyDefinition) data=(parser::primary_expressions::PropertyDefinition property_definition compiler) ;;
  CompilerPropertyName) data=(parser::primary_expressions::PropertyName property_name compiler) ;;
  CompilerCallExpression) data=(parser::left_hand_side_expressions::CallExpression call_expression compiler) ;;

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
report --name-regex="_3res${#file}${file}([^0-9][^_]+_)?${mangled}"  --uncovered  --demangled
