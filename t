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

tst ${file}::tests::${modname}
report --name-regex="_3res${#file}${file}([^0-9][^_]+_)?${mangled}"  --uncovered  --demangled

# To Check: ECMAScriptValue, 
# Ok: ArrayIndex, SymbolInternals, Symbol, PrivateElementKind, PrivateElement, Numeric, PrivateName, PropertyKey
