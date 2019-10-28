# Agile C

A Type Inference Transpiler for C Programming Language

## Introduction

C programming language is an explicitly typed language, which means every type declaration must be spell out explicitly. This process is cumbersome and error-prone especially when complex types like pointers, structures, and arrays are involved.

Intuitively, in most cases, the compiler should be able to obtain enough information from the context to determine the concrete type of a specific variable. [Melo et al.](https://dl.acm.org/citation.cfm?id=3158117) proposes a type inference engine for C that can infer concrete type for type declarations that do not have actual definitions. However, this engine only works when all type declarations are in place, which makes this engine a great fit for static code analysis for incomplete code segments but not so useful otherwise.

Agile C is a type inference transpiler that will first parse and analyze a program and insert dummy type parameters for any missing declarations. Then it will use type inference algorithms to further determine concrete types of these parameters. Finally, the transpiler will output a new file which can be directly fed into a normal C compiler.

The following 3 code segments show how a C function is transformed after each step.

``` C
// An expample input function
func(a) {
    b = 1.1;
    return a + b;
}
```

``` C
// The function after parsing
T func(T a) {
    T b = 1.1;
    return a + b;
}
```

``` C
// The output function
float func(float a) {
    float b = 1.1;
    return a + b;
}
```

## Quick Start

Agile C is written in pure [Rust](https://www.rust-lang.org/), and is distributed as a single binary named `agile_c.exe` (in the root directory). An example command is shown below.

```
.\agile_c.exe --input .\input.c --output .\output.c
```

That's all. Pretty simple. Following sections provide more in-depth explanations of how Agile C works.
- [Architecture](#Architecture) shows the module structure of Agile C and the general workflow.
- [Parsing](#Parsing) describes the general idea about how Agile C parses a program and inserts dummy type parameters.
- [Type Inference](#Type%20Inference) provides a detailed explanation about how Agile C perform type inference.
- [TODO](#TODO) lists features and bugs that I am currently working on.
- [Grammar Summary](#Grammar%20Summary) lists currently supported C grammer in BNF.

## Architecture

Agile C mainly consists of 5 components listed as below:

| Module | Source | Functionality |
| --- | --- | --- |
| Structure | src/structure.rs | Define all data structures including `Token`, `Expression`, etc. |
| Lexer | src/lexer.rs| Transform the source file into a token stream. |
| Parser | src/parser.rs| Parse the token stream into AST. Add dummy type parameters if necessary. |
| Resolver | src/resolver.rs| Resolve dummy type parameters to concrete types. Also perform basic type checking. |
| Serializer | src/serializer.rs| Serialize the AST into a string. |

## Parsing

Agile C uses a Pratt parser (operator-precedence parser), which associates semantics with tokens instead of grammer rules, with small tweaks on how it parses potentially missing type specifiers.

Now agile C can complete missing type specifiers in three places: the return type of a function, types for function parameters, and types for variable definitions.

For functions' return types and parameters, generally the parser knows there should be types here, and it will first try to parse any type specifiers. If no type specifier tokens are encountered, the parser will insert a dummy type specifier.

For variable definitions, the parser will maintain a environment object for each scope, which is a hash set containing name of variables defined in the current scope. When the parser produces a assignment expression, it will search those environment objects, and if the variable being assigned is not defined yet, the parser will transform this assignment expression into a definition statement with a dummy type specifier `T`.

## Type Inference

Type inference is in essential an automatic detection of the data type of expressions and variables given some context information including literals whose types are trivially known, variables whose types are known, and constraints imposed by language semantics.

To record and utilize known type information, Agile C implements a structure called  `SymbolTable` whose definition is given below.

``` rust
struct SymbolTable<'a> {
    functions: HashMap<&'a str, (Type, IndexMap<&'a str, Type>)>,
    tables: Vec<HashMap<&'a str, Type>>,
    current_func: Option<&'a str>,
}
```

`SymbolTable` has three fields. `functions` maps a function name to a 2-tuple containing the return type and an `IndexMap` (a hashmap which preserves the insertion order) mapping parameter names to their types. `tables`
is a `vector` containing `HashMap`s which map local variables to their types in each scope. When the resolver enters a new scope, it will push a new `HashMap` into `tables`, and when it leaves the current scope, it will pop a `HashMap` from `tables`. `current_func` contains the name of the function the resolver is currently looking into.

The general algorithm of resolver is stated below in pseudo code where `generic_ast` is the output of the parser and `ast` is the output of the resolver. Notice we first load all function definitions regardless of their actual definition order so we have maximum information to perform type inference.

```
input: generic_ast (a vector of functions)
output: ast (a vector of functions with all dummy types resolved)

for each func in generic_ast do
    store func's return type and parameters into SymbolTable.functions
for each func in generic_ast do
    enter a new scope
    set SymbolTable.current_func
    load parameters into the current scope
    resolve func's body statement (more later)
    update func's parameter types in SymbolTable.functions from local parameters
    leave the current scope
    clear SymbolTable.current_func
for each func in generic_ast do
    update func's return type and parameter types from SymbolTable.functions.
return generic_ast
```

When resolving statements, we are actaully resolving expressions in them, and there are two statements where resolved expression types will be associated with some other type or variables. The first one is the `Return` statement where the type of the expression (`void` if no expression) will be associated with the function's return type. The second one is the `Def` statement where types of initializers will be compared with the leading type declaration and then associated with variables being defined.

Conceptually, we can think this type unification process as trying to assign a value with `type_right` to a variable with `type_left`. What we need to do now is to determine whether this assignment is legal, and if these two types are not fully specified, whether we can make the assignment legal by further specifing their types. The resolver use a set of rules listed below to perform this task, where `T` is the dummy type parameter, `M` stands for some concrete type that we do not care what it really is, and `-` means the same type as its original type. All other type combinations will cause the unification to fail.

| `type_left` | `type_right` | Inferred `type_left` | Inferred `type_right` |
| --- | --- | --- | --- |
| `T` | `M` | `M` | `M` |
| `M` | `T` | `M` | `M` |
| `M` | `M` | `M` | `M` |
| `short` | `char` or `short` | `-` | `-` |
| `unsigned short` | `char` or ` unsigned short` | `-` | `-` |
| `int` | `char` or `short` or `unsigned short` or `int` | `-` | `-` |
| `unsigned int` | `char` or `short` or ` unsigned short` or `unsigned int` | `-` | `-` |
| `long` | `char` or `short` or `unsigned short` or `int` or `unsigned int` or `long` | `-` | `-` |
| `unsigned long` | `char` or `short` or ` unsigned short` or `int` or `unsigned int` or `unsigned long` | `-` | `-` |
| `float` | `char` or `short` or `unsigned short` or `int` or `unsigned int` or `long` or `unsigned long` or `float` | `-` | `-` |
| `double` | `char` or `short` or `unsigned short` or `int` or `unsigned int` or `long` or `unsigned long` or `float` or `double` | `-` | `-` |

Then we need to resolve expressions to obtain `type_right`. The resolver will first recursively destructure an expression back to five primary expressions: `Ident`, `IntConst`, `FloatConst`, `CharConst`, and `StrConst`.

For `Ident`, the resolver will retrieve its type from `SymbolTable` and if its type is `T`, it will be marked as a `generic_ident`. Each time the resolver successfully performs a type unification, it will update type information for all `generic_ident` involved. For `IntConst`, the resolver will pick the "narrowest" integer type that the literal fits. All interger types we support now are listed below from the narrowest (top) to the widest (bottom). For `FloatConst`, the same process applies and available types are also given below. Then `CharConst` will be assigned type `char` and `StrConst` will be assigned type `T` because currently we do not have pointer types.

```
Integer Types:
    short
    unsigned short
    int
    unsigned int
    long
    unsigned long

Floating Type:
    float
    double
```

After that these primary expressions will be combined into all other expressions by various operators. It will take too much space to list all specific rules, only five general rules are listed below. Notice "M is compatible with N" here means a type unification using N as its `type_left` and M as its `type_right` will succeed.

- Position where a number is required should have a type that is compatible with `double`.
- Position where a bool is required should have a type that is compatible with `int`.
- Indexes to arrays are required to be compatible with `long` or `unsigned long`.
- Aruguments in functions calls are required to be compatible with corresponding parameters in function definitions.
- All expressions in a array initializer list should be compatible with a single type, which will be the type of the array.

## TODO

- Parsing of empty statements.
- Type inference between function arguments and parameters.
- More powerful type constrains so that we can perform a chain of inference instead of just one-step inference.
- Parsing and type inference for the pointer type.
- Parsing and type inference for the structure type.
- Cross-file parsing and type inference (`#include` directive).

## Grammar Summary

Grammar are excerpted from [C18](https://www.iso.org/standard/74528.html) and rewritten in BNF.

```
<non-digit> ::= "A" | "B" | "C" | "D" | "E" | "F" | "G"
                | "H" | "I" | "J" | "K" | "L" | "M" | "N"
                | "O" | "P" | "Q" | "R" | "S" | "T"
                | "U" | "V" | "W" | "X" | "Y" | "Z"
                | "a" | "b" | "c" | "d" | "e" | "f" | "g"
                | "h" | "i" | "j" | "k" | "l" | "m" | "n"
                | "o" | "p" | "q" | "r" | "s" | "t"
                | "u" | "v" | "w" | "x" | "y" | "z"
                | "_" 
<digit> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
<escape> ::= "\n" | "\"" | "\'"
<type-specifier> ::= "T" | "void" | "char" | "short" | "int" | "long"
                    | "float" | "double" | "signed" | "unsigned"


<identifier> ::= <non-digit> (<digit> | <non-digit>)*
<int-constant> ::= ["+" | "-"] <digit>+
<floating-constant> ::= ["+" | "-"] <digit>+ "." <digit>+
<char-constant> :: "'" (<non-digit> | <digit> | " " | <escape>) "'" 
<string-constant> ::= "\"" <char-constant>* "\""


<declaration> ::= <type-specifier>* <init-declarator-list>
<init-declarator-list> ::= <init-declarator>
                        | <init-declarator-list> "," <init-declarator>
<init-declarator> ::= <identifier> ["=" (<assignment-expression> | <initializer-list>)]
<initializer-list> ::= "{" [<assignment-expression> ("," <assignment-expression>)*] "}"


<function-definition> ::= <type-specifier>* <identifier> "(" <parameter-list> ")" <compound-statement>
<parameter-list> ::= [<type-specifier>* <identifier>]
                        | <parameter-list> "," [<type-specifier>* <identifier>]


<primary-expression> ::= <identifier> | <int-constant> | <floating-constant>
                        | <char-constant> | <string-constant>
<suffix-expression> ::= <primary-expression>
                        | <suffix-expression> "[" <expression> "]"
                        | <suffix-expression> "(" <argument-list> ")"
                        | <suffix-expression> ["++" | "--"]
<argument-list> ::= <constant-expression>
                        | <argument-list> "," <constant-expression>
<prefix-expression> ::= <suffix-expression>
                        | ["++" | "--" | "!"] <suffix-expression>
<multiplicative-expression> ::= <prefix-expression>
                        | <multiplicative-expression> "*" <prefix-expression>
                        | <multiplicative-expression> "/" <prefix-expression>
                        | <multiplicative-expression> "%" <prefix-expression>
<additive-expression> ::= <multiplicative-expression>
                        | <additive-expression> "+" <multiplicative-expression>
                        | <additive-expression> "-" <multiplicative-expression>
<relational-expression> ::= <additive-expression>
                        | <relational-expression> "<" <additive-expression>
                        | <relational-expression> ">" <additive-expression>
                        | <relational-expression> "<=" <additive-expression>
                        | <relational-expression> ">=" <additive-expression>
<equality-expression> ::= <relational-expression>
                        | <equality-expression> "==" <relational-expression>
                        | <equality-expression> "!=" <relational-expression>
<logical-AND-expression> ::= <equality-expression>
                        | <logical-AND-expression> "&&" <equality-expression>
<logical-OR-expression> ::= <logical-AND-expression>
                        | <logical-OR-expression> "||" <logical-AND-expression>
<constant-expression> ::= <logical-OR-expression>
<assignment-expression> ::= <identifier> <assignment-operator> <logical-OR-expression>
<assignment-operator> ::= "=" | "+=" | "-=" | "*=" | "/=" | "%="


<statement> ::= <labeled-statement>
                | <compound-statement>
                | <expression-statement>
                | <selection-statement>
                | <iteration-statement>
                | <jump-statement>
<labeled-statement> ::= "case" <constant-expression> ":" <statement>
                        | "default" ":" <statement>
<compound-statement> ::= "{" <block-item-list> "}"
<block-item-list> ::= <block-item>
                        | <block-item-list> <block-item>
<block-item> ::= <declaration> | <statement>
<expression-statement> ::= <expression> ";"
<selection-statement> ::= "if" "(" <expression> ")" <statement> ["else" <statement>]
                        | "switch" "(" <expression> ")" <statement>
<iteration-statement> ::= "while" "(" <expression> ")" <statement>
                        | "do" <statement> "while" "(" <expression> ")" ";"
                        | "for" "(" [<expression>] ";" [<expression>] ";" [<expression>] ")" <statement>
<jump-statement> ::= "continue" ";" | "break" ";" | "return" <expression> ";" 
```
