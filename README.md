# Agile C

A Type Inference Transpiler for C Programming Language

## Introduction

C programming language is an explicitly typed language, which means every type declaration must be spell out explicitly. This process is cumbersome and error-prone especially when complex types like pointers, structures, and arrays are involved.

Intuitively, in most cases, the compiler should be able to obtain enough information from the context to determine the concrete type of a specific variable. [Melo et al.](https://dl.acm.org/citation.cfm?id=3158117) proposes a type inference engine for C that can infer concrete type for type declarations that do not have actual definitions. However, this engine only works when all type declarations are in place, which makes this engine a great fit for static code analysis for code segments but not so useful otherwise.

Agile C is a type inference transpiler that will first parse and analyze a program and insert dummy type parameters for any missing declarations. Then it will use type inference algorithms to further determine concrete types of these parameters. Finally, the transpiler will output a new file which can be directly fed into a normal C compiler.

The following 3 code segments show how a C function is transformed after each step.

``` C
// An expample input function
func(a) {
    b = 1.0;
    return a + b;
}
```

``` C
// The function after parsing
T func(T a) {
    T b = 1.0;
    return a + b;
}
```

``` C
// The output function
float func(float a) {
    float b = 1.0;
    return a + b;
}
```

## Quick Start

Agile C is written in pure [Rust](https://www.rust-lang.org/), and is distributed as a single binary named `agile_c.exe`. The program takes 2 positional arguments. The first on is the path to the input C file, and the second one is the path to the output C file. An example command is shown below.

```
.\agile_c.exe ..\test_c\input.c ..\test_c\output.c
```

That's all. Pretty simple. Following sections provide more in-depth explanations about how Agile C works.
- [Architecture](#Architecture) shows the structure of Agile C.
- [Parsing](#Parsing) provides a detailed explanation about how Agile C parses a program and inserts appropiate type information.
- [Type Inference](#Type%20Inference) provides a detailed explanation about how Agile C infer concrete types.
- [TODO](#TODO) lists features that I want to add to Agile C but have not been implemented. 
- [Grammar Summary](#Grammar%20Summary) lists currently supported C grammer in BNF.

## Architecture

Agile C mainly consists of 5 components listed as below:

| Module | Source | Functionality |
| --- | --- | --- |
| Structure | src/structure.rs | Define all data structures including `Token`, `Expression`, etc. |
| Lexing | src/lexing.rs | Transform the source file into a token stream. |
| Parser | src/parser.rs | Parsing the token stream into AST. Add placeholder type parameters if necessary. |
| Type Resolver | src/type_resolver.rs | Resolve placeholder type parameters to concrete types. |
| Serializer | src/serializer.rs | Serialize the AST into a string. |

## Parsing

Agile C uses a Pratt parser (operator-precedence parser), which associates semantics with tokens instead of grammer rules, with small tweaks on how it parse type specifiers. Specically, now agile C can complete missing type specifiers in three places: the return type of a function, types for function parameters, and types for variable definitions.

For functions' return types and parameters, the parser will first try to parse any type specifiers. If no type specifier tokens are encountered, the parser will insert a placeholder type specifier `T`.

For variable definitions, the parser will maintain a environment object for each scope, which is a hash set containing name of visible variables in the current scope and a pointer to the outer encompassing environment object. When the parser produces a assignment expression, it will search those environment objects. If the varaible being assigned is not defined yet, the parser will transform this assignment expression into a definition statement with a placeholder type specifier `T`.

## Type Inference

*To be completed*

![rules](.\rules.png)

## TODO

- Parsing of full variable initialization syntax.
- Parsing and type inference for pointer type.
- Parsing and type inference for structure type.
- Cross-file parsing and type inference with `#include`.

## Grammar Summary

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
<floating-constant> ::= ["+" | "-"] <int-constant> "." <int-constant>
<char-constant> :: "'" (<non-digit> | <digit> | " " | <escape>) "'" 
<string-constant> ::= "\"" <char-constant>* "\""


<declaration> ::= [<type-specifier>+] <init-declarator-list>
<init-declarator-list> ::= <init-declarator>
                        | <init-declarator-list> "," <init-declarator>
<init-declarator> ::= <identifier>
                        | <identifier> "=" <assignment-expression>


<function-definition> ::= [<type-specifier>] <identifier> "(" <parameter-list> ")" <compound-statement>
<parameter-list> ::= [[<type-specifier>] <identifier>]
                        | <parameter-list> "," [<type-specifier>] <identifier>


<primary-expression> ::= <identifier> | <int-constant> | <floating-constant>
                        | <char-constant> | <string-constant>
<suffix-expression> ::= <primary-expression>
                        | <suffix-expression> "[" <expression> "]"
                        | <suffix-expression> "(" <argument-list> ")"
                        | <suffix-expression> ["++" | "--"]
<argument-list> ::= <assignment-expression>
                        | <argument-list> "," <assignment-expression>
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
<assignment-expression> ::= <identifier> <assignment-operator> <logical-OR-expression>
<assignment-operator> ::= "=" | "+=" | "-=" | "*=" | "/=" | "%="
<expression> ::= <assignment-expression> ["," <assignment-expression>]*


<statement> ::= <labeled-statement>
                | <compound-statement>
                | <expression-statement>
                | <selection-statement>
                | <iteration-statement>
                | <jump-statement>
<labeled-statement> ::= "case" <logical-OR-expression> ":" <statement>
                        | "default" ":" <statement>
<compound-statement> ::= "{" <block-item-list> "}"
<block-item-list> ::= <block-item>
                        | <block-item-list> <block-item>
<block-item> ::= <declaration> | <statement>
<expression-statement> ::= <expression> ";"
<selection-statement> ::= "if" "(" <expression> ")" <statement> ["else" <statement>]
                        | "switch" "(" <expression> ")" <labeled-statement>
<iteration-statement> ::= "while" "(" <expression> ")" <statement>
                        | "do" <statement> "while" "(" <expression> ")" ";"
                        | "for" "(" [<expression>] ";" [<expression>] ";" [<expression>] ")" <statement>
<jump-statement> ::= "continue" ";" | "break" ";" | "return" <expression> ";" 
```
