# Agile-C

A Type Inference Transpiler for the C Programming Language

## Motivation

Type inference now is an important feature for modern programming languages. You can find it from the famous global inference in Haskell, local inference in Rust and Scala, and limited form in old language like C++ and Java. There is no doubt a smart type inference system can reduce the cognitive load of programmers while not giving up the benefits of strongly staticly type systems.

However, although the general idea of type inference, type unification, is quite straightforaward and concise, there is a large gap between the theory and a really working type inference system. What's worse, it is not easy to find something like "Write your a type inference engine in 21 days" that is accessable while practical for common programmers who have no expertise in programming language design and type systems to get a feeling about the principle of type inference.

Agile-C is my bachelar graduation project, which provides basic type inference for most commen used C features. This project is practice for me before I read more serious literature about programming language design and type systems. This porject is intended to ONLY educational purpose.

## Introduction

Agile-C is a transpiler, which consumes a C sourse file that omits certain type declarations and outputs a valid C sourse file. You can also invoke GCC from Agile-C directly and get a executable file directly.

Following sections are divided into three parts: First, we will walk you through "Getting Started" section to show you how Agile-C works in action; Then, we will talk about some basic knowledge about type systems; Finally, we will talks about the implementation details of Agile-C and also its formal syntax.

---

## Getting Started

### Installing

Agile-C is written in pure Rust and distributed as a single executable file available in the root directory of the project named "agile_c.exe".

### Usage

1. Hello, world!

In whatever folder you like, create `test.c` and type the following C code.

```C
#include <stdio.h>

int main(void) {
    printf("Hello, world!");
    return 0;
}
```

Then open the terminal and type the following command.

```
$ ./agile_c.exe --gcc -i ./test.c -o ./test.exe
```

`--gcc` tells the Agile-C to invoke GCC for you (notice you should have GCC installed and can be directly invoked from the command line with `gcc` command). `-i` or `--input` specifies the path to the input file, while `-o` or `--output` specifies the path to the output file. Now you should see a new executable file named `test.exe` appears in this folder. Now try to run it.

```
$ ./test.exe
```

Now you should find "Hello, world!" appears in your screen.

2. Invoke GCC with more arguments

You can provide more arguments to GCC by using `args` subcommand. For example, the following command will print the version information of GCC.

```
./agile_c.exe --gcc -i ./test.c -o ./test.exe args --version
```

3. Basic C features

Agile-C supports the most commonly used features of C. Change `test.c` to the following and recompile it to see what you get!

```


---

## Type Inference

---

## Module Structure

## Caution
int size
C: same definition, different type
agile_c: same name, later one will overwrite

## Grammer



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
- [Resolving](#Resolving) provides a detailed explanation about how Agile C perform type inference.
- [TODO](#TODO) lists features and bugs that I am currently working on.
- [Grammar](#Grammar) lists currently supported C grammer in BNF.

## Architecture

Agile C mainly consists of 5 components listed as below:

| Module | Source | Functionality |
| --- | --- | --- |
| Structure | src/structure.rs | Define all data structures including `Token`, `Expression`, etc. |
| Preprocessor | src/preprocessor.rs | Recursively `include` all required source files |
| Lexer | src/lexer.rs| Transform the source file into a token stream. |
| Parser | src/parser.rs| Parse the token stream into AST. Add dummy type parameters if necessary. |
| Resolver | src/resolver.rs| Resolve dummy type parameters to concrete types. Also perform basic type checking. |
| Serializer | src/serializer.rs| Serialize the AST into a string. |

## Parsing

Agile C uses a Pratt parser (operator-precedence parser), which associates semantics with tokens instead of grammer rules, with small tweaks on how it parses potentially missing type specifiers.

Now agile C can complete missing type specifiers in three places: the return type of a function, types for function parameters, and types for variable definitions.

For functions' return types and parameters, generally the parser knows there should be types here, and it will first try to parse any type specifiers. If no type specifier tokens are encountered, the parser will insert a dummy type specifier.

For variable definitions, the parser will maintain a environment object for each scope, which is a hash set containing name of variables defined in the current scope. When the parser produces a assignment expression, it will search those environment objects, and if the variable being assigned is not defined yet, the parser will transform this assignment expression into a definition statement with a dummy type specifier `T`.

## Resolving

Type inference is in essential an automatic detection of the data type of expressions and variables given some context information including literals whose types are trivially known, variables whose types are known, and constraints imposed by language semantics.

To record and utilize these known type information, Agile C implements a structure called  `SymbolTable` whose definition is given below.

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
    set SymbolTable.current_func
    enter a new scope
    load parameters into the current scope
    resolve func's body statement (more later)
    update func's parameter types in SymbolTable.functions from local parameters
    leave the current scope
    clear SymbolTable.current_func
for each func in generic_ast do
    update func's return type and parameter types from SymbolTable.functions.
return generic_ast
```

When resolving statements, we are actaully resolving expressions in them, and there are two statements where resolved expression types will be associated with some other type or variables. The first one is the `Return` statement where the type of the expression (`void` if no expression) will be associated with the function's return type. The second one is the `Def` statement where types of initializers will be associated with variables being defined.

This association process is known as "type unification", and conceptually, we can think type unification as trying to determine whether an assignment from a value with `type_right` to a variable with `type_left` is legal, and if these two types are not fully specified, whether we can make the assignment legal by further specifing their types. The resolver use a 3-step algorithm given below to determine whether a given unification is legal or not. Also notice in current implementation, every type object has an array flag and an pointer flag associated with it, so we can check whether a type (variable) is an array or a pointer.

```
input: type_left, type_right
output: (inferred_type_left, inferred_type_right) or error

if type_left == T
    return (type_right, type_right)
if type_right == T
    return (type_left, type_left)
if type_left == Void and type_right == Void
    return (type_left, type_right)
if type_left and type_right are not both arrays or pointers
    return error
if the combination of type_left and type_right is recorded in the table below
    return (type_left, type_right)
else
    return error
```

| `type_left` | `type_right` |
| --- | --- |
| `char` | `char` |
| `short` | `char` or `short` |
| `unsigned short` | `char` or ` unsigned short` |
| `int` | `char` or `short` or `unsigned short` or `int` |
| `unsigned int` | `char` or `short` or ` unsigned short` or `unsigned int` |
| `long` | `char` or `short` or `unsigned short` or `int` or `unsigned int` or `long` |
| `unsigned long` | `char` or `short` or ` unsigned short` or `int` or `unsigned int` or `unsigned long` |
| `float` | `char` or `short` or `unsigned short` or `int` or `unsigned int` or `long` or `unsigned long` or `float` |
| `double` | `char` or `short` or `unsigned short` or `int` or `unsigned int` or `long` or `unsigned long` or `float` or `double` |

Then we need to resolve expressions to obtain `type_right`. The resolver will first recursively destructure an expression back to five primary expressions: `Ident`, `IntConst`, `FloatConst`, `CharConst`, and `StrConst`.

For `Ident`, the resolver will retrieve its type from `SymbolTable` and if its type is `T`, it will be marked as a `generic_ident`. Each time the resolver successfully performs a type unification, it will update type information for all `generic_ident` involved. For `IntConst`, the resolver will pick the "narrowest" integer type that the literal fits. All interger types we support now are listed below from the narrowest (top) to the widest (bottom). For `FloatConst`, the same process applies and available types are also given below. Then `CharConst` will be assigned type `char` and `StrConst` will be assigned type `char` with its pointer flag set.

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

After that, these primary expressions will be combined into all other expressions by various operators. It will take too much space to list all specific rules, so we only list the most fundamental rules below. Notice "M is compatible with N" means a type unification process using N as its `type_left` and M as its `type_right` will succeed.

- Position where a number is required should have a type that is compatible with `double`.
- Position where a bool is required should have a type that is compatible with `int`.
- Prefix operator `*` will clear the pointer flag of whatever type the following epression has.
- Prefix operator `&` will set the pointer flag of whatever type the following epression has.
- Indexes to arrays are required to be compatible with `long` or `unsigned long`, and the type of the whole index expression is the same as the array itself with the array flag cleared.
- Aruguments in a functions call are required to be compatible with corresponding parameters in the corresponding function definition.
- All expressions in a array initializer list should be compatible with a single type, which will be the type of the array with its array flag set.

## TODO

param ...
param void

array initlist must be const

lost env when final resolve return/param types
init list for arrays/structures (no nest)

char? Num
Bool? Num + ptr -> Bool -> T > char
Array? const
Pointer? invar
Struct? invar
literal? T > Tmin

char <- unsigned short <- unsigned int <- unsigned long <- float <- double
^            ^                 ^                            |
|            |                 |                            |
└ short <- int <------------ long <-------------------------┘

## Grammar

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
<parameter-list> ::= "void" | [<type-specifier>* <identifier>]
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
                        | ["++" | "--" | "!" | "*" | "&"] <suffix-expression>
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
