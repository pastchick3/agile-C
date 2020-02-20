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

Agile-C is written in pure Rust and distributed as a single executable file available in the root directory of the project named "agile_c.exe".

### Hello, world!

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

### Invoke GCC with more arguments

You can provide more arguments to GCC by using `args` subcommand. For example, the following command will print the version information of GCC.

```
./agile_c.exe --gcc -i ./test.c -o ./test.exe args --version
```

### Basic C features

Agile-C supports the most commonly used features of C. Change `test.c` to the following and recompile it to see what you get!

```C
#include <stdio.h>

struct Counter {
    int num;
};

void add(struct Counter *cnt, int number) {
    cnt->num += number;
}

int main(void) {
    struct Counter cnt = { .num = 0 };
    int numbers[] = { 1, 2 };
    for (int i = 0; i < 2; ++i) {
        add(&cnt, numbers[i]);
    }
    printf("%i", cnt.num);
    return 0;
}
```

### Type Inference
Now let's omit tedious type declaration! Copy the abbreviated version below to `test.c` and rerun the program. You will find the program just runs!

```C
#include <stdio.h>

struct Counter {
    int num;
};

add(cnt, number) {
    cnt->num += number;
}

main() {
    struct Counter cnt = { .num = 0 };
    numbers = { 1, 2 };
    for (i = 0; i < 2; ++i) {
        add(&cnt, numbers[i]);
    }
    printf("%i", cnt.num);
    return 0;
}
```

---
## Features

The formal syntax, type hierarchy, and some notes related to the implementation details are given in the last [Specification](#Specification). Here we just quickly go through what Agile-C is capable of, and a few quirks you are likly to run into.

- Agile-C supports all kinds of literals, including string, character, numerical. However, you are not allow to use integer suffix (`L`).
- Most expressions and statements are supported.
- Most primary types are supported, but not specifier like `const`.
- Array type is supported, and the only valid initialization syntax is `arr = { 1 }`. The designated initializer for arrays is not supported.
- Structures are supported, but the struct definition must be complete (all members must have known types), and the declaration of struct type cannot be omitted. The only valid initialization syntax is `strcture = { .member = 1 }`, and all members must be explicitly initialized.
- Functions are supported.
- For macros, only `#include` is supported.

---
## Type Inference

In short, the language Agile-C is a weakly and statically typed subset of C, with a class-based type inference system.

Agile-C (or in general, C) is weakly typed because it does perform certain kinds of implicit type conversions. For example, a C programmer may feel comfartable assigning a `int` to a `long`, but this assignment will be rejected in stronly typed language (like Rust). What happens here is the C compiler implicitly promote the `int` to `long` and then perform the assignment.

Agile-C (or in general, C) is statically typed, which means most, if not all, of its type checking happens during the compilation time. On the contrary, dynamic typing language, like Python, does not really cares about the type of a variable until this variable is actually used in runtime. Then the Python interpreter will check the type of this variable actually fulfill the requirement, in the runtime.

Finally, a class-based is borrowed from termilogies used to discuss how a OOP language is implemented. When it talks to OOP language, there are generally two basic approches, class-based or trait-based. The class-based is relatively familiar, programmers define classes, which encapsulate methods and states, and inherit different classes to form a class hierarchy for code reuse. Traits, however, are interfaces that define methods needed to implement this trait without the notion of internal states. Programmers enhence a structure, which contains states and data by implementing various traits for it.

So what does a "class-based type inference system" means. Let's look at a example. For instance, there is a expression `a + b`, then you know variable `a` and `b` should support the operator `+`. For a class-based system, this means "`a` and `b` should belongs to a set of classes that support the `+` operator". For C, this generally means all numerical types (remember we do not have operator overloading in C). Then, how do we represent "a type that is numerical" in a type hierarchy? A simple answer is to define a common bottom subtype of all numerical types and a common supertype of all numerical types. This is exactly the approch Agile-C takes: Agile-C uses `double` as the common super type, and defines a new type `byte` as the common numerical subtype. So `a + b` is translated to constraint "`a` and `b` should be a type that is the supertype of `byte` but subtype of `double`". For trait-based system, `a + b` simply means `a` and `b` should satisfy certain trait, let say `Add`. Now we can see, trait-based approach is much cleaner, without intervining with dedicate type hierarchy and operator overloading. However, Agile-C still takes the class-based approach because C has no notion of traits, so we have to define traits and implements from scrach.

---



## Specification

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

![type hierarchy](./type_hierarchy.jpg)

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
