# Agile-C

A Type Inference Transpiler for the C Programming Language

## Motivation

Type inference now is an important feature for modern programming languages. You can find it from the famous global inference in Haskell, local inference in Rust and Scala, and limited form in old language like C++ and Java. There is no doubt a smart type inference system can reduce the cognitive load of programmers while not giving up the benefits of strongly staticly type systems.

However, although the general idea of type inference, type unification, is quite straightforaward and concise, there is a large gap between the theory and a really working type inference system. What's worse, it is not easy to find something like "Write your a type inference engine in 21 days" that is accessable while practical for common programmers who have no expertise in programming language design and type systems to get a feeling about the principle of type inference.

Agile-C is my bachelar graduation project, which provides basic type inference for most commen used C features. This project is practice for me before I read more serious literature about programming language design and type systems. This porject is intended to ONLY educational purpose.

## Introduction

Agile-C is a transpiler, which consumes a C sourse file that omits certain type declarations and outputs a valid C sourse file. You can also invoke GCC from Agile-C directly and get a executable file directly.

Following sections are divided into three parts: First, we will walk you through "Getting Started" section to show you how Agile-C works in action; Then, we will talk about some basic knowledge about type systems; Finally, we will talks about the implementation details of Agile-C and also its formal syntax.


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

## Features

The formal syntax, type hierarchy, and some notes related to the implementation details are given in the last [Specification](#Specification). Here we just quickly go through what Agile-C is capable of, and a few quirks you are likly to run into.

- Agile-C supports all kinds of literals, including string, character, numerical. However, you are not allow to use integer suffix (`L`).
- Most expressions and statements are supported.
- Most primary types are supported, but not specifier like `const`.
- Array type is supported, and the only valid initialization syntax is `arr = { 1 }`. The designated initializer for arrays is not supported.
- Structures are supported, but the struct definition must be complete (all members must have known types), and the declaration of struct type cannot be omitted. The only valid initialization syntax is `strcture = { .member = 1 }`, and all members must be explicitly initialized.
- Functions are supported.
- For macros, only `#include` is supported.


## Overview of the Type System of Agile-C

In short, the language Agile-C is a weakly and statically typed subset of C, with a class-based type inference system.

Agile-C (or in general, C) is weakly typed because it does perform certain kinds of implicit type conversions. For example, a C programmer may feel comfartable assigning a `int` to a `long`, but this assignment will be rejected in stronly typed language (like Rust). What happens here is the C compiler implicitly promote the `int` to `long` and then perform the assignment.

Agile-C (or in general, C) is statically typed, which means most, if not all, of its type checking happens during the compilation time. On the contrary, dynamic typing language, like Python, does not really cares about the type of a variable until this variable is actually used in runtime. Then the Python interpreter will check the type of this variable actually fulfill the requirement, in the runtime.

Finally, a class-based is borrowed from termilogies used to discuss how a OOP language is implemented. When it talks to OOP language, there are generally two basic approches, class-based or trait-based. The class-based is relatively familiar, programmers define classes, which encapsulate methods and states, and inherit different classes to form a class hierarchy for code reuse. Traits, however, are interfaces that define methods needed to implement this trait without the notion of internal states. Programmers enhence a structure, which contains states and data by implementing various traits for it.

So what does a "class-based type inference system" means. Let's look at a example. For instance, there is a expression `a + b`, then you know variable `a` and `b` should support the operator `+`. For a class-based system, this means "`a` and `b` should belongs to a set of classes that support the `+` operator". For C, this generally means all numerical types (remember we do not have operator overloading in C). Then, how do we represent "a type that is numerical" in a type hierarchy? A simple answer is to define a common bottom subtype of all numerical types and a common supertype of all numerical types. This is exactly the approch Agile-C takes: Agile-C uses `double` as the common super type, and defines a new type `byte` as the common numerical subtype. So `a + b` is translated to constraint "`a` and `b` should be a type that is the supertype of `byte` but subtype of `double`". For trait-based system, `a + b` simply means `a` and `b` should satisfy certain trait, let say `Add`. Now we can see, trait-based approach is much cleaner, without intervining with dedicate type hierarchy and operator overloading. However, Agile-C still takes the class-based approach because C has no notion of traits, so we have to define traits and implements from scrach.

![type hierarchy](./type_hierarchy.jpg)



## How Agile-C Works

Type inference is in essential an automatic detection of the data type of expressions and variables given some context information including literals whose types are trivially known, variables whose types are known, and constraints imposed by language semantics.

Now, we will walk through the whole process of how Agile-C works with a few examples.

1. Preprocessing

Suppose we have a really simple program below. The first thing Agile-C will do is to consult a internal module to get complete function declaration of `assert.h` and insert it into the source file, so we can later match the usage of `assert` to its declaration. The transformed program is shown below.

```C
#include <assert.h>

main() {
    m = 1;
    assert(m);
    return 0;
}
```

```C
#include <assert.h>
void assert(int expression);

main() {
    m = 1;
    assert(m);
    return 0;
}
```

2. Lexing and Parsing

Lexing is a really simple task and we will just skip it.

As parsing, Agile C uses a Pratt parser (operator-precedence parser), which associates semantics with tokens instead of grammer rules, with small tweaks on how it parses potentially missing type specifiers.

Now agile C can complete missing type specifiers in three places: the return type of a function, types for function parameters, and types for variable definitions.

For functions' return types and parameters, generally the parser knows there should be types here, and it will first try to parse any type specifiers. If no type specifier tokens are encountered, the parser will insert a dummy type specifier.

For variable definitions, the parser will maintain a environment object for each scope, which is a hash set containing name of variables defined in the current scope. When the parser produces a assignment expression, it will search those environment objects, and if the variable being assigned is not defined yet, the parser will transform this assignment expression into a definition statement with a dummy type specifier `T`.

The transformed program is shown below. The parser add `T` type for return type of `main` and variable `m`. We will see how the parser handle function parameter in a later example.

```C
#include <assert.h>
void assert(int expression);

T main() {
    T m = 1;
    assert(m);
    return 0;
}
```



3. Determine Trivial Types

In Agile-C, "symbol" is the basic unit for type constraints. A symbol can be one of four kinds entites: local variables, function parameters, function returns, expressions. For the example program, we can list important symbols as follow, where `T` type means a dummy type that we need to infer.

| Symbol ID | Description | Type |
| --- | --- | --- |
| 1 | parameter `expression` of `assert` | `int` |
| 2 | return value of `main` | `T` |
| 3 | variable `m` of `main` | `T` |
| 4 | expression `1` of `main` | `Byte` |
| 5 | expression `0` of `main` | `Byte` |

4. Constrain All Symbols

The next step is to find constraints for symbols whose type is still unknown. In essential, we may treat data flow of a program as a series of assignments. Suppose we have a expression `a = b`, reasonablely, we can assume two constraints: First, the type of symbol `a` should be at least as powerful as the type of symbol `b`. Second, the type of symbol `b` should be at most as powerful as the type of symbol `a`. Or in a more concise way, we say `a` is a upper bound of `b`, and `b` is a lower bound of `a`. This idea can be easily genalized to other language structures. For example, function calls are assignments of arguments to parameters and then assign the return value to some other variables, and a `return` statement is simply assign whatever expression on its right to the return symbol of this function. Now we can update the symbol table from the last section with upper and lower bounds included.

| Symbol ID | Description | Type | Upper Bound | Lower Bound |
| --- | --- | --- |
| 1 | parameter `expression` of `assert` | `int` | `Any` | 3 |
| 2 | return value of `main` | `T` | `Any` | 5 |
| 3 | variable `m` of `main` | `T` | 1, 4 | 4 |
| 4 | expression `1` of `main` | `Byte` | 3 | `Nothing` |
| 5 | expression `0` of `main` | `Byte` | 2 | `Nothing` |

First notice a type bound can be either a type (for example `Any`) or another symbol. Also notice that symbol 3 is bounded from both direction by symbol 4, which means they should have exactly the same type, which is just what a assignment expression means.

5. Resolve Type Bounds
 Resolving is a iterative process, we look through every symbol to check if all symbols that act as its upper or lower bounds have had a determined concrete type. If so, we unify all upper bounds into a single type, say `upper`, and unify all lower bounds into another single type, say `lower`, then unify `upper` and `lower` to a final concrete type.

Let's pretend we are the Resolver and check every symbol for the first time.

- Symbol 1 has a undertermined bound, i.e. symbol 3, stop resolving.
- Symbol 2 is bounded by `Any` and `Byte` (symbol 5), so we determine its type should by `Byte`.
- Symbol 3 is bounded by `Byte` (unifying symbol 1 and 4) and `Byte` (symbol 4), so we determine its type should by `Byte`.
- Symbol 4 is bounded by `Byte` (symbol 3) and `Nothing`. We also notice it has had a determined type `byte`, which perfectly lays between this interval.
- Symbol 5 is bounded by `Byte` (symbol 2) and `Nothing`. We also notice it has had a determined type `byte`, which perfectly lays between this interval.

After the first iterative, types for all symbols are determined and types for symbol 2-5 are also checked. Now let's go to the second iteration.

- Symbol 1 is bounded by `Any` and `Byte` (symbol 3). We also notice it has had a determined type `Int`, which perfectly lays between this interval.
- Other symbols does not change.

In the second iteration, types for all symbols are determined and checked, and we are done!

6. Heuristic Resolving

However, in more complicated programs, there is often some kind of reference loop. Let's look a slightly more complex program below.

```C
#include <assert.h>

identity(n) {
    return n;
}

main() {
    m = 1;
    assert(identity(m));
    return 0;
}
```

Now the problem is in the function call to `identity` will cause variable `m` and parameter `n` refer to each other as type bounds. The system will converge (cannot not make further deduction) without actually determine the type of `m` and `n`!

How we solve this problem is to add a little heuristic. Specifically, after the Resolver first converge, we will try to resolve each unknown symbol again, ignoring all undetermined type bounds. Each time the type of a unknown symbol is determined by heuristic resolving, we will try to rerun the resolving process again until it converges again.

For example, `m` is bounded by a numerical literal and `n`. Now we ignore `n`, then `m` is determined to be `Byte`, and the loop is break. Furthermore, the Resolver will rerun the whole checking process to make sure `Byte` actually fits all type bounds.

## Type Inference for some Special Cases

The six steps we just talks about can handle such a large set of program, but there are still some pitfalls need extra processing.

1. Infix Expression

Now let's make our program a little more complex.

```C
#include <assert.h>

add_one(n) {
    return n + 1;
}

main() {
    m = 1;
    assert(add_one(m));
    return 0;
}
```

Now we have a infix expression in `add_one`. So what is the type relationship between `n`, `1`, and `n + 1`? Intuitively, the type of `n + 1` should be the more powerful type of `n` and `1`, but we cannot represent this relationship by upper and lower type bounds.

We add a special field for each infix expression symbol, containing symbols of its left and right operend. When the Resolver resolves a symbol with this field set, the Resolver will pick the more powerful type from two operends as the overall type of the infix expression.

2. Pointer, Dot Operator, Arraw Operator Transformation

Now let's make our program a little more complex.

```C
#include <assert.h>

add_one_ptr(n) {
    return *n + 1;
}

main() {
    m = 1;
    assert(add_one_ptr(&m));
    return 0;
}
```

Now for expression symbol `*n`, we will have conflict type bounds. How? First, consider if we want to constrain this expression by its relationship with symbol `n`, we are actually adding a type bound of some kinds of pointer. However, if we want to constrain this expression by its relationship with numerical expression `1`, we are actually adding a type bound of some kinds of interger. The problem is this expression takes a symbol of some pointer type and transform it into the type this pointer is pointing to.

We add a special field to tell Resolver to automatically add or remove pointer wrapper for individual type bounds when encounter such symbols. For example, `*n` is bounded by `n` of type `Pointer<Byte>` and `1` of Type `Byte`. the Resolver will remove the `Pointer` wrapper of `n` and now these two types check.

Similarly, we can handle the dot operator (`structure.member`) and the arrow operator (`structure->member`).


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
