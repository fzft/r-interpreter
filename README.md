# Monkey Programming Language Interpreter

Welcome to the Monkey programming language interpreter. This project is a lightweight and powerful interpreter built for educational purposes. While the original concept is based on the "Writing An Interpreter In Go" book, this implementation is in Rust.

## Table of Contents

- [Components](#components)
    - [Lexer](#lexer)
    - [Parser](#parser)
    - [Evaluator](#evaluator)
    - [Built-in Functions](#built-in-functions)
- [REPL](#repl)

## Components

### Lexer

**File:** `lexer.rs`

The **Lexer** (or lexical analyzer) reads the source code as a sequence of characters and converts them into meaningful tokens.

### Parser

**File:** `parser.rs`

The **Parser** reads the sequence of tokens and organizes them into an Abstract Syntax Tree (AST).

### Evaluator

**File:** `eval.rs`

The **Evaluator** traverses the AST and performs the actual computation.

### Built-in Functions

**File:** `object.rs`

Monkey provides a suite of built-in functions to ease various common operations. Some of the built-in functions include:

- `len()`: Returns the length of an object.
- `first()`: Retrieves the first element from a list.
- `last()`: Retrieves the last element from a list.
- `rest()`: Gets all elements of a list except the first one.
- `print()`: Outputs the provided value to the console.

These built-in functions offer native capabilities without needing any external libraries.

## REPL

The REPL (Read-Eval-Print Loop) is an interactive shell for our language. Write Monkey code and immediately see the results.

### Basic Usage

Start the REPL with:
```bash
$ cargo run
```

The REPL will start and you can begin writing Monkey code:
```bash
$ cargo run

>> let a = 5;
>> let b = 10;
>> let add = fn(x, y) { x + y };
>> add(a, b);
15
>> let fibonacci = fn(x) { if (x < 2) { return x; } else { return fibonacci(x - 1) + fibonacci(x - 2); } };
>> fibonacci(10);
55

# built-in functions
>> let myArray = [1, 2, 3, 4];
>> len(myArray);
4
```