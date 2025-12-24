# OCaml C-Subset Compiler

A multi-pass compiler implementation translating a subset of the C programming language into x86-64 assembly (AT&T syntax). The system implements a complete compilation pipeline including lexical analysis, recursive descent parsing, semantic validation, intermediate representation generation, and code emission compliant with the System V AMD64 ABI.

## Build

Compile the source modules into the `mycc` executable using the OCaml bytecode compiler:

```bash
ocamlc -o mycc ast.ml asm.ml tacky.ml lexer.ml parser.ml semanticanalysis.ml tackygen.ml codegen.ml emit.ml driver.ml

```

## Usage

```bash
./mycc [options] <source_file>

```

The driver accepts a C source file, preprocesses it via `gcc -E`, and executes the pipeline. The output depends on the stage flags provided. If no flag is provided, the full pipeline runs, producing an executable via `gcc`.

### Stage Selection Flags

* `--lex`: Halt after tokenization; outputs token stream.
* `--parse`: Halt after parsing; outputs AST structure.
* `--validate`: Halt after semantic analysis; outputs validated AST.
* `--tacky`: Halt after IR generation; outputs Tacky IR.
* `--codegen`: Halt after assembly generation; outputs abstract assembly instructions.
* `--asm`: Halt after emission; writes the `.s` assembly file to disk.

## Architecture

The compiler functionality is distributed across the following modular phases:

### 1. Lexical Analysis (`lexer.ml`)

Converts input character streams into a strongly-typed token sequence.

* **Keywords:** `int`, `void`, `return`, `if`, `else`, `do`, `while`, `for`, `break`, `continue`, `switch`, `case`, `default`, `goto`, `static`, `extern`.
* **Identifiers:** Alphanumeric strings (must start with non-digit).
* **Literals:** Integer constants.
* **Operators:** Handles multi-character operators (e.g., `<<=`, `&&`, `--`, `->`) using lookahead logic.

### 2. Parsing (`parser.ml`, `ast.ml`)

Implements a recursive descent parser defining the language grammar.

* **Expression Parsing:** Handles operator precedence and associativity for arithmetic, logical, and bitwise operations.
* **Statement Parsing:** Supports compound blocks, selection statements (`if`, `switch`), iteration statements (`while`, `do`, `for`), and jump statements.
* **Declarations:** Parses function prototypes, definitions, and variable declarations with optional initialization.

### 3. Semantic Analysis (`semanticanalysis.ml`)

Performs static analysis and AST transformation.

* **Symbol Resolution:** Maintains a symbol table mapping identifiers to types and storage classes.
* **Variable Renaming:** transform variable names to unique internal identifiers (e.g., `var` -> `var.1`) to prevent shadowing collisions during code generation.
* **Validation:** Enforces type safety (strictly `int` context), verifies declaration existence, and checks initialization rules for `static` vs `auto` storage durations.

### 4. Intermediate Representation (`tacky.ml`, `tackygen.ml`)

Lowers the high-level AST into "Tacky," a Three-Address Code (TAC) representation.

* **Control Flow Lowering:** Structured loops and conditionals are converted into explicit labels and conditional jumps (`JumpIfZero`, `JumpIfNotZero`).
* **Expression Flattening:** Complex expressions are decomposed into sequential atomic operations using temporary variables.
* **Short-Circuiting:** Implements logical `&&` and `||` via control flow branching.

### 5. Code Generation (`codegen.ml`)

Translates Tacky IR into abstract x86-64 assembly.

* **Pseudo-Registers:** Maps IR temporaries to infinite pseudo-registers.
* **Stack Allocation:** resolves pseudo-registers to stack offsets relative to `%rbp`.
* **ABI Compliance:** * Integer arguments 1-6 passed in `RDI`, `RSI`, `RDX`, `RCX`, `R8`, `R9`.
* Arguments 7+ pushed to stack.
* Maintains 16-byte stack alignment during function calls.

### 6. Emission (`emit.ml`, `asm.ml`)

Serializes abstract assembly to disk.

* **OS Support:** Detects host OS to apply appropriate symbol prefixes (underscores for macOS/BSD).
* **Format:** Outputs standard AT&T assembly syntax compatible with the GNU Assembler (GAS).

## Language Specification

The compiler supports the following C subset features:

### Types and Storage

* **Type:** `int` (signed 32-bit).
* **Storage Classes:** `static` (internal linkage), `extern` (external linkage).

### Operators

* **Unary:** `-`, `~`, `!`, `++`, `--` (prefix/postfix).
* **Binary Arithmetic:** `+`, `-`, `*`, `/`, `%`.
* **Bitwise:** `&`, `|`, `^`, `<<`, `>>`.
* **Logical:** `&&`, `||`.
* **Comparison:** `==`, `!=`, `<`, `<=`, `>`, `>=`.
* **Assignment:** `=`, `+=`, `-=`, `*=`, `/=`, `%=`, `&=`, `|=`, `^=`, `<<=`, `>>=`.
* **Conditional:** Ternary operator `? :`.

### Control Structures

* **Selection:** `if`, `if-else`, `switch` (with `case` fallthrough and `default`).
* **Iteration:** `while`, `do-while`, `for`.
* **Jumps:** `goto`, `break`, `continue`, `return`.