# Regular Expression Matching Engine

This project implements a library for constructing and using deterministic finite automata (DFAs) to match strings with regular expressions. It supports 2 implementations of DFA construction, utilities for bit arrays, lists, and lookup tables, as well as a simple matching engine.

---

## Features

- **Regular Expression Parsing**  
  Parses strings into a structured regular expression type.

- **DFA Construction**  
  Two implementations for DFA construction:

  - `RE1`: Uses NFA to DFA conversion.
  - `RE2`: Uses Brzozowski derivatives for direct DFA construction.

- **Matching Engine**  
  Provides a high-level interface for testing if a string matches a regular expression.

- **Utility Modules**
  - `Bitarray`: Efficient bit array operations.
  - `MyList`: List utilities such as sorted merging and equality checks.
  - `LookupTable`: Precomputed lookup tables for optimized operations.
- **Simple Calculator**
  - A program implementing a simple calculator is provided in `examples/calculator.ml`. It demonstrates how to:
    - Read a string representing a mathematical expression.
    - Use the regular expression engine to tokenize the input.
    - Parse the tokens.
    - Calculate and return the result of the expression.

---

## Installation

This project uses [Dune](https://dune.build/) as its build system. Ensure you have Dune and OCaml installed on your system.

### Build the Project

Run the following command to build the project:

```bash
dune build
```

### Run the calculator

Run the following command to run the calculator code:
The code will calculate "85 * (36.8+90*7)"

```bash
dune exec examples/calculator.exe
```

### Run Tests (Optional)

If tests are implemented using `OUnit2`, you can run them as follows:

```bash
dune runtest
```

---

## Usage

### Example: Parsing and DFA Construction

```ocaml
open Lib.RegE.RE1  (* or RE2 *)
open Lib.Engine

let regex = parse_expression "(a|b)*c"
let alphabet = [C 'a'; C 'b'; C 'c']
let dfa = construct_dfa regex alphabet
```

### Example: Using the Matching Engine

```ocaml
open Lib.Engine.Engine1  (* or Engine2 *)

let engine = create_machine "(a|b)*c"
let is_match = match_expression engine "aababc"
```

---

## Modules

### Main Modules

- **`RegE`**: Defines types and utilities for regular expressions.
  - **`RE1` / `RE2`**: Two implementations of DFA construction.
- **`Utils`**: Defines the following modules
  - **`Bitarray`**: Efficient bit array operations.
  - **`MyList`**: List utilities for sorted operations.
  - **`LookupTable`**: Utilities for creating and querying lookup tables.
- **`Engine`**: Defines the parser, implements matching functionality and includes:
  - **`Engine1` / `Engine2`**: Matching engines using `RE1` and `RE2` implementations.

### Key Functions

- `parse_expression`: Parses a string into a regular expression.
- `create_machine`: Creates a machine for expression matching.
- `match_expression`: Checks if a string matches a regex using the DFA.
