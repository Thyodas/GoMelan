<img src="images/glados_logo.png" alt="GLaDOS Logo" width=250 height=250 align="right" />

# GLaDOS - Generic Language and Data Operand Syntax

GLaDOS is a programming language project developed as part of the B5 - Advanced Functional Programming course (B-FUN-500). It's designed to be a minimalist Lisp interpreter initially but can be extended into a more advanced language with a custom syntax, evaluation, and compilation.

> **Note**
> Here is the full documentation for GoMelan: https://glados.guillaume-hein.fr/

## Table of Contents

- [GLaDOS - Generic Language and Data Operand Syntax](#glados---generic-language-and-data-operand-syntax)
  - [Table of Contents](#table-of-contents)
  - [Introduction](#introduction)
  - [Project Overview](#project-overview)
  - [Getting Started](#getting-started)
  - [Usage](#usage)
  - [Project Structure](#project-structure)
  - [Features](#features)
  - [Contributing](#contributing)
    - [GitHub Actions](#github-actions)
      - [On push on any branch](#on-push-on-any-branch)
      - [On push on main OR merge to main](#on-push-on-main-or-merge-to-main)
    - [Pre-commit Hooks](#pre-commit-hooks)
      - [Prerequisites](#prerequisites)
      - [Getting Started](#getting-started-1)
      - [Using Pre-Commit Hooks](#using-pre-commit-hooks)
      - [Updating Pre-Commit Hooks](#updating-pre-commit-hooks)
      - [Customizing Hooks (Optional)](#customizing-hooks-optional)
  - [License](#license)

## Introduction

Welcome to GLaDOS! This project is an exploration of programming language development in Haskell. The goal is to create a functional language with a Lisp-like core and expand it into a more feature-rich language.

Our language is called GoMelan, it is a typed language similar to Rust. With the latter it is possible to carry out complex scripts using functions, recursion, loops, internal functions, advanced types, functions prototype, include other file. All this with quick-to-understand syntax by using Syntactic sugar.

## Project Overview

- **Part 0: The Enrichment Testing Center**: In this mandatory part, we focus on building a robust foundation for the project. This includes setting up a build system, creating unit and integration tests, and implementing a continuous integration and continuous delivery (CI/CD) pipeline.

- **Part 1: Lots of Irritating Superfluous Parentheses (LISP)**: In this phase, we start by implementing a minimalist Lisp interpreter. It includes handling symbolic expressions, defining functions (including lambdas), conditional expressions, and built-in functions.

- **Part 2: Embrace and Extend**: In the final part, we aim to enhance our language. This involves defining a custom syntax, implementing a virtual machine (VM), and creating a compiler for efficient code execution. Additional features, like data types, side effects, type inference, and metaprogramming, can be added for bonus points.

## Getting Started

To get started with GLaDOS, follow these steps:

1. Clone the repository:

   ```bash
   git clone https://github.com/your-username/glados.git
   cd glados
   ```

2. Build the project using your preferred build system (e.g., Stack):

   ```bash
   make
   ```

3. Run the GLaDOS interpreter/compiler:

   ```bash
   ./glados
   ```

4. Compile a GoMelan script into a binary

   ```bash
   ./glados build YOUR_FILE
   ```

5. Then execute the binary

   ```bash
   ./glados run out.gomc
   ```

You also can run some unit tests by using the following command

1. Run the tests

   ```bash
   make tests_run
   ```

## Usage

### Our language GoMelan

Here is an example of our language reproduicing factorial functions (using recursive):

```rs
fn factorial(n: Int) -> Int
{
    if (n <= 1) {
        return 1;
    } else {
        return n * factorial(n - 1);
    }
}

fn main() -> Int
{
    return factorial(5);
}
```
`Output: 120`

Our langauge can also use some internal functions like `len` for get a size of a List or conversion functions:
Here we can use the `len` function to read the List and then sum it, we also use prototype to get the function.
```rs

fn sum(a: [Int]) -> Int;

fn main() -> Int
{
   a: [Int] = [1, 2, 3, 4];

   return sum(a);
}

fn sum(a: [Int]) -> Int
{
      s: Int = 0;

      for (i: Int = 0; i < len(a); i++) {
         s += a[i];
      }
      return s;
}
```
`Output: 10`

You can find more of thoses examples by execute them in the `examples/` folder.

### LISP Usage

Here are some example usages of the first GLaDOS step (work with GLaDOS LISP release):


```glados
(define (add a b)
  (+ a b))

(add 3 4)
```

```glados
(if (< 1 2)
    "One is less than two"
    "This won't be printed")
```

For more examples, check the project's [examples](examples/) directory.

## Project Structure

The project structure is organized as follows:

- `app/`: Contains the main function for the project.
- `src/`: Contains the source code for the GLaDOS interpreter and compiler.
- `test/`: Holds unit and integration tests for the project.
- `examples/`: Contains a list of GoMelan code examples

## Features

- [x] Lisp Parser
- [x] Lisp AST
- [x] Minimal Lisp interpreter
- [x] Custom syntax and grammar (Part 2)
- [x] Virtual Machine (VM) and Compiler (Part 2)
- [x] Detailed language grammar (BNF) (Part 2)
- [x] Displayed outputs (Part 2)
- [x] Our compiler is able to output it's result as a binary format (bonus) (use -v at the run)
- [x] VM is able to load a binary and run it (bonus)
- [x] More data types (Float, List) (bonus)
- [x] Type inference (bonus)
- [x] Imperative constructs (bonus)
- [x] Syntactic Sugar (bonus)
- [x] Globals variables (bonus)
- [x] Prototype functions (bonus)
- [x] Include other files and lib (bonus)
- [x] Internal functions (len, intToString, stringToInt, floatToInt, intToFloat, floatToString, ...) (bonus)

## Contributing

### GitHub Actions

In this project, GitHub Actions will run the following checks:

#### On push on any branch
* **Build**: The project will be built using Stack.
* **Tests**: The project's unit and integration tests will be run.

#### On push on main OR merge to main
* **Build**: The project will be built using Stack.
* **Tests**: The project's unit and integration tests will be run.
* **Release**: A new release will be created and published as draft on GitHub.
* **Binary**: The project's binary will be built and published as an asset on the release.

### Pre-commit Hooks

Pre-commit hooks are automated scripts that run before each Git commit, ensuring code quality and adherence to coding standards. Follow this guide to set up and use pre-commit hooks in this project.

#### Prerequisites

Before you begin, make sure you have the following installed:

- **Git**: Pre-commit hooks are Git hooks, so you need Git installed on your system.

#### Getting Started

1. **Install Pre-Commit**:

   Install the pre-commit tool if you haven't already. Pre-commit is a Python package:

   ```bash
   pip install pre-commit
   ```

2. **Initialize Pre-Commit**:

   Run the following command in the project's root directory to initialize pre-commit:

   ```bash
   pre-commit install
   ```

   This command sets up the hooks defined in the `.pre-commit-config.yaml` file.

#### Using Pre-Commit Hooks

Pre-commit hooks will automatically run before each Git commit. Here's how they work:

1. When you run `git commit`, pre-commit will execute the configured hooks one by one.

2. If a hook fails (for example, if a test fails or coding standards are not met), the commit will be blocked, and you'll see an error message.

3. You can fix the issues reported by the hooks and try the commit again.

#### Updating Pre-Commit Hooks

If the `.pre-commit-config.yaml` file is updated with new hooks or changes, contributors should run the following command to update their pre-commit hooks:

```bash
pre-commit autoupdate
```

This command updates the pre-commit configuration to match the latest version defined in the repository.

#### Customizing Hooks (Optional)

If you need to customize the behavior of a specific hook, you can do so by modifying the `.pre-commit-config.yaml` file. Refer to the project documentation or README for specific customization instructions.




## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
