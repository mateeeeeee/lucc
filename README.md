# lucc - little & useless C compiler
Toy C compiler for x86-64. Both lexer and parser are handwritten (i.e. not using generators).
The assembly is generated from AST traversal. 

## Features
  * operators:
    - additive: `+`, `-`, `+=`, `-=`, `++`, `--`
    - multiplicative: `*`, `/`, `*=`, `/=`
    - relation: `==`, `!=`, `>`, `>=`, `<`, `<=`
    - shift: `>>`, `<<`, `>>=`, `<<=`
    - bitwise: `&`, `|`, `^`, `~`, `&=`, `|=`, `^=`
    - logic: `&&`, `||`, `!`
  * control statements: `if` `else`, `switch`, `goto`, `?:`
  * loop statements: `for`, `while`, `do` `while`, `break`, `continue`
  * pointers and arrays
  * string literals
  * `typedef`
  * C Postprocessor subset
  * `static`, `extern`
  * `sizeof`, `alignof`, `alignas`
  * constant folding
  * functions
  * scopes
  * casts & conversions
  * `enum`
  * `struct` (wip)

## Todo
  * postprocessor: `#if`,`#elif`, macro functions
  * `union`
  * floating point numbers
 
## Not planned
  * variadic functions and macros
  * thread local storage, atomic numbers
  * bitfields

## Command line options
  * -h, --help: for displaying available compile options
  * -c: no linking is preformed, only .obj files are produced
  * -S: no assembling is preformed, only .asm files are produced
  * -E: Only preprocessor is run
  * --directory: Directory where the source files are located
  * -i: Input files
  * -o: Output file
  * --dll: builds a dll
  *	--lib: builds a lib
  * --astdump: Dump AST to console
  * --test: used for running g-tests
  * --ti: input test code (used for g-tests)

## Dependencies
  * MASM (included with Visual Studio) is used as an assembler.
  * Microsoft Linker (included with Visual Studio) is used for linking.
  * [spdlog](https://github.com/gabime/spdlog) for logging.
  * [CLI11](https://github.com/CLIUtils/CLI11) for command line parsing.
