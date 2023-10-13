# lucc - little & useless C compiler
Toy C compiler for x86-64. Both lexer and parser are handwritten (i.e. not using generators).
The assembly is generated from AST traversal. 

## Features
  * operators:
    - additive: +, -, +=, -=, ++, --
    - multiplicative: *, /, *=, /=
    - relation: ==, !=, >, >=, <, <=
    - shift: >>, <<, >>=, <<=
    - bitwise: &, |, ^, ~, &=, |=, ^=
    - logic: &&, ||, !
  * pointers and arrays
  * control statements
    - if statement
    - switch statement
    - goto statement
    - ternary operator
  * loop statements 
    - while statement
    - for statement
    - do while statement
    - break & continue
  * string literals
  * typedefs
  * subset of C Postprocessor
  * static, extern
  * sizeof, alignof, alignas
  * constant folding
  * functions
  * scopes
  * casts & conversions
  * enums

## Todo
  * postprocessor: #if,#elif, macro functions
  * tags (structs, unions)
  * floating point numbers
 
## Not planned
  * variadic functions and macros
  * thread local, atomic
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
