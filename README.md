# lucc
Toy C compiler for x86-64 made for learning purposes. Both lexer and parser are handwritten (i.e. not using generators).
No optimization passes are performed, the assembly is generated from AST traversal. 

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
  * typedefs
  * subset of C Postprocessor
  * static, extern
  * sizeof, alignof
  * constant folding
  * functions
  * scopes

## Todo
  * known bugs: pointer arithmetic, declspec parsing
  * alignas
  * string literals
  * casts, conversions
  * postprocessor: #if,#elif, macro functions
  * register spilling
  * tags (structs, unions, enums)
 
## Not planned
  * floating point numbers
  * variadic functions and macros
  * thread local, atomic
  * bitfields

## Command line options
  * -h, --help: for displaying available compile options
  * -c, --nolink: no linking is preformed, only .obj files are produced
  * -S, --noassembly: no assembling is preformed, only .asm files are produced
  * -E, --only-pp: Only preprocessor is run
  * -d: Directory where the source files are located
  * -o: The name of the executable
  * -ast-dump: Dump AST to console
  * -test: used for running g-tests
  * -i: input test code (used for g-tests)

## Dependencies
  * MASM (included with Visual Studio) is used as an assembler.
  * Microsoft Linker (included with Visual Studio) is used for linking.
  * For successful build, updated the _executables_path and _lib_path in Compiler.cpp to an existing paths on your machine
