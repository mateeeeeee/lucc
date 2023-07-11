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
  * alignas
  * string literals
  * casts, conversions
  * postprocessor: #if,#elif, macro functions
  * register spilling
  * tags (structs, unions, enums)
 
## Not planned
  * floating points
  * variadic functions and macros
  * thread local, atomic
  * bitfields

## Dependencies
  * MASM (included with Visual Studio) is used as an assembler.
  * Microsoft Linker (included with Visual Studio) is used for linking.
