#pragma once
#include "../Lucc/Core/Defines.h"


#define LU_CODE(...) LU_STRINGIFY(__VA_ARGS__)
#define LU_CODE_EX(...) LU_STRINGIFY(int main(void){__VA_ARGS__})

#if NDEBUG

#define LUCC_EX(...) system(LU_STRINGIFY(Luccd -test -i LU_CODE_EX(__VA_ARGS__)))
#define LUCC_EX_DEBUG(...) system(LU_STRINGIFY(Luccd -test -debug -i LU_CODE_EX(__VA_ARGS__)))

#define LUCC(...) system(LU_STRINGIFY(Luccd -test -i LU_CODE(__VA_ARGS__)))
#define LUCC_DEBUG(...) system(LU_STRINGIFY(Luccd -test -debug -i LU_CODE(__VA_ARGS__)))

#else

#define LUCC_EX(...) system(LU_STRINGIFY(Lucc -test -i LU_CODE_EX(__VA_ARGS__)))
#define LUCC_EX_DEBUG(...) system(LU_STRINGIFY(Lucc -test -debug -i LU_CODE_EX(__VA_ARGS__)))

#define LUCC(...) system(LU_STRINGIFY(Lucc -test -i LU_CODE(__VA_ARGS__)))
#define LUCC_DEBUG(...) system(LU_STRINGIFY(Lucc -test -debug -i LU_CODE(__VA_ARGS__)))

#endif