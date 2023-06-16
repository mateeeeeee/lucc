#include "pch.h"
#include "../Lucc/Core/Defines.h"
#include "../Lucc/Compiler/Compiler.h"
#include "../Lucc/Frontend/Diagnostics.h"

using namespace lucc;
using namespace diag;

#define LU_CODE(...) LU_STRINGIFY(__VA_ARGS__)
#define LU_CODE_SAMPLE(...) LU_STRINGIFY(int main{__VA_ARGS__})

#define LUCC_SAMPLE(...) system(LU_STRINGIFY(Lucc -test -i LU_CODE_SAMPLE(__VA_ARGS__)))
#define LUCC_SAMPLE_DEBUG(...) system(LU_STRINGIFY(Lucc -test -debug -i LU_CODE_SAMPLE(__VA_ARGS__)))

#define LUCC(...) system(LU_STRINGIFY(Lucc -test -i LU_CODE(__VA_ARGS__)))
#define LUCC_DEBUG(...) system(LU_STRINGIFY(Lucc -test -debug -i LU_CODE(__VA_ARGS__)))

TEST(Lucc, Typedef)
{
	EXPECT_EQ(LUCC(typedef int t; int main(void) { t x = 12; return x; }), 12);
	EXPECT_EQ(LUCC_SAMPLE(short x = 12; return x;), 12);
}

TEST(Lucc, Arithmetic)
{
	
}

TEST(Lucc, Atomic)
{

}

TEST(Lucc, Alignof)
{

}

TEST(Lucc, Cast)
{

}

TEST(Lucc, Const)
{

}

TEST(Lucc, Constexpr)
{

}

TEST(Lucc, Control)
{

}

TEST(Lucc, Declaration)
{

}

TEST(Lucc, Enum)
{

}

TEST(Lucc, Extern)
{

}

TEST(Lucc, Initialization)
{

}

TEST(Lucc, Literal)
{

}

TEST(Lucc, Macro)
{

}

TEST(Lucc, Pointer)
{

}

TEST(Lucc, Sizeof)
{

}

TEST(Lucc, String)
{

}

TEST(Lucc, Static)
{

}


TEST(Lucc, Conversion)
{

}

TEST(Lucc, ThreadLocal)
{

}

TEST(Lucc, Variable)
{

}