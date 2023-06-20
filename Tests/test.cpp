#include "pch.h"
#include "../Lucc/Core/Defines.h"
#include "../Lucc/Compiler/Compiler.h"
#include "../Lucc/Frontend/Diagnostics.h"

using namespace lucc;
using namespace diag;

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

TEST(Arithmetic, AdditiveMultiplicativeOperators)
{
	EXPECT_EQ(LUCC_EX(return 0;), 0);
	EXPECT_EQ(LUCC_EX(return 42;), 42);
	EXPECT_EQ(LUCC_EX(return 5 + 20 - 4;), 21);
	EXPECT_EQ(LUCC_EX(return 12 + 34 - 5;), 41);
	EXPECT_EQ(LUCC_EX(return 5 + 6 * 7;), 47);
	EXPECT_EQ(LUCC_EX(return 5 * (9 - 6);), 15);
	EXPECT_EQ(LUCC_EX(return -10 + 20;), 10);
	//EXPECT_EQ(LUCC_EX(return (3 + 5) / 2;), 4);

	//ASSERT(5, 17 % 6);
	//ASSERT(5, ((long)17) % 6);
	//ASSERT(2, ({ int i = 10; i %= 4; i; }));
	//ASSERT(2, ({ long i = 10; i %= 4; i; }));
	
	//ASSERT(7, ({ int i = 2; i += 5; i; }));
	//ASSERT(7, ({ int i = 2; i += 5; }));
	//ASSERT(3, ({ int i = 5; i -= 2; i; }));
	//ASSERT(3, ({ int i = 5; i -= 2; }));
	//ASSERT(6, ({ int i = 3; i *= 2; i; }));
	//ASSERT(6, ({ int i = 3; i *= 2; }));
	//ASSERT(3, ({ int i = 6; i /= 2; i; }));
	//ASSERT(3, ({ int i = 6; i /= 2; }));
}
TEST(Arithmetic, RelationOperations)
{
	EXPECT_EQ(LUCC_EX(return 0 == 1), COMPILATION_FAILED);
	//EXPECT_EQ(LUCC_EX(return 42 == 42;), 1);
	//EXPECT_EQ(LUCC_EX(return 0 != 1;), 1);
	//EXPECT_EQ(LUCC_EX(return 42 != 42;), 0);

	//ASSERT(1, 0 < 1);
	//ASSERT(0, 1 < 1);
	//ASSERT(0, 2 < 1);
	//ASSERT(1, 0 <= 1);
	//ASSERT(1, 1 <= 1);
	//ASSERT(0, 2 <= 1);
	//
	//ASSERT(1, 1 > 0);
	//ASSERT(0, 1 > 1);
	//ASSERT(0, 1 > 2);
	//ASSERT(1, 1 >= 0);
	//ASSERT(1, 1 >= 1);
	//ASSERT(0, 1 >= 2);
}
TEST(Arithmetic, ShiftOperators)
{
	//ASSERT(1, 1 << 0);
	//ASSERT(8, 1 << 3);
	//ASSERT(10, 5 << 1);
	//ASSERT(2, 5 >> 1);
	//ASSERT(-1, -1 >> 1);
	//ASSERT(1, ({ int i = 1; i <<= 0; i; }));
	//ASSERT(8, ({ int i = 1; i <<= 3; i; }));
	//ASSERT(10, ({ int i = 5; i <<= 1; i; }));
	//ASSERT(2, ({ int i = 5; i >>= 1; i; }));
	//ASSERT(-1, -1);
	//ASSERT(-1, ({ int i = -1; i; }));
	//ASSERT(-1, ({ int i = -1; i >>= 1; i; }));
	//
}
TEST(Arithmetic, BitOperators)
{
	//ASSERT(-1, ~0);
	//ASSERT(0, ~- 1);
	//ASSERT(0, 0 & 1);
	//ASSERT(1, 3 & 1);
	//ASSERT(3, 7 & 3);
	//ASSERT(10, -1 & 10);
	//
	//ASSERT(1, 0 | 1);
	//ASSERT(0b10011, 0b10000 | 0b00011);
	//
	//ASSERT(0, 0 ^ 0);
	//ASSERT(0, 0b1111 ^ 0b1111);
	//ASSERT(0b110100, 0b111000 ^ 0b001100);
	//
	//ASSERT(2, ({ int i = 6; i &= 3; i; }));
	//ASSERT(7, ({ int i = 6; i |= 3; i; }));
	//ASSERT(10, ({ int i = 15; i ^= 5; i; }));
}
TEST(Arithmetic, PostIncrementDecrement)
{
	//ASSERT(2, ({ int i = 2; i++; }));
	//ASSERT(2, ({ int i = 2; i--; }));
	//ASSERT(3, ({ int i = 2; i++; i; }));
	//ASSERT(1, ({ int i = 2; i--; i; }));
	//ASSERT(2, ({ int i = 2; ++i; }));
	//ASSERT(2, ({ int i = 2; --i; }));
	//ASSERT(3, ({ int i = 2; ++i; i; }));
	//ASSERT(1, ({ int i = 2; --i; i; }));
}
TEST(Arithmetic, Pointers)
{
	//ASSERT(20, ({ int x; int* p = &x; p + 20 - p; }));
	//ASSERT(1, ({ int x; int* p = &x; p + 20 - p > 0; }));
	//ASSERT(-20, ({ int x; int* p = &x; p - 20 - p; }));
	//ASSERT(1, ({ int x; int* p = &x; p - 20 - p < 0; }));
}
TEST(Arithmetic, Arrays)
{
	//ASSERT(2, ({ int a[3]; a[0] = 0; a[1] = 1; a[2] = 2; int* p = a + 1; ++* p; }));
	//ASSERT(0, ({ int a[3]; a[0] = 0; a[1] = 1; a[2] = 2; int* p = a + 1; --* p; }));
	//
	//ASSERT(1, ({ int a[3]; a[0] = 0; a[1] = 1; a[2] = 2; int* p = a + 1; *p++; }));
	//ASSERT(1, ({ int a[3]; a[0] = 0; a[1] = 1; a[2] = 2; int* p = a + 1; *p--; }));
	//
	//ASSERT(0, ({ int a[3]; a[0] = 0; a[1] = 1; a[2] = 2; int* p = a + 1; (*p++)--; a[0]; }));
	//ASSERT(0, ({ int a[3]; a[0] = 0; a[1] = 1; a[2] = 2; int* p = a + 1; (*(p--))--; a[1]; }));
	//ASSERT(2, ({ int a[3]; a[0] = 0; a[1] = 1; a[2] = 2; int* p = a + 1; (*p)--; a[2]; }));
	//ASSERT(2, ({ int a[3]; a[0] = 0; a[1] = 1; a[2] = 2; int* p = a + 1; (*p)--; p++; *p; }));
	//
	//ASSERT(0, ({ int a[3]; a[0] = 0; a[1] = 1; a[2] = 2; int* p = a + 1; (*p++)--; a[0]; }));
	//ASSERT(0, ({ int a[3]; a[0] = 0; a[1] = 1; a[2] = 2; int* p = a + 1; (*p++)--; a[1]; }));
	//ASSERT(2, ({ int a[3]; a[0] = 0; a[1] = 1; a[2] = 2; int* p = a + 1; (*p++)--; a[2]; }));
	//ASSERT(2, ({ int a[3]; a[0] = 0; a[1] = 1; a[2] = 2; int* p = a + 1; (*p++)--; *p; }));
}

TEST(Control, If)
{

}
TEST(Control, Goto)
{

}
TEST(Control, TernaryOperator)
{

}

TEST(Iteration, For)
{

}
TEST(Iteration, While)
{

}

TEST(Declaration, Variable)
{

}
TEST(Declaration, Function)
{

}
TEST(Declaration, Typedef)
{
	EXPECT_EQ(LUCC(typedef int t; int main(void) { t x = 12; return x; }), 12);
	EXPECT_EQ(LUCC_EX(typedef int t; t x = 12; return x;), 12);
}

TEST(Initialization, LocalVariables)
{

}
TEST(Initialization, GlobalVariables)
{

}
TEST(Initialization, LocalArrays)
{

}
TEST(Initialization, GlobalArrays)
{

}
TEST(Initialization, LocalPointers)
{

}
TEST(Initialization, GlobalPointers)
{

}

TEST(Postprocessor, Macros)
{

}
TEST(Postprocessor, Includes)
{

}
TEST(Postprocessor, Directives)
{

}

TEST(Storage, Static)
{

}
TEST(Storage, Extern)
{

}
TEST(Storage, TLS)
{

}

TEST(Misc, Alignof)
{

}
TEST(Misc, Sizeof)
{

}
TEST(Misc, Atomic)
{

}
TEST(Misc, Cast)
{

}
TEST(Misc, Conversion)
{

}
TEST(Misc, Const)
{

}
TEST(Misc, Constexpr)
{

}
TEST(Misc, Enum)
{

}
