#include "pch.h"
#include "TestMacros.h"
#include "../Lucc/Compiler/Compiler.h"
#include "../Lucc/Frontend/Diagnostics.h"

using namespace lucc;
using namespace diag;

TEST(Arithmetic, AdditiveMultiplicativeOperators)
{
	EXPECT_EQ(LUCC_EX(return 0;), 0);
	EXPECT_EQ(LUCC_EX(return 42;), 42);
	EXPECT_EQ(LUCC_EX(return 5 + 20 - 4;), 21);
	EXPECT_EQ(LUCC_EX(return 12 + 34 - 5;), 41);
	EXPECT_EQ(LUCC_EX(return 5 + 6 * 7;), 47);
	EXPECT_EQ(LUCC_EX(return 5 * (9 - 6);), 15);
	EXPECT_EQ(LUCC_EX(return -10 + 20;), 10);
	EXPECT_EQ(LUCC_EX(return (3 + 5) / 2;), 4);
	EXPECT_EQ(LUCC_EX(int i = 2; i += 5; return i;), 7);
	EXPECT_EQ(LUCC_EX(int i = 5; i -= 7; return i;), -2);
	EXPECT_EQ(LUCC_EX(int i = 5; i *= 4; return i - 15;), 5);
	EXPECT_EQ(LUCC_EX(int i = 7; i /= 3; return i;), 2);
}
TEST(Arithmetic, RelationOperations) 
{
	EXPECT_EQ(LUCC_EX(return 42 == 42;), 1);
	EXPECT_EQ(LUCC_EX(return 0 != 1;), 1);
	EXPECT_EQ(LUCC_EX(return 42 != 42;), 0);
	EXPECT_EQ(LUCC_EX(return 0 < 1;), 1);
	EXPECT_EQ(LUCC_EX(return 1 < 1;), 0);
	EXPECT_EQ(LUCC_EX(return 2 < 1;), 0);
	EXPECT_EQ(LUCC_EX(return 0 <= 1;), 1);
	EXPECT_EQ(LUCC_EX(return 1 <= 1;), 1);
	EXPECT_EQ(LUCC_EX(return 2 <= 1;), 0);
	EXPECT_EQ(LUCC_EX(return 1 > 0;), 1);
	EXPECT_EQ(LUCC_EX(return 1 > 1;), 0);
	EXPECT_EQ(LUCC_EX(return 1 > 2;), 0);
	EXPECT_EQ(LUCC_EX(return 1 >= 0;), 1);
	EXPECT_EQ(LUCC_EX(return 1 >= 1;), 1);
	EXPECT_EQ(LUCC_EX(return 1 >= 2;), 0);
}
TEST(Arithmetic, ShiftOperators)
{
	EXPECT_EQ(LUCC_EX(return 1 << 0;), 1);
	EXPECT_EQ(LUCC_EX(return 1 << 3;), 8);
	EXPECT_EQ(LUCC_EX(return 5 << 1;), 10);
	EXPECT_EQ(LUCC_EX(return 5 >> 1;), 2);
	EXPECT_EQ(LUCC_EX(return 5 >> 1;), 2);
	EXPECT_EQ(LUCC_EX(int i = 1; i <<= 0; return i;), 1);
	EXPECT_EQ(LUCC_EX(int i = 1; i <<= 3; return i;), 8);
	EXPECT_EQ(LUCC_EX(int i = 5; i <<= 1; return i;), 10);
	EXPECT_EQ(LUCC_EX(int i = 5; i >>= 1; return i;), 2);
	EXPECT_EQ(LUCC_EX(int i = -1; i >>= 1; return i;), -1);
}
TEST(Arithmetic, BitOperators)
{
	EXPECT_EQ(LUCC_EX(return ~-1;), 0);
	
	//#todo add x86-64 and, or, xor implementation
	//EXPECT_EQ(LUCC_EX(return 0 & 1;), 0);
	//ASSERT(0, 0 & 1);
	//ASSERT(1, 3 & 1);
	//ASSERT(3, 7 & 3);
	//ASSERT(10, -1 & 10);
	//
	//ASSERT(1, 0 | 1);
	//
	//ASSERT(0, 0 ^ 0);
	//ASSERT(2, ({ int i = 6; i &= 3; i; }));
	//ASSERT(7, ({ int i = 6; i |= 3; i; }));
	//ASSERT(10, ({ int i = 15; i ^= 5; i; }));
}
TEST(Arithmetic, PostIncrementDecrement)
{
	EXPECT_EQ(LUCC_EX(int i = 2; i++; return i;), 3);
	EXPECT_EQ(LUCC_EX(int i = 2; ++i; return i;), 3);
	EXPECT_EQ(LUCC_EX(int i = 2; i--; return i;), 1);
	EXPECT_EQ(LUCC_EX(int i = 2; --i; return i;), 1);
	EXPECT_EQ(LUCC_EX(int i = 2; int j = ++i; return j;), 3);
	EXPECT_EQ(LUCC_EX(int i = 2; int j = i++; return j;), 2);
	EXPECT_EQ(LUCC_EX(int i = 2; int j = --i; return j;), 1);
	EXPECT_EQ(LUCC_EX(int i = 2; int j = i--; return j;), 2);
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
	EXPECT_EQ(LUCC_EX(int x; if (0) x = 2; else x = 3; return x;), 3);
	EXPECT_EQ(LUCC_EX(int x; if (1 - 1) x = 2; else x = 3; return x;), 3);
	EXPECT_EQ(LUCC_EX(int x; if (1) x = 2; else x = 3; return x;), 2);
	EXPECT_EQ(LUCC_EX(int x; if (2 - 1) x = 2; else x = 3; return x;), 2);
}
TEST(Control, Switch)
{
	//ASSERT(5, ({ int i = 0; switch (0) { case 0:i = 5; break; case 1:i = 6; break; case 2:i = 7; break; } i; }));
	//ASSERT(6, ({ int i = 0; switch (1) { case 0:i = 5; break; case 1:i = 6; break; case 2:i = 7; break; } i; }));
	//ASSERT(7, ({ int i = 0; switch (2) { case 0:i = 5; break; case 1:i = 6; break; case 2:i = 7; break; } i; }));
	//ASSERT(0, ({ int i = 0; switch (3) { case 0:i = 5; break; case 1:i = 6; break; case 2:i = 7; break; } i; }));
	//ASSERT(5, ({ int i = 0; switch (0) { case 0:i = 5; break; default:i = 7; } i; }));
	//ASSERT(7, ({ int i = 0; switch (1) { case 0:i = 5; break; default:i = 7; } i; }));
	//ASSERT(2, ({ int i = 0; switch (1) { case 0: 0; case 1: 0; case 2: 0; i = 2; } i; }));
	//ASSERT(0, ({ int i = 0; switch (3) { case 0: 0; case 1: 0; case 2: 0; i = 2; } i; }));
}
TEST(Control, Goto)
{
	//ASSERT(3, ({ int i = 0; goto a; a: i++; b: i++; c: i++; i; }));
	//ASSERT(2, ({ int i = 0; goto e; d: i++; e: i++; f: i++; i; }));
	//ASSERT(1, ({ int i = 0; goto i; g: i++; h: i++; i: i++; i; }));
}
TEST(Control, TernaryOperator)
{

}

TEST(Iteration, For)
{
	EXPECT_EQ(LUCC_EX(int i = 0; int j = 0; for (i = 0; i <= 10; i = i + 1) j = i + j; return j;), 55);
	//ASSERT(55, ({ int j = 0; for (int i = 0; i <= 10; i = i + 1) j = j + i; j; }));
	//ASSERT(3, ({ int i = 3; int j = 0; for (int i = 0; i <= 10; i = i + 1) j = j + i; i; }));
	//ASSERT(3, ({ int i = 0; for (; i < 10; i++) { if (i == 3) break; } i; }));
	//ASSERT(3, ({ int i = 0; for (; i < 10; i++) { for (;;) break; if (i == 3) break; } i; }));
	//ASSERT(10, ({ int i = 0; int j = 0; for (; i < 10; i++) { if (i > 5) continue; j++; } i; }));
	//ASSERT(6, ({ int i = 0; int j = 0; for (; i < 10; i++) { if (i > 5) continue; j++; } j; }));
	//ASSERT(10, ({ int i = 0; int j = 0; for (; !i;) { for (; j != 10; j++) continue; break; } j; }));
}
TEST(Iteration, While)
{
	EXPECT_EQ(LUCC_EX(int i = 0; while (i < 10) i = i + 1; return i;), 10);
	//ASSERT(10, ({ int i = 0; while (i < 10) i = i + 1; i; }));
	//ASSERT(55, ({ int i = 0; int j = 0; while (i <= 10) { j = i + j; i = i + 1; } j; }));
	//ASSERT(4, ({ int i = 0; while (1) { if (i++ == 3) break; } i; }));
	//ASSERT(4, ({ int i = 0; while (1) { while (1) break; if (i++ == 3) break; } i; }));
	//ASSERT(11, ({ int i = 0; int j = 0; while (i++ < 10) { if (i > 5) continue; j++; } i; }));
	//ASSERT(5, ({ int i = 0; int j = 0; while (i++ < 10) { if (i > 5) continue; j++; } j; }));
	//ASSERT(11, ({ int i = 0; int j = 0; while (!i) { while (j++ != 10) continue; break; } j; }));
}
TEST(Iteration, DoWhile)
{
	//ASSERT(7, ({ int i = 0; int j = 0; do { j++; } while (i++ < 6); j; }));
	//ASSERT(4, ({ int i = 0; int j = 0; int k = 0; do { if (++j > 3) break; continue; k++; } while (1); j; }));
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
