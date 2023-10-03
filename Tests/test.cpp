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
	EXPECT_EQ(LUCC_EX(int i = 1; i <<= 0; return i;), 1);
	EXPECT_EQ(LUCC_EX(int i = 1; i <<= 3; return i;), 8);
	EXPECT_EQ(LUCC_EX(int i = 5; i <<= 1; return i;), 10);
	EXPECT_EQ(LUCC_EX(int i = 5; i >>= 1; return i;), 2);
	EXPECT_EQ(LUCC_EX(int i = -1; i >>= 1; return i;), -1);
}
TEST(Arithmetic, Comma)
{
	EXPECT_EQ(LUCC_EX(return (1,3);), 3);
}
TEST(Arithmetic, BitOperators)
{
	EXPECT_EQ(LUCC_EX(return ~-1;), 0);
	EXPECT_EQ(LUCC_EX(return 0 & 1;), 0);
	EXPECT_EQ(LUCC_EX(return 3 & 1;), 1);
	EXPECT_EQ(LUCC_EX(return 3 & 7;), 3);
	EXPECT_EQ(LUCC_EX(return 10 & -1;), 10);
	EXPECT_EQ(LUCC_EX(return 0 | 1;), 1);
	EXPECT_EQ(LUCC_EX(return 0 ^ 0;), 0);
	EXPECT_EQ(LUCC_EX(int i = 6; i &= 3; return i;), 2);
	EXPECT_EQ(LUCC_EX(int i = 6; i |= 3; return i;), 7);
	EXPECT_EQ(LUCC_EX(int i = 15; i ^= 5; return i;), 10);
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
	EXPECT_EQ(LUCC_EX(int x = 3; return *&x;), 3);
	EXPECT_EQ(LUCC_EX(int x = 3; int* y = &x; int** z = &y; return **z;), 3);
	EXPECT_EQ(LUCC_EX(int x = 3; int* y = &x; *y = 5; return x;), 5);
	EXPECT_EQ(LUCC_EX(int x = 3; int y = 5; *(&x + 1) = 7; return y;), 7);
	EXPECT_EQ(LUCC_EX(int x = 3; int y = 5; *(&y - 2 + 1) = 7; return x;), 7);

	EXPECT_EQ(LUCC_EX(int x = 3; return (&x + 2) - &x + 3;), 5);
	EXPECT_EQ(LUCC_EX(int x; int* p = &x; return p + 20 - p;), 20);
	EXPECT_EQ(LUCC_EX(int x; int* p = &x; return p + 20 - p > 0;), 1);
	EXPECT_EQ(LUCC_EX(int x; int* p = &x; return p - 20 - p;), -20);
	EXPECT_EQ(LUCC_EX(int x; int* p = &x; return p - 20 - p < 0;), 1);
}
TEST(Arithmetic, Arrays)
{
	EXPECT_EQ(LUCC_EX(int a[3]; a[0] = 0; a[1] = 1; a[2] = 2; int* p = a + 1; return ++*p;), 2);
	//EXPECT_EQ(LUCC_EX(int a[3]; a[0] = 0; a[1] = 1; a[2] = 2; int* p = a + 1; return *p++;), 1);
	EXPECT_EQ(LUCC_EX(int a[3]; a[0] = 0; a[1] = 1; a[2] = 2; int* p = a + 1; return --*p;), 0);
	//EXPECT_EQ(LUCC_EX(int a[3]; a[0] = 0; a[1] = 1; a[2] = 2; int* p = a + 1; return *p--;), 1);
	EXPECT_EQ(LUCC_EX(int a[3]; a[0] = 0; a[1] = 1; a[2] = 2; int* p = a + 1; (*p)--; return a[2];), 2);
	EXPECT_EQ(LUCC_EX(int a[3]; a[0] = 0; a[1] = 1; a[2] = 2; int* p = a + 1; (*p++)--; return a[0];), 0);
	//EXPECT_EQ(LUCC_EX(int a[3]; a[0] = 0; a[1] = 1; a[2] = 2; int* p = a + 1; (*(p--))--; return a[1];), 0);

	//EXPECT_EQ(LUCC_EX(int a[3]; a[0] = 0; a[1] = 1; a[2] = 2; int* p = a + 1; (*p)--; p++; return *p;), 2);
	//EXPECT_EQ(LUCC_EX(int a[3]; a[0] = 0; a[1] = 1; a[2] = 2; int* p = a + 1; (*p++)--; return a[1];), 0);
	EXPECT_EQ(LUCC_EX(int a[3]; a[0] = 0; a[1] = 1; a[2] = 2; int* p = a + 1; (*p++)--; return a[2];), 2);
	//EXPECT_EQ(LUCC_EX(int a[3]; a[0] = 0; a[1] = 1; a[2] = 2; int* p = a + 1; (*p++)--; return *p;), 2);
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
	EXPECT_EQ(LUCC_EX(int i = 0; switch (0) { case 0:i = 5; break; case 1:i = 6; break; case 2:i = 7; break; } return i;), 5);
	EXPECT_EQ(LUCC_EX(int i = 0; switch (1) { case 0:i = 5; break; case 1:i = 6; break; case 2:i = 7; break; } return i;), 6);
	EXPECT_EQ(LUCC_EX(int i = 0; switch (2) { case 0:i = 5; break; case 1:i = 6; break; case 2:i = 7; break; } return i;), 7);
	EXPECT_EQ(LUCC_EX(int i = 0; switch (3) { case 0:i = 5; break; case 1:i = 6; break; case 2:i = 7; break; } return i;), 0);
	EXPECT_EQ(LUCC_EX(int i = 0; switch (3) { case 0:i = 5; break; case 1:i = 6; break; case 2:i = 7; break; default: i = 8; } return i;), 8);
	EXPECT_EQ(LUCC_EX(int i = 0; switch (0) { case 0:i = 5; break; default:i = 7; } return i;), 5);
	EXPECT_EQ(LUCC_EX(int i = 0; switch (1) { case 0:i = 5; break; default:i = 7; } return i;), 7);
}
TEST(Control, Goto)
{
	EXPECT_EQ(LUCC_EX(int i = 0; goto a; a: i++; b: i++; c: i++; return i;), 3);
	EXPECT_EQ(LUCC_EX(int i = 0; goto e; d: i++; e: i++; f: i++; return i;), 2);
	EXPECT_EQ(LUCC_EX(int i = 0; goto i; g: i++; h: i++; i: i++; return i;), 1);
}
TEST(Control, TernaryOperator)
{
	EXPECT_EQ(LUCC_EX(int a = 14; return a > 10 ? a - 10 : a - 5;), 4);
	EXPECT_EQ(LUCC_EX(int a = 6; return a > 10 ? a - 10 : a - 5;), 1);
}

TEST(Iteration, For)
{
	EXPECT_EQ(LUCC_EX(int i = 0; int j = 0; for (i = 0; i <= 10; i = i + 1) j = i + j; return j;), 55);
	EXPECT_EQ(LUCC_EX(int i = 0; for (; i < 10; i++) { if (i == 3) break; } return i;), 3);
	EXPECT_EQ(LUCC_EX(int i = 0; for (; i < 10; i++) { for (;;) break; if (i == 3) break; } return i;), 3);
	EXPECT_EQ(LUCC_EX(int i = 0; int j = 0; for (; i < 10; i++) { if (i > 5) continue; j++; } return i;), 10);
	EXPECT_EQ(LUCC_EX(int i = 0; int j = 0; for (; i < 10; i++) { if (i > 5) continue; j++; } return j;), 6);
	EXPECT_EQ(LUCC_EX(int i = 0; int j = 0; for (; i==0;) { for (; j != 10; j++) continue; break; } return j;), 10);
}
TEST(Iteration, While)
{
	EXPECT_EQ(LUCC_EX(int i = 0; while (i < 10) i = i + 1; return i;), 10);
	EXPECT_EQ(LUCC_EX(int i = 0; int j = 0; while (i <= 10) { j = i + j; i = i + 1; } return j;), 55);
	EXPECT_EQ(LUCC_EX(int i = 0; while (1) { if (i++ == 3) break; } return i;), 4);
	EXPECT_EQ(LUCC_EX(int i = 0; while (1) { while (1) break; if (i++ == 3) break; } return i;), 4);
	EXPECT_EQ(LUCC_EX(int i = 0; int j = 0; while (i++ < 10) { if (i > 5) continue; j++; } return i;), 11);
	EXPECT_EQ(LUCC_EX(int i = 0; int j = 0; while (i++ < 10) { if (i > 5) continue; j++; } return j;), 5);
}
TEST(Iteration, DoWhile)
{
	EXPECT_EQ(LUCC_EX(int i = 0; int j = 0; do { j++; } while (i++ < 6); return j;), 7);
	EXPECT_EQ(LUCC_EX(int i = 0; int j = 0; int k = 0; do { if (++j > 3) break; continue; k++; } while (1); return j;), 4);
}

TEST(Declaration, Variable)
{
	EXPECT_EQ(LUCC(int t = 5; int main(void) { int t = 12; return t; }), 12);
	EXPECT_EQ(LUCC_EX(int a = 1; return a + 2;), 3);
	EXPECT_EQ(LUCC_EX(int a = 1; int b = 48 + 2; int c = a + b; return c * 2;), 102);
	EXPECT_EQ(LUCC_EX(int a[1]; a[0] = 55; int* b = a; return *b;), 55);
	EXPECT_EQ(LUCC_EX(int a[2]; a[0] = 55; a[1] = 67; int* b = a + 1; return *b;), 67);

}
TEST(Declaration, Function)
{
	EXPECT_EQ(LUCC(int f(int a) { return a; } int main(void) { int i = f(1); return i; }), 1);
	EXPECT_EQ(LUCC(int f(int a, int b) { return a + b; } int main(void) { int i = f(1, 2); return i; }), 3);
	EXPECT_EQ(LUCC(int f(int a, int b, int c) { return a + b + c; } int main(void) { int i = f(1, 2, 3); return i; }), 6);
	EXPECT_EQ(LUCC(int f(int a, int b, int c, int d) { return a + b + c + d; } int main(void) { int i = f(1, 2, 3, 4); return i; }), 10);
	EXPECT_EQ(LUCC(int f(int a, int b, int c, int d, int e) { return a + b + c + d + e; } int main(void) { int i = f(1, 2, 3, 4, 5); return i; }), 15);
	EXPECT_EQ(LUCC(int f(int a, int b, int c, int d, int e, int f) { return a + b + c + d + e + f; } int main(void) { int i = f(1, 2, 3, 4, 5, 6); return i; }), 21);
	EXPECT_EQ(LUCC(int f(int a, int b, int c, int d, int e, int f, int g) { return a + b + c + d + e + f + g; } int main(void) { int i = f(1, 2, 3, 4, 5, 6, 7); return i; }), 28);

	EXPECT_EQ(LUCC(int a = 5; int ptr_to_int(int* ptr) { return *ptr; } int main(void) { int* aptr = &a; return ptr_to_int(aptr); }), 5);
}
TEST(Declaration, Typedef)
{
	EXPECT_EQ(LUCC(typedef int t; int main(void) { t x = 12; return x; }), 12);
	EXPECT_EQ(LUCC_EX(typedef int t; t x = 12; return x;), 12);
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

TEST(Misc, Alignof)
{
	EXPECT_EQ(1, LUCC_EX(return _Alignof(char);));
	EXPECT_EQ(2, LUCC_EX(return _Alignof(short);));
	EXPECT_EQ(4, LUCC_EX(return _Alignof(int);));
	EXPECT_EQ(4, LUCC_EX(return _Alignof(long);));
	EXPECT_EQ(8, LUCC_EX(return _Alignof(long long);));
	EXPECT_EQ(1, LUCC_EX(return _Alignof(char[3]);));
	EXPECT_EQ(4, LUCC_EX(return _Alignof(int[3]);));

	EXPECT_EQ(1, LUCC_EX(_Alignas(char) char x, y; return &y - &x;));
	EXPECT_EQ(4, LUCC_EX(_Alignas(int) char x, y; return &y - &x;));
	EXPECT_EQ(16, LUCC_EX(_Alignas(16) char x, y; return &y - &x;));
	//EXPECT_EQ(32, LUCC_EX(_Alignas(32) int* x, *y; ((char*)&y) - ((char*)&x);));
}
TEST(Misc, Sizeof)
{
	EXPECT_EQ(1,  LUCC_EX(return sizeof(char);));
	EXPECT_EQ(2,  LUCC_EX(return sizeof(short);));
	EXPECT_EQ(2,  LUCC_EX(return sizeof(short int);));
	EXPECT_EQ(2,  LUCC_EX(return sizeof(int short);));
	EXPECT_EQ(4,  LUCC_EX(return sizeof(int);));
	EXPECT_EQ(4,  LUCC_EX(return sizeof(long);));
	EXPECT_EQ(4,  LUCC_EX(return sizeof(long int);));
	EXPECT_EQ(8,  LUCC_EX(return sizeof(long long);));
	EXPECT_EQ(8,  LUCC_EX(return sizeof(char*);));
	EXPECT_EQ(8,  LUCC_EX(return sizeof(int*);));
	EXPECT_EQ(8,  LUCC_EX(return sizeof(long*);));
	EXPECT_EQ(8,  LUCC_EX(return sizeof(int**);));
	EXPECT_EQ(8,  LUCC_EX(return sizeof(int(*)[4]);));
	EXPECT_EQ(8,  LUCC_EX(return sizeof(void(*)(void));));
	EXPECT_EQ(32, LUCC_EX(return sizeof(int* [4]);));
	EXPECT_EQ(16, LUCC_EX(return sizeof(int[4]);));
	EXPECT_EQ(48, LUCC_EX(return sizeof(int[3][4]);));
}
TEST(Misc, Const)
{
	EXPECT_EQ(LUCC(const int i = 0; int main(void) { i = 5; return i; }), EXIT_CODE_COMPILATION_FAILED);
	EXPECT_EQ(LUCC_EX(const int i = 0; i = 5; return i;), EXIT_CODE_COMPILATION_FAILED);
	EXPECT_EQ(LUCC_EX(int a = 5; int b = 10; int const* c = &a; c = &b; return *c;), 10);
	EXPECT_EQ(LUCC_EX(int a = 5; int b = 10; int* const c = &a; c = &b; return *c;), EXIT_CODE_COMPILATION_FAILED);

}
TEST(Misc, Constexpr)
{
	EXPECT_EQ(1, LUCC_EX(int i = 0; switch (3) { case 5 - 2 + 0 * 3: i++; } return i; ));
	EXPECT_EQ(8, LUCC_EX(int x[1 + 1]; return sizeof(x); ));
	EXPECT_EQ(6, LUCC_EX(char x[8 - 2]; return sizeof(x); ));
	EXPECT_EQ(6, LUCC_EX(char x[2 * 3]; return sizeof(x); ));
	EXPECT_EQ(3, LUCC_EX(char x[12 / 4]; return sizeof(x);));
	//EXPECT_EQ(2, ({ char x[12 % 10]; return sizeof(x); }));
	EXPECT_EQ(4, LUCC_EX(char x[1 << 2]; return sizeof(x);));
	EXPECT_EQ(2, LUCC_EX(char x[4 >> 1]; return sizeof(x);));
	EXPECT_EQ(2, LUCC_EX(char x[(1 == 1) + 1]; return sizeof(x);));
	EXPECT_EQ(1, LUCC_EX(char x[(1 != 1) + 1]; return sizeof(x);));
	EXPECT_EQ(1, LUCC_EX(char x[(1 < 1) + 1]; return sizeof(x);));
	EXPECT_EQ(2, LUCC_EX(char x[(1 <= 1) + 1]; return sizeof(x);));
	EXPECT_EQ(2, LUCC_EX(char x[1 ? 2 : 3]; return sizeof(x);));
	EXPECT_EQ(3, LUCC_EX(char x[0 ? 2 : 3]; return sizeof(x);));
	EXPECT_EQ(3, LUCC_EX(char x[(1,3)]; return sizeof(x); ));
	EXPECT_EQ(2, LUCC_EX(char x[!0 + 1]; return sizeof(x);));
	EXPECT_EQ(1, LUCC_EX(char x[!1 + 1]; return sizeof(x);));
	EXPECT_EQ(2, LUCC_EX(char x[~- 3]; return sizeof(x);));
	EXPECT_EQ(2, LUCC_EX(char x[(5 || 6) + 1]; return sizeof(x);));
	EXPECT_EQ(1, LUCC_EX(char x[(0 || 0) + 1]; return sizeof(x);));
	EXPECT_EQ(2, LUCC_EX(char x[(1 && 1) + 1]; return sizeof(x);));
	EXPECT_EQ(1, LUCC_EX(char x[(1 && 0) + 1]; return sizeof(x);));
}
TEST(Misc, Enum)
{
	EXPECT_EQ(0, LUCC_EX(enum { zero, one, two }; return zero;));
	EXPECT_EQ(1, LUCC_EX(enum { zero, one, two }; return one;));
	EXPECT_EQ(2, LUCC_EX(enum { zero, one, two }; return two;));
	EXPECT_EQ(5, LUCC_EX(enum { five = 5, six, seven }; return five;));
	EXPECT_EQ(6, LUCC_EX(enum { five = 5, six, seven }; return six; ));
	EXPECT_EQ(0, LUCC_EX(enum { zero, five = 5, three = 3, four }; return zero;));
	EXPECT_EQ(5, LUCC_EX(enum { zero, five = 5, three = 3, four }; return five;));
	EXPECT_EQ(3, LUCC_EX(enum { zero, five = 5, three = 3, four }; return three;));
	EXPECT_EQ(4, LUCC_EX(enum { zero, five = 5, three = 3, four }; return four;));
	EXPECT_EQ(4, LUCC_EX(enum { zero, one, two } x; return sizeof(x);));
	EXPECT_EQ(4, LUCC_EX(enum t { zero, one, two }; enum t y; return sizeof(y);));

}
TEST(Misc, Cast)
{
	EXPECT_EQ(EXIT_CODE_COMPILATION_FAILED, LUCC_EX(int a = 5; int* b = &a; int c = b; return 0; ));
	EXPECT_EQ(0, LUCC_EX(int a = 5; int* b = &a; int c = (int)b; return 0; ));
}

