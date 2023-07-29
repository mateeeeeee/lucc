
extern int printf(char const* fmt, ...);

struct S
{
	int a;
	short b;
};

int main(void) 
{ 
	struct S s;
	return s.a;
}