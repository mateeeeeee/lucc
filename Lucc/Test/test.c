
extern int printf(char const* fmt, ...);

struct s
{
	int a;
	short b;
	int c;
};

int main(void) 
{ 
	struct s s1;
	s1.a = 5;
	s1.b = -7;
	s1.c = 10;
	return s1.c + s1.b + s1.a;
}