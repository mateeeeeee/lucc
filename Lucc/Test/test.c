
extern int puts(char const*);
extern int printf(char const*, ...);

int f(unsigned short a, int c)
{
	return c - a;
}

int main(void)
{
	int b = 5;
	short c = -5;
	return f(c, b);
}