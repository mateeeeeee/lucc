
extern int puts(char const*);
extern int printf(char const*, ...);

int main(void)
{
	int a = 5; int* b = &a; int c = (int)b; return 0;
}