
extern int puts(char const*);
extern int printf(char const*, ...);


int main(void)
{
	short a = -1;
	unsigned int b = (unsigned int)a;
	return b;
}