
extern int puts(char const*);

extern int atoi(const char* str);

int main(void)
{
	char const* str = "abc";
	puts(str);
	int i = 7; i /= 3; return i;
}