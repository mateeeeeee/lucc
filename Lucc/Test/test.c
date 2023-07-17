
extern int puts(char const*);

_Alignas(16) char x, y;

int main(void)
{
	return &y - &x;
}