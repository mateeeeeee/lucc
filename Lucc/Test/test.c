
extern int puts(char const*);
extern int printf(char const*, ...);

enum color
{
	Red,
	Green = 10, 
	Blue
} c;

typedef enum color color_t;

int main(void)
{
	c = Green;
	int a = c;
	return sizeof(c);
}