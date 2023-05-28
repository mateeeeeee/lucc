#include <stdio.h>

extern int f();
extern int number;

int main(void)
{
	f();
	printf("Result: %d", number);
}