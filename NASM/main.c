#include <stdio.h>

extern int factorial(int);

int main(void)
{
	printf("factorial of 5: %d", factorial(5));
}