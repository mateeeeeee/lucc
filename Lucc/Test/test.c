
int f(void)
{
	return 10;
}

int main(void)
{
	int (*pf)(void) = f;
	return pf();
}