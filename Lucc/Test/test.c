
int f(void)
{
	return 10;
}

int g(void)
{
	return 20;
}



int main()
{
	int (*pf)(void) = NULL;
	return pf ? pf() : 6;
}