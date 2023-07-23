
int f(int)
{
	return 16;
}

int main()
{
	int (*pf)(int) = f;
	return pf(1);
}