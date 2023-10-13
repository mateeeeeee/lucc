

int f2(int a, int b)
{
	return a + b;
}

int f1(int a)
{
	return a + 1;
}

int f4(int a, int b, int c, int d)
{
	return a + b + c + d;
}

int f5(int a, int b, int c, int d, int e)
{
	return a + b + c + d + e;
}

int main(void)
{
	int i = 1, j = 2, k = 3, l = 4, m = 5;
	return f5(i, j, k, l, m);
}