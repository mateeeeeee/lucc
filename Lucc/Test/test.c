

int main(void)
{
	int a[3];
	a[0] = 0;
	a[1] = 1;
	a[2] = 2;
	int* p = a + 1;
	++* p;
	return *p;
}