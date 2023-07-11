

void f(int* p)
{
	*p = 5;
}

int main(void) 
{
	int a = 1;
	f(&a);
	return a;
}