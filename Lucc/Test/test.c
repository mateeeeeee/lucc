
#define PFN f

int f(void)
{
	return 10;
}

int main(void)
{
	int* a = NULL;
	if (a) return 10;
	else return 20;
}