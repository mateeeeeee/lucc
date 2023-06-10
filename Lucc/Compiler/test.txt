
int f(int a, int b)
{
	return b - a;
}

int main(void)
{
	int final[4];
	for(int j = 0; j < 16; ++j) final[0] = final[0] + f(j, j + 1);
	return final[0];
}