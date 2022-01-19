#include <stdio.h>
//1.1
void exercicio1 ()  {
	int x, y;
x = 3; y = x+1;
x = x*y; y = x + y;
printf ("%d %d\n", x, y);
}

int main () {
exercicio1 ();
	return 0;
}