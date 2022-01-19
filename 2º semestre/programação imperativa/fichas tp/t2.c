#include <stdio.h>
#include <math.h>


//1.
int maiorS() {
	int v[1026],i,m,j;
	printf("Introduza os numeros da sequencia\n")
	for (i = 0, i != 0, i++) {
		scanf("%d", &v[i]);
		}
		m = v[0];
		for (j = 1; j!=0; j++) {
			if (m < v[j]) m = v[j];
		}
		return m;
	}

int main() {
	maiors();
    return 0;
}