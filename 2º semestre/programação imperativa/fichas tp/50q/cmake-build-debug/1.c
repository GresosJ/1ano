#include <stdio.h>
int maiorElem (int v[],int n) {
    int m,j;
    int i,r;
    for (i=0;i<50;i++){
        scanf (" %d",&v[i]);
        if (v[i] == 0) {r = i;
            i +=50;
        }
    }
    m = v[0];
    for (j=0;j<r;j++) {
        if (m < v[j]) m = v[j];
    }
    printf ("%d",m);
    return m;
}