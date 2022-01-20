#include <stdio.h>
float media (int v[]) {
    float g;
    int i, r=0,j,m=0;
    for (i = 0; i < 50; i++) {
        scanf(" %d", &v[i]);
        if (v[i] == 0) {r = i;
            i +=50;
        }
        }
    for(j=0;j<r;j++){
        m += v[j];
    }
    g= (float) m/r;
    return g;
}