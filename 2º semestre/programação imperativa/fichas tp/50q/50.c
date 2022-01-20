#include "posicao.h"
#include <math.h>

int medir (int x1,int y1,int x2,int y2){
    int i;
    int a=(x1-x2),b = (y1-y2);
    i=sqrt(pow(a,2)+pow(b,2));
    return i;
}

int vizinhos (Posicao p, Posicao pos[], int N) {
    int i,j=0;
    for(i=0;i<N;i++){
        if(medir(pos[i].x,pos[i].y,p.x,p.y) == 1) j+=1;
    }
    return j;
}