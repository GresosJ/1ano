#include <math.h>
#include "posicao.h"

double medir (int x1,int y1,int x2,int y2){
    double i;
    int a=x1-x2,b = y1-y2;
    i=sqrt(pow(a, 2)+pow(b, 2));
    return i;
}

int maiscentral (Posicao pos[], int N) {
    int i,j=0;
    double primeiro = (medir(pos[0].x,pos[0].y,0,0));
    for(i=1;i<N;i++){
        if (medir(pos[i].x,pos[i].y,0,0) < primeiro) {j = i;
            primeiro = medir(pos[i].x,pos[i].y,0,0);
        }
    }
    return j;
}