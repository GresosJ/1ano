#include "posicao.h"
#include <stdio.h>


Posicao posFinal (Posicao inicial, Movimento mov[], int N){
    int i;
    for(i = 0; i < N; i++) {
        Movimento x = mov[i];
        if(x == Norte) inicial.y++;
        if(x == Este) inicial.x++;
        if(x == Sul) inicial.y--;
        if(x == Oeste) inicial.x--;
    }
    return inicial;
}