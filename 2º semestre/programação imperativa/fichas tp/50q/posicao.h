#ifndef INC_50Q_POSICAO_H
#define INC_50Q_POSICAO_H

typedef enum movimento {Norte, Oeste, Sul, Este} Movimento;

typedef struct posicao {
    int x, y;
} Posicao;

Posicao posFinal (Posicao inicial, Movimento mov [], int N);
int caminho (Posicao, Posicao, Movimento [], int );
int maiscentral (Posicao pos[], int N);
int vizinhos (Posicao p, Posicao pos[], int N);

#endif //INC_50Q_POSICAO_H