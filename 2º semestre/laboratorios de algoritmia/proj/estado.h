/**
estado.h
Defini��o do estado i.e. tabuleiro. Representa��o matricial do tabuleiro.
*/


// defini��o de valores possiveis no tabuleiro
typedef enum {VAZIA, VALOR_X, VALOR_O} VALOR;

/**
Estrutura que armazena o estado do jogo
*/
typedef struct estado {
    VALOR peca; // pe�a do jogador que vai jogar!
    VALOR grelha[8][8];
    char modo; // modo em que se est� a jogar! 0-> manual, 1-> contra computador
} ESTADO;


void printa(ESTADO);
