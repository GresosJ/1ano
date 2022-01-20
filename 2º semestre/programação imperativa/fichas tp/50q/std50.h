//
// Created by User on 06/03/2019.
//
#ifndef INC_50Q_STD50_H
#define INC_50Q_STD50_H

int maiorElem (int v[]);
float media (int v[]);
int segmaior (int v[]);
void swap (int v[],int x,int y);
int bitsUm (unsigned int n);
int trailingZ (unsigned int n);
int qDig (unsigned int n);
char *mystrcat(char s1[], char s2[]);
char *mystrcpy(char s1[],const char s2[]);
int mystrcmp(char s1[], char s2[]);
char *mystrstr (char s1[], char s2[]);
void strrev (char s[]);
void truncW (char t[], int n);
char charMaisfreq (char s[]);
int iguaisConsecutivos (char s[]);
int difConsecutivos(char s[]);
int maiorPrefixo (char s1 [], char s2 []);
int maiorSufixo (char s1 [], char s2 []);
int sufPref (char s1[], char s2[]);
int contaPal (char s[]);
int contaVogais (char s[]);
int contida (char a[], char b[]);
int palindroma (char s[]);
int remRep (char texto[]);
int limpaEspacos (char t[]);
void insere (int s[], int N, int x);
void merge (int r [], int a[], int b[], int na, int nb);
int crescente (int a[], int i, int j);
int retiraNeg (int v[], int N);
int menosFreq (int v[], int N);
int maisFreq (int v[], int N);
int maxCresc (int v[], int N);
int elimRep (int v[], int N);
int elimRepOrd (int v[], int N);
int comunsOrd (int a[], int na, int b[], int nb);
int comuns (int a[], int na, int b[], int nb);
int minInd (int v[], int n);
void somasAc (int v[], int Ac [], int N);
int triSup (int N, float m [N][N]);
void transposta (int N, float m[N][N]);
void addTo(int N, int M, int a [N][M], int b[N][M]);
int unionSet (int N, int v1[N], int v2[N], int r[N]);
int intersectSet (int N, int v1[N], int v2[N], int r[N]);
int intersectMSet (int N, int v1[N], int v2[N], int r[N]);
int cardinalMSet (int N, int v[N]);

#endif //INC_50Q_STD50_H