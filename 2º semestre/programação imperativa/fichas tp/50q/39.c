int triSup (int N, float m [N][N]) {
    int x = 1,i,j;
    for(i = 0; i < N; i++) {
        for(j = 0; j < i; j++) {
            if(m[i][j]) x = 0;
        }
    }
    return x;
}