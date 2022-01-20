void transposta (int N, float m[N][N]) {
    int i,j;
    float temp;
    for(i = 0; i < N; i++) {
        for(j = 0; j < i; j++) {
            temp = m[i][j];
            m[i][j] = m[j][i];
            m[j][i] = temp;
        }
    }
}