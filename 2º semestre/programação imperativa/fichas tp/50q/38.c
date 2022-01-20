void somasAc (int v[], int Ac [], int N){
    int i,j;
    for(i = 0; i < N; i++) {
        Ac[i] = 0;
        for(j = 0; j <= i; j++) {
            Ac[i] += v[j];
        }
    }
}