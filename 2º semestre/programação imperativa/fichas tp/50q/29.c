int retiraNeg (int v[], int N) {
    int i = 0,j;
    while(i < N) {
        if(v[i] < 0) {for(j = i; j < N; j++)
                v[j] = v[j + 1];
            N--;
        }
        else i++;
    }
    return N;
}