int elimRep (int v[], int N) {
    int i=0,j,k;

    for(i=0;i<N;i++){

        for(j=i+1;j<N;j++){

            if(v[i] == v[j]) {
                for(k = j; k < N; k++)
                    v[k] = v[k + 1];
                N--;
                j--;
            }
        }
    }
    return N;
}