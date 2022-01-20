int intersectSet (int N, int v1[N], int v2[N], int r[N]){
    int c=0,i;
    for(i=0;i<N;i++){
        r[i]= v1[i] && v2[i];
        c += r[i];
    }
    return c;
}