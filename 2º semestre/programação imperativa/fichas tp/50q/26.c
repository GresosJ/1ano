void insere (int s[], int N, int x){
    int i,j;
    for(i=0;i<N;i++){
        if(s[i] > x) {for(j = N; j > i; j--) {
                s[j] = s[j - 1];}
            s[i]=x;
            break;
        }
    }
    if(i == N-1) x=s[i+1];
}