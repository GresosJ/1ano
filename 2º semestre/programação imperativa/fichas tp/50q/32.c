int maxCresc (int v[], int N) {
    int i=0,melhor=1,t=1;
    for(;i<N && v[i]<=v[i+1];i++){
        melhor++;
    }
    i++;

    for(;i<N;i++){
        if(v[i]<=v[i+1]) t++;
        else{ if (t>melhor) melhor= t;
            t=1;
        }
    }
    return melhor;
}