int menosFreq (int v[], int N) {
    int f = 1, m = N, a = v[0], i;
    for(i = 1; i < N; i++) {
        if(v[i] == v[i - 1]) f++;
        if(v[i] != v[i - 1]) {
            if(f < m) {
                m = f;
                a = v[i - 1];
            }
            f = 1;
        }
    }
    if(f < m) {
        m = f;
        a = v[i - 1];
    }
    return a;
}