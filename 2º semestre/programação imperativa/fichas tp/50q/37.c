int minInd (int v[], int n) {
    int i,j = v[0],k=0;
    for(i=0;i<n;i++){
        if(v[i]<j) {j=v[i]; k = i;}
    }
    return k;
}