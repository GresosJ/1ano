int comuns (int a[], int na, int b[], int nb){
    int i=0,j=0,k=0,be=0;
    for(i=0;i<na;i++){
        be=0;
        for(j=0;j<nb;j++){if (a[i]==b[j]) be=1;}
        if(be) k++;
    }
    return k;
}