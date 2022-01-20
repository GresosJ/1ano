int crescente (int a[], int i, int j){
    int r=1;
    for(;i<j;i++){
        if(a[i]>a[i+1]) {r=0;
            break;}
    }
    return r;
}
