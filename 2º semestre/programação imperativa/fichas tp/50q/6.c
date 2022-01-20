int qDig (unsigned int n){
    int k=1;
    while(n>10){
        n=n/10;k++;
    }
    return k;
}