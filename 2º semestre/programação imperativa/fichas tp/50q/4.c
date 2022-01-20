int bitsUm (unsigned int n){
    int r=0;
    while(n>0){
        if(n%2==1){r=r+1;
                   n=n/2;}
        else (n=n/2);}
    return r;
}