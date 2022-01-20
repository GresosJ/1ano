int comunsOrd (int a[], int na, int b[], int nb){
    int i=0,j=0,k=0;
    while(i<na && j<nb){

        if (a[i]==b[j]) {k++;i++;j++;}
        else if(a[i]<b[j]) i++;
        else j++;
    }
    return k;
}