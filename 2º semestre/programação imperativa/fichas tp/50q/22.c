int contida (char a[], char b[]) {
    int i,j;
    int p = 1;
    for(i = 0; a[i] && p == 1 ; i++) {
        p=0;
        for(j = 0; b[j]; j++) {
            if(a[i] == b[j]) p = 1;
        }

    }
    return p;
}