char charMaisfreq (char s[]) {
    int c[256]={0};
    int f = 0,i,p=0;

    for(i = 0; s[i] != '\0'; i++) {
        c[s[i]] += 1;
    }

    for (i=0 ; i<256 ; i++) {
        if (c[i]>f) { f = c[i]; p=i;}

    }
    return p;
}