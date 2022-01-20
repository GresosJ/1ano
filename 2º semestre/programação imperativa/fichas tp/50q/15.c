int iguaisConsecutivos (char s[]) {
    int c = 0, m = 0,i;
    for(i = 0; s[i]; i++) {
        if(s[i] == s[i - 1]) c++;
        else {
            if(c > m) m = c;
            c = 1;
        }
    }
    if(c > m) m = c;
    return m;
}