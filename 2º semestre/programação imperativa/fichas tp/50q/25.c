int limpaEspacos (char t[]) {
    int i = 0,j;
    int p = 0;
    while(t[i]) {
        if(t[i++] == ' ') {
            if(p) for(j = --i; t[j]; j++) t[j] = t[j + 1];
            else p = 1;
        }
        else p = 0;
    }
    return i;
}
