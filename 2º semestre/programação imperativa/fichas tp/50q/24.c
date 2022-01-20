void removeI(char s[], int n) {
    int j;
    for(j = n; s[j]; j++) {
        s[j] = s[j + 1];
    }
}

int remRep (char texto[]) {
    if(!texto[0]) return 0;
    int i = 1;
    char p = texto[0];
    while(texto[i]) {
        if(texto[i] == p) removeI(texto,i);
        else p = texto[i++];
    }
    return i;
}