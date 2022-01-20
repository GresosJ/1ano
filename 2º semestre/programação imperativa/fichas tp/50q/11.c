void strrev (char s[]) {
    int l,i;
    for(l = 0; s[l]; l++);
    char S2[l];
    for(i = 0; i < l; i++) S2[i] = s[l - i - 1];
    for(i = 0; i < l; i++) s[i] = S2[i];
}