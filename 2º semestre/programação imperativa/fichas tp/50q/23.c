int palindroma (char s[]) {
    int i,j,p = 1;
    for(i=0;s[i];i++);
    for(j=0;s[j];j++){
        if(s[j]!=s[i-1-j]) p = 0;
    }
    return p;
}