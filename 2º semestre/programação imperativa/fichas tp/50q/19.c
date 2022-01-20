//isto da mal

int maiorPrefixo (char s1 [], char s2 []) {
    int i;
    for(i=0;s1[i]&&s2[i]&&s1[i]==s2[i];i++);
    return i;
}

int sufPref (char s1[], char s2[]) {
    int i,r,x;
    r=0;
    for(i=0;s1[i];i++){
        x=maiorPrefixo(s1+i,s2);
        if(s1[1+x] && x>r) r=x;
    }
    return r;
}