int maiorSufixo (char s1 [], char s2 []) {
    int i,j,r=0;
    for(i=0;s1[i];i++);
    for(j=0;s2[j];j++);
    for(;s1[i]==s2[j];i--,j--){
        r++;
    }
    return r-1;
}