int prefDif (char t[]){
    int i,j;
    for(i=0;t[i];i++){
        for(j=0;j<i && t[i] != t[j];j++);
        if(i!=j) break;
    }
    return i;
}

int difConsecutivos(char s[]) {
    int i,r=0,x;
    for(i=0;s[i];i++){
        x=prefDif(s+i);
        if(x>r) r= x;
    }
    return r;
}