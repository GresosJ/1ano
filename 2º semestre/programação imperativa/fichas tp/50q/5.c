int trailingZ (unsigned int n) {
    int r=0;
    while(n>0) {
        if (n % 2 == 0) {r = r + 1;
                         n = n / 2;
        }
        else n= 0;
    }
    return r;
}

