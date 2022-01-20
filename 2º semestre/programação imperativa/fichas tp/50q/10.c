#include <string.h>

char *mystrstr (char s1[], char s2[]) {
    int i = 0, j = 0, s = 0;
    int r = 1;
    while(s2[i] && s1[j]) {
        if(s1[j] == s2[i]) {
            if(!r) {
                r = 1;
                s = j;
            }
            i++;
        }
        else r = 0;
        j++;
    }
    if (r && !s2[i]) return &s1[s];
    else return NULL;
}