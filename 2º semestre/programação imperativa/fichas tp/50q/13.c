#include <ctype.h>
void removeI(char s[], int n) {
    int j;
    for(j = n; s[j]; j++) {
        s[j] = s[j + 1];
    }
}
void truncW (char t[], int n) {
    int r = 0,i = 0;
    while(t[i] != '\0') {
        if(isspace(t[i])) {i++; r = 0;}
        else {
            if(r >= n) removeI(t, i);
            else {i++; r++;}
        }
    }
}