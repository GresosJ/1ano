#include <ctype.h>
int contaPal (char s[]) {
    int i,c=0;
    for(i=0;s[i];i++){
        if (!isspace(s[i]) && (isspace(s[i+1]) || s[i+1]=='\0')) c++;
    }
    return c;
}