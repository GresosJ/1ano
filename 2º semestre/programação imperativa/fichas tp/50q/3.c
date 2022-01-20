void swap (int v[],int x,int y){
    int t =v[x];
    v[x]=v[y];
    v[y]=t;
}

int segmaior (int v[]){
    int i, r,m,g;
    for (i = 0; i < 50; i++) {
        scanf(" %d", &v[i]);
        if (v[i] == 0) {r = i;
            i +=50;
        }
    }
   for (m=0;m<2;m++){
       for(g=0;g<r-1;g++){
           if (v[g]>v[g+1]) swap(v,g,g+1);
       }
   }
   return v[r-2];
}