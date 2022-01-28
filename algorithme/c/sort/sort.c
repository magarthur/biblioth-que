void swap(int* tab, int i, int j){
    int temp = tab[i];
    tab[i] = tab[j];
    tab[j] = temp;
}


void tri_bulle(int* tab, int n) {
    for(int i = 0; i<= n-1;i++){
        for(int j = 0; j<= n-2;j++){
            if(tab[j]>tab[j+1]){
                swap(tab,j,j+1);
                }
        }
    }
}
