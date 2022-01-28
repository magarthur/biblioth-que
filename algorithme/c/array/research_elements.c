bool dichotomie( int t[], int e, int i , int j ) {
    bool resultat = false;
    if(i>j){
        return resultat;
    }
    else {
        int m = (i+j)/2;
        if (t[m] == e){
            resultat = true;
        }
        else if (t[m] > e) {
            return dichotomie (t, e, i, m-1);
        }
        else {
            return dichotomie (t, e, m+1, j);
            }  
    }
    return resultat;
