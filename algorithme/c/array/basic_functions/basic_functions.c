int minimum(int* t, int n){
    int resultat = INT_MAX;
    for(int i = 0; i<=n-1;i++){
        if(t[i]< resultat){
            resultat = t[i];
        }
    }
    return resultat;
}


int maximum(int* t, int n){
    int resultat = INT_MIN;
    for(int i = 0; i<=n-1; i++){
        if(t[i]>resultat){
            resultat = t[i];
        }
    }
    return resultat;
}

void print_array(int* t, int n){
    for(int i=0; i<=n-1; i++){
        printf("%d, ", t[i]);
    }
}

bool doublon_array(int* t, int n){
    bool resultat = false;
    for(int i = 0; i<=n-2;i++){
        for(int j = i+1; j<=n-1;j++){
            if(t[i] == t[j]){
                resultat = true;
            }
        }
    }
    return resultat;
}

bool increase_array(int* t, int n){
    bool resultat = true;
    for(int i = 0; i < n-1; i++){
        if(t[i]>t[i+1]){
            resultat = false;
        }
    }
    return resultat;
}

bool equal(int* t1, int* t2, int n){
    bool result = true;
    for(int i = 0; i<n ; i++){
        if (t1[i]!=t2[i]){
            result = false;
        }
    }
    return result;
}
