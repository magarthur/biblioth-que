struct darray {
    int* t; // tableau utilisé pour stocker les éléments
    int size; // nombre d'éléments
    int capacity; // taille de t
};

typedef struct darray darray;


darray* empty() {
    darray* d = (darray*)malloc(sizeof(darray));
    d->t = NULL;
    d->size = 0;
    d->capacity = 0;
    return d;
}


void print_darray(darray* d) {
    for(int i = 0; i < d->size; i++)
        printf("%d", d->t[i]);
}


void copy(int* t1, int* t2, int n) {
    for(int i = 0; i < n; i++)
        t2[i] = t1[i];
}


void add(darray* d, int e) {
    if(d->size == d->capacity) {
        int* tmp = d->t; // conserve a->t pour pouvoir le free ensuite
        if (d->capacity == 0){
            d->capacity = 1;
            }
        else{
        d->capacity *= 2;
        }
        d->t = (int*)malloc(sizeof(int)*d->capacity);
        copy(tmp, d->t, d->size);
        free(tmp);
    }
    d->t[d->size] = e;
    d->size++;
}


int del(darray* d){
    d->size = d->size -1;
    return d->t[d->size];
}




