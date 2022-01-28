typedef struct list {
    list* next;
    int elem;
}


list* add(list* l, int e) {
    list* l_new = (list*)malloc(sizeof(list));
    l_new->next = l;
    l_new->elem = e;
    return l_new;
}



list* range(int n) {
    list* res = NULL;
    for(int i = n - 1; i > -1; i--)
        res = add(res, i);
    return res;
}

void print_list(list* l) {
    while(l != NULL) {
        printf("%d ", l->elem);
        l = l->next;
    }
}


bool has(list* l, int e) { 
    if(l == NULL)
        return false;
    return l->elem == e || has(l->next, e);
}

unsigned size(list* l) {
    if(l == NULL)
        return 0;
    return 1 + size(l->next);
}


void free_list(list* l) {
    if(l != NULL) {
    list* next = l->next; 
    free(l);
    free_list(next);
    }
}


list* reverse(list* l) {
    list* res = NULL;
    while(l != NULL) {
        res = add(res, l->elem);
        l = l->next;
    }
    return res;
}
