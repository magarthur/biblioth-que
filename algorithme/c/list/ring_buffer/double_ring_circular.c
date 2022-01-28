struct list {
    int elem;
    list* next;
    list* prev;
};
typedef struct list list;

list* create(int e){
    list* l =(list*)malloc(sizeof(list));
    l->elem =e;
    l->next =l;
    l->prev = l;
    return l;
}

list* add(list* l, int e){
    list* l_new = (list*)malloc(sizeof(list));
    l_new -> elem = e;
    l_new -> next = l->next;
    l_new -> prev = l;
    l->next->prev = l_new;
    l->next = l_new;
    return l_new;
}

void print_list_1(list* l){
    list* l_cur = l->next;
    printf("[");
    while(l_cur != l){
        printf("%d; ", l_cur -> elem);
        l_cur = l_cur->next;
    }
    printf("%d]", l_cur -> elem);

}

list* reverse(list* l) {
    list* res = create(l->elem);
    list* l_cur = l->prev;
    while(l_cur != l) {
        res = add(res, l_cur->elem);
        l_cur = l_cur->prev;
        
    }
    return res;
}


void print_list(list* l){
    return print_list_1(reverse(l));
}

bool appartient(list* l, int e){
    list* l_cur = l->next;
    while(l_cur != l){
        if (l_cur -> elem == e){
            return true;
        }
        l_cur = l_cur->next;
    }
    return l_cur -> elem == e;
}
    
list* del(list*l){
    l->next->prev = l->prev;
    l->prev->next = l->next;
    // mettre une variable
    return l->next;
}
