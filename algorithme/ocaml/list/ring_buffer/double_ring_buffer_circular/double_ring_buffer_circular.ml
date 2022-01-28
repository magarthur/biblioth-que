type 'a l2c = {elem : int; mutable prev : 'a l2c;mutable  next : 'a l2c}

(*[create_l2c e] create a l2C with one element, e *)
let create_l2c e =
    let rec l = {elem = e; prev = l ; next = l } in l;;
    
(* [add_l2c l e]  add e after l *)   
let add_l2c l e =
    let l_new = {elem = e; prev = l; next = l.next } in
    l.next.prev <- l_new;
    l.next <- l_new;;
    
(* suppr_l2c l° del l *)    
let suppr_l2c l =
    l.next.prev <- l.prev;
    l.prev.next <- l.next;;
    
(* [mem_l2c e l] returns if e belongs at l *)    
let mem_l2c e l =
    let cur = ref l.next in
    while !cur.elem <> e && !cur != l do
        cur := !cur.next
    done;
    !cur.elem = e;;
    
(*[length_l2c l] returns the length of l*)   
let length_l2c l = 
    let rec aux l1 =
    if l1 == l then 1 else 1 + aux l1.next in aux l.next;;

(* [print_l2c l] prints all elements of l*)
let print_l2c l =
    let l_cur = ref l.next in 
    while !l_cur != l do 
        print_int !l_cur.elem; print_newline ();
        l_cur := !l_cur.next
        done;
        print_int !l_cur.elem;
        print_newline ();;
        
(*[fusion_l2c l1 l2] merges [l1] and [l2] in one l2c*)        
let fusion_l2c l1 l2 =
    (l1.next).prev <- l2;
    (l2.next).prev <- l1;
    let l1n = l1.next in
        l1.next<- l2.next;
        l2.next<- l1n;;
