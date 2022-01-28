(*missing insert_sort in the test and l2c*)

(* [fst_l l1] returns the first element of [l1]*)
let fst_l l1 = match l1 with
    |[] -> failwith " not elements"
    |e::q -> e;;
    
(* [lst_l l1] returns the last element of [l1]*)
let rec lst_l l1 = match l1 with
    |[] -> failwith "not elements"
    |[e] -> e
    |e::q -> lst_l q;;

(* [nth_l l1] returns the n-th element of [l1] *)
let rec nth_l e l1 = match l1 with
    |[] -> failwith "impossible"
    |e1::q -> if e = 0 then e1
                else nth_l (e-1) q;;

(* [length_l l1] returns the length of [l1]*)
let rec length_l l1 = match l1 with
    |[] -> 0
    |e::q -> 1 + length_l q;;

(* [sum_l l1] returns the sum of elements of [l1] *)
let rec sum_l l1 = match l1 with
    | [] -> 0
    | e::q -> e + sum_l q;;
    
(* [mem_l e l1] returns if e belongs to [l1]*)     
let rec mem_l e l1 = match l1 with
    |[] -> false
    |e1::q -> e1 = e || mem_l e q;;
    
(* [append_l l1 l2] returns the concatenation between [l1] and [l2] *)
let rec append_l l1 l2 = match l1 with
    |[] -> l2
    |e::q -> e::append_l q l2;;
    
(* [rev_append_l l1 l2] returns the concatenation between the reverse of [l1] and [l2]*)
(* for reverse a list, put [l2] = []*)
let rec rev_append_l l1 l2 = match l1 with
    |[] -> l2
    |e::q -> rev_append_l q (e::l2)

(* [mini_l l1] returns the minimum of [l1]*)
let rec mini_l l1 = match l1 with
    |[] -> max_int
    |e::q -> let m = mini_l q in
            if e < m then e
            else m;;
            
(** [maxi_l l1] returns the maximum of [l1]*)            
let rec maxi_l l1 = match l1 with
    |[] -> min_int
    |e::q -> let m = maxi_l q in
            if e > m then e
            else m;;

(* [increase_l l1] returns if [l1] increases *)
let rec increase_l l1 = match l1 with
    |[] -> true
    |[e] -> true
    |e::e1::q -> e<e1 && increase_l (e1::q);;

(* [decrease_l l1] returns if [l1] decreases *)
let rec decrease_l l1 = match l1 with
    |[] -> true
    |[e] -> true
    |e::e1::q -> e>e1 && decrease_l (e1::q);;
    
(* [doublon_l l1] returns true or false if the has one or more duplicates *)
let rec doublon_l l1 = match l1 with
    |[] -> false
    |e::q -> if mem_l e q then true
                else doublon_l q;;

(* [doublon2_l l1] returns [l1] without duplicates *)
let rec doublon2_l l1 = match l1 with
    |[] -> []
    |e::q -> if mem_l e q then doublon2_l q
            else e::doublon2_l q;;
    
(* [equal_l [a1; ...; an] [b1; ..; bm]] holds when the two input lists have the same length, and for each pair of elements ai, bi at the same position we have eq ai bi.*)
let rec equal_l l1 l2 = match l1,l2 with
    |[],[] -> true
    |[],_ | _,[]-> false
    |e::q, e1::q1 -> if e <> e1 then false 
                    else equal_l q q1;;
                    
(* [map f [a1; ...; an]] applies function f to a1, ..., an, and builds the list [f a1; ...; f an] *)           
let rec map_l f l1 = match l1 with
    | [] -> []
    |e::q -> f(e)::map_l f q;;

(* [filter_l f l] returns all the elements of the list l that satisfy the predicate f *)
let rec filter_l f l1 = match l1 with
    | [] -> []
    |e::q -> if f(e) then e::filter_l f q
            else filter_l f q;;

(* [for_all_l f [a1; ...; an]] checks if all elements of the list satisfy the predicate f. That is, it returns (f a1) && (f a2) && ... && (f an) for a non-empty list and true if the list is empty. *)
let rec for_all_l f l1 = match l1 with
    |[] -> true
    |e::q -> if f(e) = true then for_all_l f q
            else false;;
            
(* [exists_l f [a1; ...; an]] checks if at least one element of the list satisfies the predicate f. That is, it returns (f a1) || (f a2) || ... || (f an) for a non-empty list and false if the list is empty. *)
let rec exists_l f l1 = match l1 with
    |[] -> false
    |e::q ->  f(e) || exists_l f q
    
(* [combine_pairs_l l1 l2] transform a pair of lists into a list of pairs: combine [a1; ...; an] [b1; ...; bn] is [(a1,b1); ...; (an,bn)]. *)   
let rec combine_pairs_l l1 l2 = match l1, l2 with
    |[],[] -> []
    |_,[] | [],_ -> failwith "Invalid_argument"
    |e1::q1, e2::q2 -> (e1,e2)::combine_pairs_l q1 q2;;

(* [split_pairs_l l1 ] transform a list of pairs into a pair of lists: split [(a1,b1); ...; (an,bn)] is ([a1; ...; an], [b1; ...; bn]) *)
let rec split_pairs_l l1 = match l1 with
    |[] -> ([],[])
    |(e1,e2)::q -> let q1,q2 = split_pairs_l q in
                    e1::q1, e2::q2;;

(* [split_l l1] transform a list into two lists : split [a1;a2;a3;a4;...,an] is [a1;a3...;an], [a2;a4;...;a(n-1)] *)
let rec split_l l1 = match l1 with
    |[] -> ([],[])
    |[e] -> ([e],[])
    |e1::e2::q -> let q1, q2 = split_l q in
                    e1::q1, e2::q2;;
                    
(* [fusion_l l1 l2] merges two sorted lists into one sorted list *)
let rec fusion_l l1 l2 = match l1, l2 with
    |[],_ -> l2
    |_,[] -> l1
    |e1::q1, e2::q2 -> if e1< e2 then e1::fusion_l q1 l2
                        else e2::fusion_l l1 q2;;
                        
(* [tri_l l1] transform l1 into a sorted list *)
let rec sort_l l1 = match l1 with
    |[] -> []
    |[e] -> [e]
    |_ -> let l2, l3 = split_l l1 in
        fusion_l (sort_l l2) (sort_l l3);;
        
(*partition l p returns a pair (l1, l2) where :
- l1 is a list containing the elements of l strictly less than p
- l2 is a list containing the elements of l greater than or equal to p *)
let rec partition_l l1 pivot = match l1 with
    |[] -> [],[]
    |e::q -> let l2,l3 = partition_l q pivot in
                if e< pivot then e::l2, l3
                else l2, e::l3

(* [quicksort_l l1] transform l1 into a sorted list *)
let rec quicksort_l l1 = match l1 with
    |[]->[]
    |p::q -> let l2, l3 = partition_l q p in
         append_l (quicksort_l l2) (p::quicksort_l l3)
  
(* [insert x l] insert x into a sorted list l *)
let rec insert x l = match l with 
    |[] -> [x]
    |e::q -> if x <=e then x::e::q
            else e::insert x q;;

(* [insert_sort l] transform a list into a sorted list, complexity = O(n^2) *)
let rec insert_sort l = match l with
    |[] -> []
    |e::q -> insert e (insert_sort q);;
        
(* [add_l e ll] returns a list of lists obtained by adding e to each list of [ll]. *)
let rec add_l e ll = match ll with
    | [] -> []
    | x::s -> (e::x)::add_l e s;;

(* [parts_l l] returns a list consisting of all subsets of elements of [l]. *)  
let rec parties_l l = match l with
    | [] -> [[]]
    | e::q -> let parties_q = parties_l q in
              parties_q @ (add_l e parties_q);;

(* [array_of_list l] transform a list into a array *)
let array_of_list l = 
    let m = length_l l in
    let t = Array.make m 0 in
    let rec aux m1 l1 = match l1 with
        |[]->[||]
        |e::q -> t.(m-m1) <- e;
                if m1 = 1 then t
                else aux (m1 - 1) q in
aux m l;;

(* [array_to_list t] transform a array into a list *)
let array_to_list t = 
    let m = Array.length t in
    let rec aux i = 
        if i = 0 then []
        else t.(m-i)::aux (i-1) in
aux m;;



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
