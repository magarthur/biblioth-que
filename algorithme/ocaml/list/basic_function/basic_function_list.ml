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
