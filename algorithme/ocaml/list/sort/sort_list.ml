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
 
(* [append_l l1 l2] returns the concatenation between [l1] and [l2] *)
let rec append_l l1 l2 = match l1 with
    |[] -> l2
    |e::q -> e::append_l q l2;;
    
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
