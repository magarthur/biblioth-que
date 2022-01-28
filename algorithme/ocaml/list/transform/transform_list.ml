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
        
(* [add_l e ll] returns a list of lists obtained by adding e to each list of [ll]. *)
let rec add_l e ll = match ll with
    | [] -> []
    | x::s -> (e::x)::add_l e s;;

(* [parts_l l] returns a list consisting of all subsets of elements of [l]. *)  
let rec parties_l l = match l with
    | [] -> [[]]
    | e::q -> let parties_q = parties_l q in
              parties_q @ (add_l e parties_q)

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
