(* [slice_max_t t1] returns the maximum sum of consecutive elements of [t1] *)
let slice_max_t t1 =
    let m = ref t1.(0) in
    let m_cur = ref t1.(0) in
    for i = 1 to Array.length t1 - 1 do
        m_cur := max (!m_cur + t1.(i)) t1.(i);
        m := max !m !m_cur
    done;
    !m;;

(* [map_t f [|a1; ...; an|]] applies function f to a1, ..., an, and builds the array [|f a1; ...; f an|] *) 
let map_t f t1 =
    for i = 0 to ((Array.length t1)-1) do
        t1.(i) <- f(t1.(i))
    done;
    t1;;

(* [for_all_l f [|a1; ...; an|]] checks if all elements of the list satisfy the predicate f. That is, it returns (f a1) && (f a2) && ... && (f an) for a non-empty array and true if the array is empty. *)
let for_all_t f t1 =
    let result = ref true in
    for i = 0 to ((Array.length t1)-1) do
        if f(t1.(i)) = false then result := false
    done;
    !result;;

(* [exists_l f [|a1; ...; an|]] checks if at least one element of the array satisfies the predicate f. That is, it returns (f a1) || (f a2) || ... || (f an) for a non-empty array and false if the array is empty. *)
let exists_t f t1 =
    let result = ref false in
    for i = 0 to ((Array.length t1)-1) do
        if f(t1.(i)) = true then result := true
    done;
    !result;;
  
(* [length_l l1] returns the length of [l1]*) 
let rec length_l l1 = match l1 with
    |[] -> 0
    |e::q -> 1 + length_l q;;

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
