(*missing dynamic array in the test*)



(* [sum_l t1] returns the sum of elements of [t1] *)
let sum_t t1 = 
    let result = ref 0 in
    for i=0 to ((Array.length t1)-1) do
        result := t1.(i) + !result
    done;
    !result;;

(* [mem_t e t1] returns if e belongs to [t1]*)
let mem_t e t1 = 
    let result = ref false in
    for i = 0 to ((Array.length t1)-1) do
        if t1.(i) = e then result := true
    done;
    !result;;

(* [mini_t t1] returns the minimum of [t1]*)
let mini_t t1 = 
    let result = ref max_int in
    for i = 0 to ((Array.length t1)-1) do
        if t1.(i) < !result then result := t1.(i)
    done;
    !result;;

(* [maxi_l t1] returns the maximum of [t1]*)
let maxi_t t1 = 
    let result = ref min_int in
    for i = 0 to ((Array.length t1)-1) do
        if t1.(i)> !result then result := t1.(i)
    done;
    !result;;

(* [increase_l t1] returns if [t1] increases *)
let increase_t t1 =
    let result = ref true in
    for i = 0 to ((Array.length t1)-2) do
        if t1.(i) > t1.(i+1) then result := false
    done;
    !result;;

(* [decrease_t t1] returns if [t1] decreases *)
let decrease_t t1 = 
    let result = ref true in
    for i = 0 to ((Array.length t1)-2) do
        if t1.(i) < t1.(i+1) then result := false
    done;
    !result;;
    
(* [doublon_t t1] returns true or false if the has one or more duplicates *)
let doublon_t t1 =
    let result = ref false in
    for i = 0 to ((Array.length t1)-2) do
        for j = i + 1 to ((Array.length t1)-1) do
        if t1.(i)=t1.(j) then result := true
        done;
    done;
    !result

(* [reverse_t t1] transform [\a1;a2;...;an|] into [an;...;a2;a1|]*)
let reverse_t t1 =
    let n = ((Array.length t1)-1) in 
    for i = 0 to n/2 do
        let c = ref t1.(i) in 
        let d = ref t1.(n-i)in
        t1.(i) <- !d;
        t1.(n-i) <- !c;
    done;
    t1;;

(* [equal_t [|a1; ...; an|] [|b1; ..; bm|]] holds when the two input arrays have the same length, and for each pair of elements ai, bi at the same position we have eq ai bi.*)
let equal_t t1 t2 = 
    let result = ref true in
    let m1 = Array.length t1 in
    let m2 = Array.length t2 in
        if m1 <> m2 then result := false
        else for i = 0 to m1-1 do
            if t1.(i) <> t2.(i) then result := false
            done;
    !result;;

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
    
(* [dichotomie_t e t1] returns if e belongs to [t1] in complexity O(nlog(n))*)
let dichotomie e t1 = 
    let rec aux i j =
        if i > j then false
        else let m = (i+j)/2 in
            if t1.(m) = e then true
            else if t1.(m) > e then aux i (m-1)
            else aux (m+1) j in
aux 0 ((Array.length t1)-1);;

(* [trichotomie_t e t1] returns if e belongs to [t1] in complexity O(nlog(n))*)
let trichotomie e t1 =
    let rec aux i j =
    if i > j then false
    else let m1 = (2*i + j + 1)/3 in
        let m2 =(i + 2*j +2)/3 in
        if t1.(m1) = e || t1.(m2) = e then true
        else if e< t1.(m1) then aux i (m1-1)
        else if e< t1.(m1) then aux (m1+1) (m2-1)
        else aux (m2+1) j in
aux 0 ((Array.length t1)-1);;

(* [tri_comptage_t  t1] transform t1 into a sorted array*)
let tri_comptage_t t1 =
    let m = maxi_t t1 in
    let compte = Array.make (m+1) 0 in
    let n = Array.length t1 in
    for i=0 to n - 1 do
        compte.(t1.(i)) <- compte.(t1.(i)) + 1
    done;
    let k = ref 0 in 
    for i=0 to m do
        for j=1 to compte.(i) do
            t1.(!k) <- i;
            incr k
        done;
    done;
    t1;;
    
(* [swap t i j] exchanges [t.(i)] and [t.(j)] *)    
let swap_t t1 i j =
  let tmp = t1.(i) in
  t1.(i) <- t1.(j);
  t1.(j) <- tmp;;
  
(* [sort_bubble t] sorts array t using bubble sorting (O(n**2)) *)
let sort_bubble_t t1 = 
  for _ = 0 to Array.length t1 - 1 do
    for j = 0 to Array.length t1 - 2 do
      if t1.(j) > t1.(j + 1) then swap_t t1 j (j + 1)
      done
  done;;
  
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


type 'a dyn = {mutable t :'a array; mutable n : int};;

(*copy t1 into t2 *)
let copy t1 t2 = 
    for i = 0 to Array.length t1 - 1 do
    t2.(i) <- t1.(i)
    done;;
    
    
(*[add e d] add einto d with O(1) complexity *)
let add e d =
    if d.n < Array.length d.t then (d.t.(d.n) <- e;d.n <-d.n +1) 
    else if d.n = 0 then (d.t <- [|e|]; d.n <- d.n + 1)
    else let t' = Array.make (2*d.n) d.t.(0) in
    (copy d.t t'; t'.(d.n) <- e; d.t <- t';d.n <- d.n + 1);

(* [del d] delete the last data add*)
let del d = 
    if d.n = 0 then failwith "array is empty"
    else (d.n <- d.n - 1; d.t.(d.n));;
