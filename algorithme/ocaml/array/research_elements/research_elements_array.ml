(* [mem_t e t1] returns if e belongs to [t1]*)
let mem_t e t1 = 
    let result = ref false in
    for i = 0 to ((Array.length t1)-1) do
        if t1.(i) = e then result := true
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

