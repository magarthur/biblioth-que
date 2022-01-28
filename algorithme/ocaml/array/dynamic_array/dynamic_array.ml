type 'a dyn = {mutable t :'a array; mutable n : int};;

(*copy t1 into t2 *)
let copy t1 t2 = 
    for i = 0 to Array.length t1 - 1 do
    t2.(i) <- t1.(i)
    done;;
    
    
(*[add e d] add e into d with O(1) complexity *)
let add e d =
    if d.n < Array.length d.t then (d.t.(d.n) <- e;d.n <-d.n +1) 
    else if d.n = 0 then (d.t <- [|e|]; d.n <- d.n + 1)
    else let t' = Array.make (2*d.n) d.t.(0) in
    (copy d.t t'; t'.(d.n) <- e; d.t <- t';d.n <- d.n + 1);
    
(* [del d] deletes the last data add in the dynamic array *)
let del d = 
    if d.n = 0 then failwith "array is empty"
    else (d.n <- d.n - 1; d.t.(d.n));;


let copy t1 t2 = 
    for i = 0 to Array.length t1-1 do 
    t2.(i) <- t1.(i)
    done;


let del_dyn d =
    if d.n = 0 then failwith "array is empty"
    else if d.n <= (Array.length d.t)/2 && d.n > 0 then 
        let t' = Array.make ((Array.length d.t)/2) d.t.(0) in (copy_bis d.t t'; d.t <- t' ; d.n <- d.n -1; d.t.(d.n))
    else (d.n <- d.n - 1; d.t.(d.n));;
