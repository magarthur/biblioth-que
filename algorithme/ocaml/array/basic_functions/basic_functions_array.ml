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
    




  
