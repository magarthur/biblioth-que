(* [maxi_l t1] returns the maximum of [t1]*)
let maxi_t t1 = 
    let result = ref min_int in
    for i = 0 to ((Array.length t1)-1) do
        if t1.(i)> !result then result := t1.(i)
    done;
    !result;;

(* [tri_comptage_t  t1] transform t1 into a sorted array of size n whose inputs are integers between 0 and M*)
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
  
