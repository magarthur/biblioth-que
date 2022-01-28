type ('a, 'b) table_hachage = {hache : 'a -> int; donnees : ('a*'b) list array; largeur:int};;

(*create a "table_hachage" *)
let create h w = {hache = h; donnees = Array.make w []; largeur = w};;

(*[recherche t k] returns if k belongs at "table_hachage"*)
let recherche t k =
    let rec aux = function
    | [] -> false
    |(k',_)::q -> k=k' || aux q in 
    aux t.donnees.(t.hache k);;
    
(*
let recherche_bis t k =
    List.exists (fun x -> fst x = k) t.donnees.t.(hache k )
 *)

(* [element t k] returns element of key k in the "table_hachage"]*)
let element t k =
    let rec aux = function
    | [] -> failwith "not in the table"
    |(k',e)::q -> if k=k' then e
                else aux q in
aux t.donnees.(t.hache k);;

(*
let element_bis t k 
    list.filter (fun x -> fst x = k) t.donnees.(t.hache k)
    |> List.hd 
*)

(* [ajout t k e] add the element e in "table_hachage" with the key k *)
let add t k e =
    if recherche t k == true then failwith "key already here"
    else t.donnees.(t.hache k) <- (k,e)::t.donnees.(t.hache k);;

(* [suppr t k] delete the key k in the "table_hachage"*)
let delete t k =
    t.donnees.(t.hache k) <- List.filter (fun c -> fst c <> k) t.donnees.(t.hache k)
    
