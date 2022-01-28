type 'a file={tab: 'a array; mutable deb : int; mutable fin: int; mutable vide: bool};;

(* [ajout f e] add the element e in the pile*)
let ajoute f e =
  if f.deb = f.fin && not f.vide then failwith "full"
  else (f.tab.(f.fin) <-e;
       f.fin <- (f.fin + 1) mod Array.length f.tab;
       f.vide <- false);;

(* [retire f] delete the first element of the pile and returns this element *)
let retire f =
  if f.vide then failwith "empty"
  else let res = f.tab.(f.deb) in
        (f.deb <- (f.deb + 1) mod Array.length f.tab;
        f.vide <- f.deb = f.fin;
        res);;
 


(*file with two lists *)
type 'a file = {extra: 'a list; ajout: 'a list};;

(* [add_file f e] add e in the "ajout" list *)
let add_file f e = 
    {extra = f.extra; ajout = e::f.ajout}
    
(* [empty_file ] returns if f is empty or not *)
let empty_file f =
    f.extra = [] && f.ajout = [];;

(*[pop_file f] delete the first element of extra (so the last element of the file) and give this element*) 
let rec pop_file f = match f.extra with
  |e::q -> e, {extra = q; ajout = f.ajout}
  |[] -> pop_file {extra = List.rev f.ajout ; ajout = []};;
    
