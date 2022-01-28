type 'a l2c = {elem : int ; mutable prev : 'a liste ; mutable next : 'a liste} and 'a liste = None |C of 'a l2c;;


let create e = {elem = e; prev = None; next = None};;


let ajoute l e = 
        let l_new = {elem = e; prev = C(l); next = None} in
         l.next <- C(l_new);;
