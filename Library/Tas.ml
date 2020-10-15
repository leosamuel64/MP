type 'a arbre = Vide | Noeud of ('a arbre * 'a * 'a arbre) ;;

let estUnTas a=
  let rec aux a xprec=
    match a with
    | Vide -> true
    | Noeud(fg,x,fd) when xprec >= x -> aux fg x && aux fd x
    | _ -> false
    in 
      match a with
      | Vide -> true
      | Noeud(fg,e,fd) -> aux fg e && aux fd e
;;

let maxi = function
  | Vide -> failwith "Erreur : Tas vide"
  | Noeud(_,e,_) -> e
;;

let rec insertion x t=
  match t with
  | Vide -> Noeud(Vide,x,Vide)
  | Noeud(fg,e,fd) when x>e ->  Noeud(fd,x,insertion e fg) (* x est le nouveau max du tas *) 
  | Noeud(fg,e,fd) -> Noeud(fd,e,insertion x fg)
;;

let rec tas_of_list = function
  | [] -> Vide
  | t::q -> insertion t (tas_of_list q)
;;

let rec rassemble t1 t2=
  match t1,t2 with
  | Vide , _ -> t2
  | _ , Vide -> t1
  | Noeud(fg1,e1,fd1), Noeud(fg2,e2,fd2) when e1 <= e2 -> Noeud(t1,e2,rassemble fg2 fd2)
  | Noeud(fg1,e1,fd1), Noeud(fg2,e2,fd2) -> Noeud(rassemble fg1 fd1,e1,t2)
;;

let max_extrait = function
 | Vide -> failwith "Erreur : Tas Vide"
 | Noeud(fg,m,fd) -> m, rassemble fg fd
;;

let triParTas l=
  let rec aux t=
    match t with
    | Vide -> []
    | _ -> let max,suitetas = max_extrait t in 
                      max::(aux suitetas)
  in aux (tas_of_list l)
;;
