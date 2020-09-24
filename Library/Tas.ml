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