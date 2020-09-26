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

tas_of_list [1;4;3;7;9];;                        

(* demo que l'arbre sera complet*)
(* On se place dans le cas ou on insere *)
(* ∀ k ∈ ℕ, soit P(h) : "Tout arbre formé par tas_of_list à partir d'une liste de longueur
                          2**(k+1)-1 est parfait de hauteur h" *)
          
(* Init : 2**1 -1 = 1. On insère 1 element dan Vide, on obtient une feuille cad un arbre parfait de hauteur 0 *)
(* Hered : Soit h ∈ ℕ tq P(h). On insère 2**(h+1)-1 elements dans Vide *)
(* Apres une insertion, la racine est formée, il reste 2**(k+2)-2 elem à placer *)
(* Vu le pg insertion, il y aura (2**(2+k)-2)/2 dans chaque fils soit 2**(2k+1)-1 dans chaque fils *)
(* Par HR, on obtient deux fils parfait de hauteur h *)
(* Donc l'arbre final est parfait de hauteur h+1 *)
(* Conclusion : ∀ k ∈ ℕ, P(h) *)

(* On peut meme montrer que ∀ k ∈ ℕ, si on insere n elements dans Vide et que 2 ≤ n ≤ 2**(k+1)-1  *)
(* On obtient un arbre complet de hauteur h *)

(* On a h ≤ log2(n). C'est la hauteur la plus petite possible pour un arbre binaire *)

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

List.rev(triParTas [8;3;2;7;0;5]);;

(* Complexité :
tas_of_list : On appelle n fois la fonction insertion donc O(nlog(n))
insertion : O(hauteur du tas) donc ici = O(log(n)) 

max_extrait : O(hauteur)=O(log(n))
*
lemme : max extrait renvoie un arbre de hauteur au plus celle du tas initial

Ainsi au fur et à mesure du detassage, la hauteur du tas reste < log2(n)

On a aux : O(nlogn)

Le tri est en O(nlog(n)) donc il est efficace, comparable au tri fusion
*)

(* Pour le 28/09 faire l'exercice 10 (1 et 2) *)

