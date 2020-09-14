(* -- Exercice 1 : Arbre complet, équilibré, parfait

Complet :

        O
      /   \
      O    O
     / \    \ 

Complet a gauche :

     O
   /   \
   O    O
  / \  /

parfait :

     O
   /   \
   O    O
  / \  / \

Equilibré (mais pas complet)

        O
      /   \
      O    O
     / \  / \
     O    O  O
    /    /  / \
               O
                \

Lien logique :  parfait => complet gauche => complet => équilibré 
                        *)

type 'a arbre = Vide | Noeud of ('a arbre * 'a * 'a arbre) ;;
let feuille x = Noeud(Vide, x, Vide);;
let exemple = Noeud 
        (Noeud(Vide, 2, feuille 1),5,Noeud(Vide,3,Noeud
                                          (Noeud(Vide, 0, Vide),2,Vide)));;

let rec profondeur a =
  match a with
  | Vide -> 1
  | Noeud(fg,_,fd) -> 1 + max (profondeur fg) (profondeur fd) 
;;


profondeur exemple;;

let estComplet a =
  let hInit = profondeur a in
  let rec aux a2 h hInit =
    (*  
        a est un arbre
        h est la hauteur du noeud 
        hInit est la hauteur de a
    *)
    match a2 with
    | Vide when h = hInit || h-1=hInit -> true
    | Vide -> false
    | Noeud(fg,_,fd) -> (aux fg (h+1) hInit) && (aux fd (h+1) hInit)

  in 
    aux a 0 hInit
;;

estComplet exemple;;

let estCompletGauche a=
  let rec aux a h hInit =
    (*  
        a est un arbre
        h est la hauteur du noeud 
        hInit est la hauteur de a
    *)
    match a with
    | Noeud(fg,_,fd) -> match 
    | Vide when h = hInit || h-1=hInit-> true
    | Vide -> false
    | Noeud(fg,_,fd) -> (aux fg (h+1) hInit) && (aux fd (h+1) hInit)

  in 
    aux a 0 (profondeur a)
;;

(* Pour le 17/09 : Finir l'ex 1 (faire complet gauche a la fin car plus compliqué) *)