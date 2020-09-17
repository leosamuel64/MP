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


(* Pour le 17/09 : Finir l'ex 1 (faire complet gauche a la fin car plus compliqué) *)

let estCompletGauche a =
  let f = Queue.create () in
  Queue.add a f;

  let neContientQueVide f=
    let res = ref true in
    while not(Queue.is_empty f) && !res do
      if Queue.take f <> Vide then res:=false
    done;
    !res
    in
  
  let rec aux f =
    if Queue.is_empty f then 
      true
    else
      let t = Queue.take f in
      match t with
        | Vide  -> neContientQueVide f
        | Noeud(fg,_,fd) -> ( Queue.add fg f ;
                              Queue.add fd f ; 
                              aux f )
  in aux f                 
;;

let estParfait a=
  let hinit = profondeur a in
  let rec aux a h i=
    match a with
    | Vide when h = i -> true
    | Vide -> false
    | Noeud(fg,_,fd) -> aux fg h (i+1) && aux fd h (i+1)
  
  in aux a hinit 0
;;


let rec estEquilibre a=
  match a with
      |Vide               -> true
      | Noeud(fg,_,fd)    -> estEquilibre fg && estEquilibre fd && abs (profondeur fg - profondeur fd) <2
;;



let feuille x = Noeud(Vide, x, Vide);;
let exemple1=
  Noeud (
     Noeud  (
         feuille 2,
         1,
         feuille 3     
         ),
     2,
     Noeud ( 
         feuille 1,
         0,
         feuille 2
         )
     );;

let exemple = Noeud (Noeud(Vide, 2, feuille 1),5,Noeud(Vide,3,Noeud(Noeud(Vide, 0, Vide),2,Vide)));;
let arbre1 = Noeud(Noeud(Noeud(Vide,5,Vide), 6, Vide), 7, Vide) ;;
let arbre2 = Noeud(
					Noeud(Vide,
							5,
							Vide)
					, 12,
					
					Noeud(Vide,
							7,
							Vide))
      ;;
      
estComplet exemple;;
estParfait exemple;;
estCompletGauche exemple;;
estEquilibre exemple;;

