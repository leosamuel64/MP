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

type categorieArbre = Parfait | CompletGauche | Complet | Equilibre | Quelconque;;


let categorieNaif a=
  if estParfait a then
    Parfait
  else if estCompletGauche a then
    CompletGauche
  else if estComplet a then
    Complet
  else if estEquilibre a then
    Equilibre
  else
    Quelconque
;;

categorieNaif exemple;;
categorieNaif arbre1;;
categorieNaif arbre2;;

let compare a b=
  if a=Parfait && b=Parfait then Parfait
  else if a=Quelconque || b = Quelconque then Quelconque
  else Quelconque
;;

(* 1.2) Revision sur les ABR *)

(* Une fonction un peu plus compliquée que celles vue en MPSI *)

(* enlever un element d'un ABR : *)

(* L'étape cruciale est celle consistante à rassembler les deux fils d'un ABR en un unique ABR *)

let rec fusion_ABR_disjoints ag ad=
  (* ag et ad deux abr tq les étiquette de ag sont toutes < aux étiquettes de ad *)
  match ag,ad with
  | Vide , _ -> ad
  | _ , Vide -> ag
  | Noeud(fgg,eg,fgd),Noeud(fdg,ed,fdd) -> Noeud(fgg ,eg ,Noeud(
                                                                fusion_ABR_disjoints fdg fgd,
                                                                ed,
                                                                fdd))
;;
(* 
          eg
      /        \
  (fgg)          ed
                /  \
  (Fusion fdg fgd)  (fdd)
 *)

let rec sans a x=
  match a with
  | Vide -> Vide
  | Noeud(fg,e,fd) when x=e -> fusion_ABR_disjoints fg fd
  | Noeud(fg,e,fd) when x < e -> Noeud(sans fg x,e,fd)
  | Noeud(fg,e,fd) -> Noeud(fg,e,sans fd x)
;;
  
(* -- Exercice 5 : Union et intersection *)

(* - 1 *)

let rec segmente a x=
  (* abr des elem <x, abr des elem >x, x ∈ a *)
  match a with
  | Vide -> (Vide,Vide,false)
  | Noeud(Vide,e,Vide) when e=x -> (Vide,Vide,true)
  | Noeud(Vide,_,Vide)  -> (Vide,Vide,false)

  | Noeud(fg,e,fd) when e=x -> (fg,fd,true)
  | Noeud(fg,e,fd) when e<x -> let fgg,fgd,x_dans_fg = segmente fg x in
                                  (
                                    fgg,
                                    Noeud(fgd,e,fd),
                                    x_dans_fg
                                  )
  | Noeud(fg,e,fd) -> let fgg,fgd,x_dans_fd = segmente fd x in
                                  (
                                    fgd,
                                    Noeud(fg,e,fgg),
                                    x_dans_fd
                                  )
;;  

  (* Finir l'exo 5 pour le 21/09/2020 *)




  let rec reunion a1 a2 =
    match a1,a2 with
        | Vide,_    -> a2
        | _,Vide    -> a1
        | Noeud(fg1,e1,fd1), Noeud(fg2,e2,fd2) when e1 < e2 ->
                Noeud( fg1,e1,  Noeud(reunion (sans fd1 e2) (sans fg2 e1), e2, fd2))
        | Noeud(fg1,e1,fd1), Noeud(fg2,e2,fd2) when e1 > e2 ->
                Noeud( fg2,e2,  Noeud(reunion (sans fd2 e1) (sans fg1 e2), e1, fd1))
        | Noeud(fg1,e1,fd1), Noeud(fg2,e2,fd2) ->
                Noeud(fg1,e1, reunion (reunion fd1 fg2) fd2)
;;


let rec intersection a1 a2=
    match a1,a2 with
        | Vide,_    -> Vide
        | _,Vide    -> Vide
        
        | Noeud(fg1,e1,fd1), Noeud(fg2,e2,fd2) when e1 < e2 ->
            reunion   (intersection a1 fg2) (intersection fd1 a2)
                    
        | Noeud(fg1,e1,fd1), Noeud(fg2,e2,fd2) when e1 > e2 ->
            reunion   (intersection a2 fg1) (intersection fd2 a1)
        
        | Noeud(fg1,e1,fd1), Noeud(fg2,e2,fd2) ->
            Noeud( intersection fg1 fg2, e1 , intersection fd1 fd2)
;;

let rec difference a b=
    (* renvoie un ABR contenant les Ã©tiquettes de a qui ne sont pas dans b*)
    match a,b with
        | Vide,_    -> Vide
        |_,Vide     -> a
        
        | Noeud(fg1,e1,fd1), Noeud(fg2,e2,fd2) when e1 = e2 ->
              fusion_ABR_disjoints (difference fg1 fg2) (difference fd1 fd2)
        
        | Noeud(fg1,e1,fd1), Noeud(fg2,e2,fd2) when e1 < e2 ->
                let fd1bis = difference (sans fd1 e2) fd2 
                 in
                    difference (Noeud(fg1,e1,fd1bis)) fg2 
        
        | Noeud(fg1,e1,fd1), Noeud(fg2,e2,fd2)  ->
                let fg1bis = (difference (sans fg1 e2)) fg2
                 in
                    difference (Noeud(fg1bis,e1,fd1)) fd2
;;

       

reunion exemple arbre1;;
intersection exemple arbre1;;
difference exemple arbre1;;

