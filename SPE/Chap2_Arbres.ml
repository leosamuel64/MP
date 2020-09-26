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
  | Noeud(fg,e,fd) when e<x ->  let fgg,fgd,x_dans_fg = segmente fg x in
                                  (
                                    fgg,
                                    Noeud(fgd,e,fd),
                                    x_dans_fg
                                  )
  | Noeud(fg,e,fd) ->           let fgg,fgd,x_dans_fd = segmente fd x in
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



(* Exercice 3 : Reconstruire un arbre à partir de la liste de ses noeuds *)

(* -- 1 *)

type 'a morceauDArbre = V | E of 'a;;


let reconstruit1_Rec l =
  let rec aux l pile =
    match l with
    | [] -> (match pile with 
            | [res] -> res
            | _ -> failwith "Erreur")

    | V::q -> aux q (Vide::pile)
    | E x::q -> match pile with
                  | fd::fg::suite -> aux q (Noeud(fg,x,fd)::suite)
                  | _ -> failwith "Erreur2"
    in aux l []
;;

let test1 = [V;V;E 1;V;E 5;V;V;E 2;V;E 0;V;V;E 3;E 2;E 2];;
let test2 = [|V;V;E 1;V;E 5;V;V;E 2;V;E 0;V;V;E 3;E 2;E 2|];;


reconstruit1_Rec test1;;

let reconstruit1_Imp t=
  let pile = Stack.create () in

  for i=0 to Array.length t-1 do
    match t.(i) with
    | V -> Stack.push Vide  pile
    | E x -> let fg,fd =  Stack.pop pile,
                          Stack.pop pile in 
                                          Stack.push (Noeud(fg,x,fd)) pile
  done;

  let res = Stack.pop pile in
  if not (Stack.is_empty pile) then
    failwith "erreur syntaxe"
  else
    res
;;
  
reconstruit1_Rec test1;;
reconstruit1_Imp test2;;


(* -- 2 *)

let reconstruit2_Imp t=
  let file = Queue.create () in

  for i=0 to Array.length t-1 do
    match t.(i) with
    | V -> Queue.add Vide file
    | E x -> let fg,fd =  Queue.take file,
                          Queue.take file in 
                                          Queue.add  (Noeud(fg,x,fd)) file
  done;   

  let res = Queue.take file in
  if not (Queue.is_empty file) then
    failwith "erreur syntaxe"
  else
    res
;;

let test3 = [|V;V;V;V;V;E 2;V;V;E 3;E 0;V;E 1;E 2;E 5;E 2|];;

reconstruit2_Imp test3;;

(* Pour le 24/09 finir l'exercice 3 *)

(* Cours :  *)
(* II - Tas *)

  (* II.1 - Intro *)

  (* Un tas est un type d'arbre permettant la recherche efficace du max.*)
  (* Autres avantages :   - On peut controler efficacement sa hauteur *)
                      (*  -On peut en faire une version mutable facilement au sein d'un tableau *)

  (* II.2 - Definition *)

    (* Soit a un arbre binaire. On dit que c'est un tas lorsque ∀ noeud de a, 
    étiquette(noeud) > étiquette(fils) *)

          (*        5         *)
          (*      /   \       *)
          (*    2     4       *)
          (*   /     /        *)
          (*  0     2         *)
          (*         \        *)
          (*          1       *)

          (* Ce tas est un tas-max *)
          (* Si on permutte les fils d et g d'un tas, il reste un tas *)

          (* Le max d'un tas est l'étiquette de la racine *)


    (* Pour le 24/09 : Faire une fonction qui teste si un arbre est un tas *)

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

estUnTas exemple;;

(* II.3 - File de priorité *)

  (* Une file de priorité est une structure qui permet :
        - d'inserer un élément associé à un nombre appeller priotité 
        - d'extraire l'élément de priorité max *)

  (* Un tas est un bon moyen de réaliser une file de priorité *)

(* II.4 - Tas Persistant *)

    (* Gardons le type d'arbre binaire utilisé jusqu'ici *)
    (* Nous devons programmer les fonctions suivantes : 
                            - maxi : 'a arbre -> 'a qui renvoie le max d'un tas 
                            - max_extrait : 'a arbre -> a * 'a abre : renvoie le 
                                                                      couple (max_t, tas avec les autres élem)
                            - insertion : 'a -> 'a arbre : renvoie un nouveau tas *)

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

(* Exercice 12 *)

let rec insere x t n =

  let bin_of_dec n= 
    let rec aux n=
      match n with
      | 0 -> [] 
      | _ -> n mod 2 ::aux (n/2)
    in List.rev (aux n)
  in
  
  let rec insere_aux x t n =
    match t,n with
    | Noeud(Vide,e,Vide),[0] -> Noeud(Noeud(Vide,x,Vide),e,Vide)
    | Noeud(Vide,e,Vide),[1] -> Noeud(Vide,e,Noeud(Vide,x,Vide))
    | Noeud(fg,e,fd),t::q when t=0 -> Noeud(insere_aux x fg q,e,fd)
    | Noeud(fg,e,fd),t::q -> Noeud(fg,e,insere_aux x fd q)
    | _,_ -> failwith "Erreur"

  in insere_aux x t (bin_of_dec (n+1))

  
;;


(* Exercice 10 *)

(* -- 1 *)

let rec fusionAvecColle t1 t2 x =
  match t1,t2 with
  | Vide,Vide -> Noeud(Vide, x, Vide)
  | Noeud(Vide,e1,Vide),Noeud(Vide,e2,Vide) -> if x>e1 && x>e2 then Noeud(t1,x,t2)
                                               else if x>e1 then Noeud(t1,e2,Noeud(Vide,x,Vide))
                                               else Noeud(Noeud(Vide,x,Vide),e1,t2)

  | Noeud(tg1,e1,td1),Vide -> if x > e1 then Noeud(t1,x,Vide)
                              else Noeud(fusionAvecColle tg1 td1 x,e1,Vide)

  | Vide,Noeud(tg1,e1,td1) -> if x > e1 then Noeud(Vide,x,t1)
                              else Noeud(Vide,e1,fusionAvecColle tg1 td1 x)

  | Noeud(tg1,e1,td1),Noeud(tg2,e2,td2) -> if x>e1 && x>e2 then Noeud(t1,x,t2)
                                           else if x>e1 then Noeud(t1,e2,fusionAvecColle tg2 td2 x)
                                           else Noeud(fusionAvecColle tg1 td1 x,e1,t2)
;;

let tas1 = tas_of_list [1;2;3;4;5;7;9];;
let tas2 = tas_of_list [4;3;9;0;2;4;1;8];;

(* -- 2 *)


let rec arbreBinaire_of_tas a=
  match a with
  | Vide -> Vide
  | Noeud(fg,e,fd) -> fusionAvecColle (arbreBinaire_of_tas fg) (arbreBinaire_of_tas fd) e
;;

let feuille x = Noeud(Vide, x, Vide);;
let exemple = Noeud (Noeud(Vide, 2, feuille 1),5,Noeud(Vide,3,Noeud(Noeud(Vide, 6, Vide),2,Vide)));;

arbreBinaire_of_tas exemple;;



