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
fusionAvecColle tas1 tas2 3;;
(* -- 2 *)


let rec tas_of_arbre a=
  match a with
  | Vide -> Vide
  | Noeud(fg,e,fd) -> fusionAvecColle (tas_of_arbre fg) (tas_of_arbre fd) e
;;

let feuille x = Noeud(Vide, x, Vide);;
let exemple = Noeud (Noeud(Vide, 2, feuille 1),5,Noeud(Vide,3,Noeud(Noeud(Vide, 6, Vide),2,Vide)));;

tas_of_arbre exemple;;

(* Notre premiere version de tas_of_list a une complexité de O(nlogn). Nous devons faire mieux : *)
(* L'arbre est parfait de hauteur p-1, il nous faut 2**(p)-1 éléments 
   dont feuilles 2**(p-1) et 2**(p-1)-1 noeud interne*)

(* On prend une liste de longueur 2**(p)-1 et on la coupe en 2 *)

(* Tas_of_list amélioré (Floyd) *)

(* Etape 1 : on découpe la liste en deux. La premiere partie moitié deviendra des feuille et l'autre servira de colle *)

let rec decoupe l=
  (* Renvoie deux listes: - une liste de feuille de longueur (l+1)/2 
                          - une liste des éléments restant de longueur l/2*)
  match l with
  |[]-> [],[]
  |[a]->[feuille a],[]
  |a::b::q-> let l1,l2 = decoupe q in 
             (feuille a)::l1,b::l2
;;

let testdecoupe = [3;2;7;9;0;3;1];;

decoupe testdecoupe;;

(* La methode pour rassembler le tout : *)
(* On maintient une file d'attente de petits tas. Initialement, on y met toute les feuilles *)


type 'a file = {entree :'a list; sortie : 'a list};;
let fileVide = {entree=[]; sortie=[]};;

let enfile x f =
  {entree = (x::f.entree) ; sortie = f.sortie}
  ;;

let rec defile f =
  match f.sortie with
  | [] -> if f.entree <> [] then defile {entree = [] ; sortie = List.rev f.entree}
            else failwith "Erreur : File Vide"
  | t::q -> (t,{entree = f.entree ; sortie = q})
  ;;

let testfile = {entree = [] ; sortie = []};; 
defile testfile;;

let enfile_liste l f =
  List.fold_left (fun file elem -> enfile elem file) f l;;

let egales f1 f2=
  ((f1.sortie)@(List.rev f1.entree)) = ((f2.sortie)@(List.rev f2.entree))
;;

(* Pour charger un fichier library
# directory "/mnt/c/Users/leosa/Desktop/INFO/CPGE/MP/Library/File.ml"
# use "file.ml" 
*)

let tas_of_list_Floyd l=
  let l_feuille,l_colle_initial = decoupe l in
  let f_feuille = enfile_liste l_feuille fileVide in
  let rec aux file colle=
    if colle=[] then
      let res,_ = defile file in res
    else
    match defile file,defile file,colle with
    | (t1,_),(t2,suiteFile),colle::q -> aux 
                                          (enfile (fusionAvecColle t1 t2 colle) file) 
                                          q
    | _ -> failwith "Erreur : La liste n'a pas le bon nombre d'élément"                                      
    in aux f_feuille l_colle_initial
;;

tas_of_list_Floyd testdecoupe;;


(* Complexité : 

- fusionAvecColle t1 t2 x : O(hauteur) 
∀ k ∈ ℕ, soit Ck la complexité max de fusionAvecColle de deux tas de hauteur ≤ hypot
Alors C0=O(1)
      ∀ k ∈ ℕ*, Ck = C(k-1)+O(1)
D'où Ck = O(h)

∀ i de p-1 à 0
La construction de l'étage necessite 2**i fusions d'arbre de hauteur p-2-i

D'où cxt 2**i*O(p-i)
          O(2**i *(p-i))

D'où cxté : Σ(i=0 -> p-1) O(2**i(p-i)) = O(Σ(i=0 -> p-1) 2**i (p-i))    (serie à termes ≥ 0)
                                       = O(p(2**p-1)- Σ(i=1 -> p-1)i2*i)

                                       On note f:x -> Σ(i=1->p-1) i2*i
                                               f':x -> Σ(i=1->p-1) i2*(i-1)

                                      Or ∀ x≠1 : f(x)=(x-x**p)/(1-x)
                                      D'où ∀ x≠1 : (1-px**(p-1)(1-x)+xx**p)/(1-x)²

                                      D'où Σ(i=1->p-1) i2*i = 2f'(2)
                                                            = (p-1)*2**(p-1)-p2**p+2

                                      Finalment : cxt = O(p*2**p-p-(p-1)*2**(p+1)+p*2**p-2)
                                                      = O(2**(p+1)-p-2)
                                                      = O(2**p)
                                                      = O(Longueur de la liste initial)

                                      Cette méthode est plus efficace que la méthode naive
*)

(* II-5 : Tas mutables *)


(* Un tas peut être facilement enregistré dans un simple tableau *)
(* De plus, on peut alors toujours faire en sorte d'avoir un arbre complet gauche *)
(* L'idée est de numéroter les noeuds selon un parcours en largeur *)

(* Prop : Soit a un arbre binaire dont on numérote les noeuds selon un parcourt en largeur en partant de 1 *)
(* Soit n=|a|. On suppose que a complet gauche *)
(* ∀ i in in ⟦1,n⟧, le numéro du fils gauche du noeud i (s'il existe) est 2i et le numero du fils droit 
   (s'il existe) est 2i+1 *)

(* Demo : Soit N le noeud numéro i. Soit p sa profondeur *)

       (* (  2**p-1   )
( i-2**p )      /   \
(2*(i-2**p))   fg   fd *)


(* Il y a (2**p -1) élements dans les etages 0 à p-1 (arbre parfait de hauteur p-1 *)
(* Il y a (i-2**p) élements à gauche de N*)

(* Soit fg et fd les fils de N *)
(* il y a 2(i-2**p) elements à gauche de fg car ce sont les fils des éléments à gauche de i, et a est complet gauche *)

(* Donc le numéro de fg est  2**p-1 + 2i-2**(p+1) +1 = 2i*)
(* Donc le numéro de fd est 2i+1 *)
(* ----------- *)

(* NB : en base 2, le numéro du fg s'obtient en rajoutant 0 à la fin et le numéro du fd *)
(*      en rajoutant un 1. Par exemple comme dans l'Exercice 12 *)

(* Cependant, les tableaux sont d'ordinaire numéroté en partant de 0. Voyant ce que devienent les formules *)

(* Soit N un noeud de numéro i dans la numerotation en partant de i et j dans la numérotation 
en partant de 1 : Donc j=i-1 *)

(* Soit ig,id,jg,jd les numéros des fils g et d dans la 1er et 2emme numérotation *)

(* ig=2i *)
(* jg = ig-1=2i-1= 2j+1 *)

(* Et jd = ig-1 = 2i+1-1= 2j+2   (Plus de rapport simple avec l'éccriture en base 2) *)

(* En résumé : avec cette nouvelle numérotation :  *)

(* Le fg du neoud numéro j a pour numéro 2j+1  et le fd 2j+2*)

(* Soit t un tas complet gauche et n=|t|.
   Pour l'enregistrer dans un tableau. On enregistre ∀ j ∈ ⟦0,n⟦, l'étiquette du noeud numéro j dans la case j *)

(* Souvent, on peut laisser des cases vides à la fin du tableau pour permettre des insertions ulterieures *)
(* On doit alors enregistrer le nombre d'éléments du tas à partir de 0 *)

 
type 'a tasMutable = {
                      mutable longueur :  int;
                      donnees : 'a array
                      };;

(*  *)

let nouveauTas n x = {longueur=0;donnees = Array.make n x};;

(* t est un tas <=> ∀ i ∈ ⟦0,n⟦ t.donnees.(i) ≥ t.donnees.(2i+1) t.donnees.(i) ≥ t.donnees.(2i) *)
(* Soit i ∈ ℕ, i =  ⌞((2i+2)-1/)2⌟ = ⌞((2i+1)-1)/2⌟ *)
(* Donc k ∈ ⟦1,n⟦, le numero du père de k est  ⌞(k-1)/2⌟ *)
(* Donc t est un tas ssi ∀ k ∈ ⟦1,n⟦ t.donnes.((k-1)/2)≥ t.donnees.(k) *)

let estUnTas t=
  (* Indique si t est un tas *)
  (* ∀ k ∈ ⟦1,n⟦ t.donnes.((k-1)/2)≥ t.donnees.(k) *)
  let res = ref true in
  for k=0 to (t.longueur-1) do
    if not(t.donnees.((k-1)/2) >= t.donnees.(k)) then
      res:=false 
  done;
  !res
;;

let estUnTasOpti t=
  let res = ref true in
  let k = ref 0 in
  while !k<=(t.longueur-1) && !res=true do
    if not(t.donnees.((!k-1)/2) >= t.donnees.(!k)) then
      res:=false;
    k:=!k+1
  done;
  !res
;;

let tas = {longueur=7;donnees=[|5;4;3;2;1;2;18|]};;
let pastas = {longueur=5;donnees=[|1;4;2;7;1|]};;

estUnTas tas;;
estUnTas pastas;;

estUnTasOpti tas;;
estUnTasOpti pastas;;

let estUnTasRec t=
  let n = t.longueur in
  let rec aux k =
    if k=n then true
    else t.donnees.((k-1)/2) >= t.donnees.(k) && aux (k+1)
  in aux 1
;;

estUnTasRec tas;;
estUnTasRec pastas;;

(* insertion d'un élément *)
(* On place le nouvel élément au prochain emplacement libre *)
(* On l'echange avec son pere puis son grand père etc jusqu'a que se soit a nouveau un tas  *)


let insere t x=
  let transpose tab i j=
    let tmp = tab.(i) in
    tab.(i) <- tab.(j);
    tab.(j) <- tmp
  in

  let rec remonteASaPlace tab k=
    (* Entrée : un tableau qui represente un arbre binaire
       Precondition : la portion tab.(0:k) represente un tas 
       Effet : en sortie, tab.(0;k+1) represente un tas *)
    if k=0 || tab.((k-1)/2) >= tab.(k) then ()
    else (
      transpose tab k ((k-1)/2);
      remonteASaPlace tab ((k-1)/2)
        )
  in 

  let n = t.longueur in
  t.donnees.(n) <- x;
  t.longueur <- n+1;
  remonteASaPlace t.donnees n
;;

let tas = {longueur=5;donnees=[|5;4;3;2;1;2;18|]};;
insere tas 15;;
tas;;



let extraitMax t =
  let rec descendASaPlace tab k n=
    (*  tab est un tableau representant un arbre binaire
      Preconditions : Les sous-arbre enracinés en 2k et 2k+1  sont des tas
      Effet : à la fin, l'arbre enraciné en k est un tas
    *)
    let transpose tab i j=
      let tmp = tab.(i) in
      tab.(i) <- tab.(j);
      tab.(j) <- tmp
    in
    (* Cas 1 : on doit descendre tab.(k) à droite *)
    if 2*k+2<n && tab.(k) <= tab.(2*k+1) && tab.(2*k)> tab.(k) then (
      transpose tab k (2*k+2);
      descendASaPlace tab (2*k+2) n
      )
    else if 2*k+1<n && tab.(k)<tab.(2*k+1) then (
      transpose tab k (2*k+1);
      descendASaPlace tab (2*k+1) n
      )
  in

  let n = t.longueur in
  if n=0 then failwith "Erreur : Tas Vide" 
    else (
    let maxi = t.donnees in
    t.donnees.(0) <- t.donnees.(n-1);
    t.longueur <- n-1;
    descendASaPlace t.donnees 0 (n-1);
    maxi
    )
;;

(* Pour le 5/10 : Programmer le tri par tas avec des tas mutables *)


