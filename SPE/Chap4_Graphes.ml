(* exercice 1 *)

type graphe1 = int array array;;
type graphe2 = int list array;; 

let exemple_mat = [|  [|0;1;0;0|];
                      [|0;0;1;0|];
                      [|0;0;0;1|];
                      [|1;0;0;0|]
|];;

let exemple_tab = [|[1];[2];[3];[0]|];;

(* Exercice 1 *)

(* 1 *)

let compte_Matrice m=
  let res = ref 0 in
  for i=0 to Array.length m-1 do
    for j=0 to Array.length m-1 do
      if m.(i).(j) = 1 then
        res:=!res+1
    done;
  done;
  !res
;;

compte_Matrice exemple_mat;;

let compte_liste t=
  let res=ref 0 in
  for i=0 to Array.length t-1 do
    res:=!res+ List.length t.(i)
  done;
  !res
;;

compte_liste exemple_tab;;


(* Pour Samedi : questions 2,3 *)

(* 2 *)
let tableau_i n i=
	let res = Array.make n 1 in
	for j = 0 to n-1 do
		if i=j then
			res.(j) <- 0
  done;
  res
;;
	
let clique_mat n=
	let res = Array.make n [||] in
	for i=0 to n-1 do
		res.(i) <- tableau_i n i
	done;
	res
;;

clique_mat 2;;

	
let rec liste_sans_i i n=
	match n with
	|0 -> []
	|t when i <> t -> t::(liste_sans_i i (t-1))
	|t -> liste_sans_i i (t-1)
;;
	
let rec clique_tab n=
	let res = (Array.make n [] ) in
	for i=0 to n-1 do
		res.(i) <- liste_sans_i (i+1) n
	done;
	res
;;

clique_tab 4;;	
	
	
(* 3 *)
let chemin_mat l g=
	let res = ref true in
	for i=0 to Array.length l -2 do
		if g.(l.(i)).(l.(i+1)) = 0 then
			res := false
	done;
	!res
;;

chemin_mat [|1;2;3|] exemple_mat;;

let rec appartient x l=
	match l with
	|[] -> false
	|t::q when t = x -> true
	|t::q -> appartient x q
;;

let rec chemin_tab l g=
	match l with
	|[] -> true
	|[x] -> true
	|a::b::q when appartient b g.(a) -> chemin_tab (b::q) g
	|_ -> false
;;

chemin_tab [1;2;3] exemple_tab;;	

(* 4 *)

let miroir g=
  let n = Array.length g in
  let h = Array.make n [] in
  for i=0 to n-1 do
    let l = ref [] in
    for j=0 to n-1 do
      if List.mem i g.(j) then
        l:=(i::!l)
    done;
    h.(i) <- !l
  done;
  h
;;

(* Entrée : un graphe (S,A) *)
(* Sortie : le graphe (S,{(y,x) ; (x,y) ∈ A}) *)
(* 
Am <- {}

(
∀ (x,y) ∈ A, 
  Rajouter (y,x) dans Am
Renvoyer (S,Am)
)

Devient  :
(
∀ s ∈ S,                      (1)
  ∀ t voisin de s,            (2)
    Rajouter (t,s) dans Am
Renvoyer (S,Am)
)
*)

(* Etude de la complexité de miroir :
Premiere boucle -> s'execute |S| fois
Deuxième boucle -> s'execute |S| -1 fois dans le pire cas

Complexité au pire : O(|S|²)

En plus fin :
On prend chaque ligne séparément et on étudie :
1- Combien de fois elle est executée au total pendant l'execution de l'algo
2- sa complexité

n<- |S|                                 ->(O(1))
res <- tab de n cases avec des []       ->(O(n))
∀ s ∈ S:                                ->(O(n))
    ∀ t ∈ g.(s)                         
    res.(t) <- s::res.(t)               ->( ∀ (s,t) ∈ A : O(|A|))
                                        Donc le cous est |A| * O(1) 
                                        cad O(|A|) sur la boucle
renvoyer le resultat                    ->(O(1))

Total : Σ O(chaque ligne) = O(|S|) + O(|A|)
*)

(* Remarque : Place mémoire occupée par un graphe :  
- Matrice d'adj : O(|S|²)
- tableau de listes d'adj : total O(|S|+|A|) -> on appelle taille du graphe
  - pour le tab : |S|
  - pour les listes : |A|

![GrapheExemple]()


Le nombre |S|+|A| est appelé "taille du graphe"
    *)

let miroir2 g=
  let n = Array.length g in 
  let res = (Array.make n []) in

  let rec parcoursVoisins s = function     (* (2) *)
    (* Pour t dans l, on rajoute l'arête (t,s) dans res. Procedure *)
  | [] -> ()
  | t::q -> (* t est un sommet accessible depuis s *)
            res.(t) <- s::(res.(t));
            parcoursVoisins s q
  in

  for s=0 to n-1 do
    parcoursVoisins s (g.(s))
    done;
  res
;;

let miroir3 g=
  let n = Array.length g in 
  let res = (Array.make n []) in

  for s=0 to n-1 do
    List.iter (fun t -> res.(t) <- s::(res.(t)))
              g.(s) 
    (* List.iter ne fonctionne que pour les procédures !! *)
    done;
  res
;;



(* 5 *)

(* 
Entrée : un graphe (S,A)
Sortie : Rien
Effet de bord : ∀ (s,t) ∈ A, rajoute (t,s) si elle n'y était pas dans A

∀ s ∈ S,                      (1)
  ∀ t voisin de s,            (2) 
    Si (t,s) n'appartient pas à A        ( List.mem s g.(t)  )
      Rajouter (t,s) dans A              ( g.(t) <- s::g.(t) )
*)

let desoriente g=
  let n = Array.length g in 
  let res = (Array.make n []) in

  let rec parcoursVoisins s = function     (* (2) *)
    (* Pour t dans l, on rajoute l'arête (t,s) dans res. Procedure *)
  | [] -> ()
  | t::q when not(List.mem s g.(t)) ->  g.(t) <- s::g.(t);
                                        parcoursVoisins s q
  | t::q -> parcoursVoisins s q
  in

  for s=0 to n-1 do
    parcoursVoisins s (g.(s))
    done;
  res
;;

let desoriente2 g=
  let n = Array.length g in 
  let res = (Array.make n []) in
  
  for s=0 to n-1 do
    List.iter (fun t -> if not(List.mem s g.(t)) then 
                          res.(t) <- s::(res.(t)))
              g.(s) 
    (* List.iter ne fonctionne que pour les procédures !! *)
    done;
  res
;;

(* Pour le 16/11 : faire la 7 et la 8 *)
(* Indication pour la 8):
Entrée : Un graphe (S,A)
Sortie : le puits total s'il existe

n= |S|
nb_aretes_entrantes <- Un tab de n case initialement à 0

∀ s ∈ S:
    ∀ t voisin de s:
    Augmenter n_aretes_entrantes.(t) de 1
    
  (* En déduire le résultat ... *)


  (* Et le programmer ... *)
*)





(* Exercice 2 *)


let matrice_of_liste t=
  let n = Array.length t in
  let m = Array.make_matrix n n 0 in
  
  let rec aux l i m=
    match l with
    | [] -> ()
    | t::q -> m.(i).(t) <- 1; aux q i m;
  in

  for i=0 to n-1 do
    aux t.(i) i m
  done;
  m
;;

matrice_of_liste exemple_tab;;
    


let liste_of_matrice m=
  let n = Array.length m in
  let t = Array.make n [] in
  for ligne=0 to n-1 do
    for colonne=0 to n-1 do
      if m.(ligne).(colonne)=1 then
        t.(ligne) <- colonne::(t.(ligne))
    done;
  done;
      
  t
  ;;

liste_of_matrice exemple_mat;;

(* Exercice 3 *)

let matriceAleatoire n=
  let m = Array.make_matrix n n 0 in

  for i=0 to n-1 do
    for j=0 to n-1 do
      m.(i).(j)<-Random.int 2
        
    done;
  done;
  m
;;

matriceAleatoire 4;;


let matriceAleatoireNonOriente n=
  let m = Array.make_matrix n n 0 in
  for i=0 to n-1 do
    for j=0 to n-1-i do
      let r = Random.int 2 in
      m.(i).(j)<-r;
      m.(j).(i)<-r;
    done;
  done;
  m
;;

matriceAleatoireNonOriente 4;;