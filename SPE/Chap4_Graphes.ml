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
                                        Donc le cout est |A| * O(1) 
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

Si une n-1 est dans nb_aretes_entrantes, alors on verifie que que le sommet est un puis
c'est alors un puits total sinon non

*)

(* 
Soit un graphe G=(S,G),
On suppose que G possède deux puits totaux p1 et p2 tq p1≠p2.

Or p1 est un puits total, alors :
∀ s ∈ S, p1 est accessible depuis s 

En particulier : p1 est accessible depuis p2
Or p2 est un puits total alors il ne mène à aucun sommet.

D'où la contradiction. Alors p1=p2.
Un puits total s'il existe est unique.
*)

let aUnPuitsTotal g =
  let n = Array.length g in
  let nb_arretes_entrante = Array.make n 0 in
  let rec voisins s = function
    | [] -> ()
    | t::q -> nb_arretes_entrante.(t)<-nb_arretes_entrante.(t)+1;
              voisins s q
  in
  for s=0 to n-1 do
    voisins s g.(s);
  done;

  let puits = ref (-1) in
   for i=0 to n-1 do
    if nb_arretes_entrante.(i) = n-1 && g.(i)=[] then
      puits := i;
   done;

    if !puits <> -1 then
      (true,!puits)
    else
      (false,-1)
  ;;




let aUnPuits g=
  let res = ref false in
  for i=0 to Array.length g-1 do
    if g.(i)=[] then
      res:=true
  done;
  !res
;;

aUnPuits exemple_tab;;



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

let chemin_entre sd sa g=
  let n= Array.length g in
  let chemin = Array.make n [] in 
  let a_Visiter= Queue.create () in
  let deja_vu= Array.make n false in

  Queue.add sd a_Visiter; 
  chemin.(sd)<-[sd];

  let rec visite_voisins s l=
    match l with 
    | [] -> ()
    | t::q when chemin.(t) = []->  Queue.add t a_Visiter;
                                    chemin.(t)<- t::chemin.(s);
                                    visite_voisins s q
    | t::q -> visite_voisins s q
    in
  while  not (Queue.is_empty a_Visiter) && chemin.(sa)=[] do
    let s= Queue.take a_Visiter in
    if not (deja_vu.(s)) then
    (
      visite_voisins s g.(s);
      deja_vu.(s) <- true;
    )

  done;
  List.rev chemin.(sa)
  ;;

let exemple_tab = [|[1];[2];[3];[0]|];;

let testTab = [|[1;2];[3;4;5];[6];[4];[];[];[0]|];;

let pasConnexe = [|[1];[2];[];[4];[]|];;

chemin_entre 0 2 testTab;;

let plus_court_chemin_entre sd sa g=
  let n= Array.length g in
  let chemin = Array.make n [] in 
  let dist = Array.make n (-1) in
  let a_Visiter= Queue.create () in
  let deja_vu= Array.make n false in

  Queue.add sd a_Visiter; 
  chemin.(sd)<-[sd];

  let rec visite_voisins s l=
    match l with 
    | [] -> ()
    | t::q when dist.(t)=(-1) ->   Queue.add t a_Visiter;
                                    dist.(t)<- dist.(s)+1;
                                    chemin.(t)<- t::chemin.(s);
                                    visite_voisins s q
    | t::q -> visite_voisins s q
    in
  while  not (Queue.is_empty a_Visiter) && chemin.(sa)=[] do
    let s= Queue.take a_Visiter in
    if not (deja_vu.(s)) then
    (
      visite_voisins s g.(s);
      deja_vu.(s) <- true;
    )

  done;
  List.rev chemin.(sa)
  ;;

(* Pour Lundi 23/11 : Exercice 10*)

(* Exercice 10 *)

(* 1 *)



let sphere g s d =
  let res = ref []
  and n = Array.length g in
  for i=0 to n-1 do
    let dist = (List.length (plus_court_chemin_entre s i g))-1 in
    if dist = d then
      res:= i::(!res);
  done;
  !res
;;

let distanceTab = [|[];[2;3;4];[5;6];[0];[7];[];[];[8];[]|];;

sphere distanceTab 1 2;;

let boule g s d =
  let res = ref []
  and n = Array.length g in
  for i=0 to n-1 do
    let dist = (List.length (plus_court_chemin_entre s i g))-1 in
    if dist <= d then
      res:= i::(!res);
  done;
  !res
;;

boule distanceTab 1 2;;



(* Exercice 6 *)

let composantes_connexes g=
  
  let n= Array.length g in
  let deja_vu= Array.make n false in
  
  let composante_connexe g sd =
    let a_Visiter= Queue.create () in
    let cc = ref [] in
    Queue.add sd a_Visiter; 
    let rec visite_voisins = function
      | [] -> ()
      | t::q -> Queue.add t a_Visiter;
                visite_voisins q
    in
    while  not (Queue.is_empty a_Visiter) do
      let s= Queue.take a_Visiter in
      if not (deja_vu.(s)) then
      (
        cc:=s::!cc;
        visite_voisins g.(s);
        deja_vu.(s) <- true;
      )
    done;
    !cc
    (* Maintenant, la composante connexe de sd correspond aux sommetsde deja_vu *)
  in
  let res = ref [] in
  for i=0 to n-1 do
    if not(deja_vu.(i)) then
      res:=(composante_connexe g i)::!res
  done;
  !res
  ;;

    
  
  


let testTab = [|[1;2];[3;4;5];[6];[4];[];[];[0]|];;

composantes_connexes testTab;;




type 'a arbre = Noeud of 'a * ('a arbre) list;; 

let arbre_exemple=
  Noeud(2,[
    Noeud(3,[
      Noeud(4,[]);
      Noeud(5,[])
    ]);
    Noeud(-1,[]);
    Noeud(5,[])
  ]);;




let rec somme = function
  | Noeud(e,fils) -> e + somme_foret fils
and somme_foret = function
  | [] -> 0
  | t::q -> (somme t)+somme_foret(q)
;;
    
somme arbre_exemple;;

let rec etiquettes_arbre = function
  | Noeud(e,fils)-> e::(etiquettes_foret fils)
and etiquettes_foret = function
  | [] -> []
  | t::q -> etiquettes_arbre(t)@(etiquettes_foret q)
;;

etiquettes_arbre arbre_exemple;;


(* Exercice 5 *)

(* 1- *)
(* 
Parcours : C'est un parcours en largeur (file)
But : Determiner  si le graphe est "fortement" connexe depuis s_0
Variable :
F -> file (aVisiter) [contient des sommets gris]
b -> bool Array (deja_vu) [Donne les sommets noirs, ceux qui étaient dans la file mais qui n'y sont plus]
π -> (int list) Array -> liste des sommets parents
s -> int
*)

(* 2- *)
(* 
Parcours : Parcours en largeur (file)
But : on récupère les pages accessibles en n pas/étape
*)

(* 3- *)
(* Parcours : parcours en largeur 
But : on récupère les pages accessibles en n pas/étape
*)

(* 4- *)
(* 
Parcours : 
But : 
*)



let parcours_prof g sd =
  let n = Array.length g in
  let deja_vu = Array.make n false in

  let rec visite_sommet s=
    deja_vu.(s) <- true;
    (* Faire quelque chose avec s... *)
    visite_voisins s g.(s)
  and visite_voisins s= function
    | [] -> (* Renvoyer quelque chose *)
    | t::autre_Voisin when not(deja.(t)) -> (* Renvoyer quelque chose avec visite_sommet et visite_voisins *)
    | t::autre_Voisin -> visite_voisins autre_Voisin

  in visite_sommet sd
;;

let composante_connexe_rec g sd=
  let n = Array.length g in
  let deja_vu = Array.make n false in

  let rec visite_sommet s=
    deja_vu.(s) <- true;
    s::(visite_voisins g.(s))
  and visite_voisins = function
    | [] -> []
    | t::autres_Voisins when not(deja_vu.(t)) -> (visite_sommet t)@(visite_voisins autres_Voisins)
    | t::autres_Voisins -> visite_voisins autres_Voisins

  in visite_sommet sd
;;

composante_connexe_rec testTab 2;;


(* Pour le 3/12 : Exercice 11 (en DM) *)

(* Exercice 10 *)

(* -3 *)

(* En utilisant un algo récursif *)

let sommets_accessible_en n g sd=
  let n = Array.length g in
  let deja_vu = Array.make n false in

  let rec visite_sommet n s=
    deja_vu.(s) <- true;
    s::(visite_voisins g.(s))
  and visite_voisins n = function
    | [] -> []
    | t::autre_Voisin when not(deja_vu.(t)) -> (visite_sommet (n+1) t)@(visite_voisins n autre_Voisin)
    | t::autre_Voisin -> visite_voisins n autre_Voisin

  in visite_sommet sd
;;

let sommets_accessible_en_n_aretes g n sd=
  let rec visite_sommet s i=
    if i=0 then 
      [s] 
    else
      (visite_voisins i g.(s))

  and visite_voisins i = function
  | [] -> []
  | t::autres_Voisins -> (visite_sommet t (i-1))@(visite_voisins i autres_Voisins)

  in
  visite_sommet sd n
;;

sommets_accessible_en_n_aretes testTab 2 0;;


(* Exercice 12 *)

let aCycle g =
  let n = Array.length g in
  let deja_vu = Array.make n false in
  
  let rec visite_sommet s sprec=
      if deja_vu.(s) then true
      else 
      (
          deja_vu.(s) <- true;
          visite_voisins s sprec g.(s)
      )
  and visite_voisins s sprec= function
      | [] -> false
      | t::autres_voisins when t<>sprec -> (visite_sommet t s) || (visite_voisins s sprec autres_voisins)
      | t::autres_voisins -> (visite_voisins s sprec autres_voisins)
  
  in
  List.exists (fun s -> visite_sommet s 0) g.(0)
  ;;

let arbreTest = [|[1;2];[3;4;0];[5;0];[6;7;8;1];[1];[2];[3];[3];[3]|];;
let testTab = [|[1;2];[3;4;5];[6];[4];[];[];[0]|];;

aCycle testTab;;

let exemple_matrice = [|[|0.;1.;infinity;4.;infinity|];
                        [|1.;0.;1.;infinity;infinity|];
                        [|infinity;1.;0.;1.;3.|];
                        [|4.;infinity;1.;0.;1.|];
                        [|infinity;infinity;2.;1.;0.|]        
                      |];;
                      
let exemple_liste = [|
                        [(1,1.);(3,4.)];
                        [(0,1.);(2,1.)];
                        [(1,1.);(3,1.);(4,3.)];
                        [(0,4.);(2,1.);(4,1.)];
                        [(3,1.);(2,2.)]
                    |];;

let copie_mat m=
  let n,p = Array.length m,Array.length m.(0) in
  let res = Array.make_matrix n p m.(0).(0) in

  for i=0 to n-1 do
    for j=0 to p-1 do
      res.(i).(j)<-m.(i).(j);
    done;
  done;
  res
;;

copie_mat exemple_matrice;;

let floyd_warshall m=
  (* Entrée : m matrice d'adjacence d'un graphe pondéré G *)
  let n = Array.length m in
  let dist = copie_mat m in
  for k=0 to n-1 do
    (* Ici dist contient les d_{i,j}^{k} *)
    let sauv = copie_mat dist in
    for i=0 to n-1 do
      for j=0 to n-1 do
        dist.(i).(j) <- min (sauv.(i).(j)) (sauv.(i).(k)+.sauv.(k).(j));
      done;
    done;
    (* Maintenant, dist contient les d_{i,j}^{k+1} *)
  done;
  dist
;;

floyd_warshall exemple_matrice;;

(* Calculer le chemin entre deux points. Faire une mat chemin qui sera une matrice de liste *)


let floyd_warshall_chemin m sd sa=
	(* Renvoie, s'il en existe, un chemin entre deux sommets et [] sinon *)
	let n = Array.length m in
	let chemin = Array.make_matrix n n [] in
  let dist = copie_mat m in
  (* Init *)
  for i=0 to n-1 do
    for j=0 to n-1 do
      chemin.(i).(j) <- [i];
    done;
  done;
	for k=0 to n-1 do
		let sauv = copie_mat dist in
		for i=0 to n-1 do
			for j=0 to n-1 do
				if (sauv.(i).(k) +. sauv.(k).(j)) < dist.(i).(j) then
					(
					dist.(i).(j) <- sauv.(i).(k) +. sauv.(k).(j);
					chemin.(i).(j) <- chemin.(i).(k)@chemin.(k).(j);
          )
			done;
		done;
	done;
  chemin.(sd).(sa)@[sa]
;;

floyd_warshall_chemin exemple_matrice 1 4;;

let rec pcc pred sd sa=
    if sd = sa then
      [sd]
    else
      sa::(pcc pred sd pred.(sd).(sa))
      
;;
    

let floyd_warshall_chemin2 m sd sa=
  let n = Array.length m in
  let pred = Array.make_matrix n n (-1) in
  let dist = copie_mat m in
  (* Init *)
  for i=0 to n-1 do
    for j=0 to n-1 do
      if m.(i).(j) <> infinity && i<>j then
        pred.(i).(j) <- i;
    done;
  done;
	for k=0 to n-1 do
		let sauv = copie_mat dist in
		for i=0 to n-1 do
			for j=0 to n-1 dor
				if (sauv.(i).(k) +. sauv.(k).(j)) < dist.(i).(j) then
					(
					dist.(i).(j) <- sauv.(i).(k) +. sauv.(k).(j);
					pred.(i).(j) <- pred.(k).(j);
          )
			done;
		done;
	done;
List.rev (pcc pred sd sa)
;;

floyd_warshall_chemin2 exemple_matrice 0 4;;



(* BIBLIO TAS *)

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

(* FIN BIBLIO TAS *)

let dijkstra g sd sa=
  let n = Array.length g in
  let dejaVu = Array.make n false
  and dist = Array.make n infinity in

  let rec boucle_principale aVisiter =
    (* aVsiter est un tas de couple (-dist.(s) au moment où s a été entasser, s) *)
    if (aVisiter=Vide) || dejaVu.(sa) then
      dist.(sa)
    else
      (
      let (_,s), suiteAvisiter = max_extrait aVisiter in
      if not dejaVu.(s) then
        (
        dejaVu.(s)<-true;
        parcours_voisins s aVisiter g.(s);
        )
      else
        boucle_principale suiteAvisiter
      )

  and parcours_voisins s aVisiter = function
    | [] -> boucle_principale aVisiter
    | (t,lst)::autres when dist.(t) > dist.(s) +. lst -> dist.(t)<-dist.(s) +. lst;
                                        parcours_voisins s (insertion (-.dist.(s) -. lst, t) aVisiter) autres

    | (t,lst)::autres -> parcours_voisins s aVisiter autres
  in

  (* initialisation *)
  dist.(sd) <- 0.;
  boucle_principale (insertion (0.,sd) Vide)
  ;;

dijkstra exemple_liste 0 4;;

