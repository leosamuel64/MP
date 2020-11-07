
#directory "/mnt/c/Users/leosa/Desktop/INFO/CPGE/MP/Library";;
#use "File.ml";;
#use "Tas.ml";;
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

let rec fusion liste1 liste2=
  match liste1,liste2 with
  | [],_->liste2
  | _,[]->liste1
  | t1::q1,t2::q2 when t1<t2->t1::(fusion q1 liste2)
  | t1::q1,t2::q2 ->t2::(fusion liste1 q2)
;;

(* TAS *)

type 'a arbre = Vide | Noeud of ('a arbre * 'a * 'a arbre) ;;

let rec insertionTas x t=
  match t with
  | Vide -> Noeud(Vide,x,Vide)
  | Noeud(fg,e,fd) when x>e ->  Noeud(fd,x,insertionTas e fg) (* x est le nouveau max du tas *) 
  | Noeud(fg,e,fd) -> Noeud(fd,e,insertionTas x fg)
;;

let rec rassembleTas t1 t2=
  match t1,t2 with
  | Vide , _ -> t2
  | _ , Vide -> t1
  | Noeud(fg1,e1,fd1), Noeud(fg2,e2,fd2) when e1 <= e2 -> Noeud(t1,e2,rassembleTas fg2 fd2)
  | Noeud(fg1,e1,fd1), Noeud(fg2,e2,fd2) -> Noeud(rassembleTas fg1 fd1,e1,t2)
;;

let max_extrait = function
 | Vide -> failwith "Erreur : Tas Vide"
 | Noeud(fg,m,fd) -> m, rassembleTas fg fd
;;


(* 1.1- Premiere amélioration : sous-listes déja triées *)

(* 1- *)

let rec prochain_morceau l =
  let rec aux l dernier =
    match l with
    (* argument supplémentaire : prec le dernier élément pris dans le morceau actuel *)
    (* Renvoie un couple (éléments dans l'ordre et >= prec au début de la liste, suite de la liste)  *)
    | []  ->[], []
    | t::q when dernier <= t -> let morceau, suite = aux q t in t::morceau, suite
    | t::q -> [], t::q
  in
  match l with
  | []->[],[]
  | t::q-> let m, s = aux q t in (t::m,s)
;;

prochain_morceau [1;2;6;7;3;2];;
(* int list * int list = ([1; 2; 6; 7], [3; 2]) *)

(* 2- *)
let rec decoupe_liste = function
    | [] -> fileVide
    | l -> let morceau, suite = prochain_morceau l in
	     enfile morceau (decoupe_liste suite)
;;

decoupe_liste [1;2;0;-1;3;4];;
(* int list file = {entree = [[1; 2]; [0]; [-1; 3; 4]]; sortie = []} *)

(* 3-  *)
let rec fusionFile f=
    match defile f with
    | t,q when q=fileVide -> t
    | t,q -> let s, q2 = defile q in fusionFile (enfile (fusion s t) q2)
;;

fusionFile (decoupe_liste [1;2;0;-1;3;4]);;
(* int list = [-1; 0; 1; 2; 3; 4] *)

let tri1 l =
  fusionFile (decoupe_liste l)
;;

tri1 [1;2;3;4;1;2;8;9;4;2;3;4];;
(* int list = [1; 1; 2; 2; 2; 3; 3; 4; 4; 4; 8; 9] *)



(* 1.2- Deuxième amélioration : Sous-listes décroissantes *)

let rec retourne l=
  let rec aux res = function
    | [] -> res
    | t::q -> aux (t::res) q
  in
  aux [] l;;

retourne [4;1;8;3];;
(* int list = [3; 8; 1; 4] *)

let prochain_morceau l =
  let rec aux prec croissant l = 
    match l with
    | [] -> [], []
    | t::q when (t>=prec && croissant) || (t<= prec && not croissant) -> 
                                                let morceau, suite = aux t croissant q in
                                                t::morceau, suite
    | t::q  -> [], t::q
  in
  match l with
  | [] -> [], []
  | [t] -> [t], []
  | s::t::q when s<= t -> (* croissant *)
               let morceau, suite = aux t true q in (s::t::morceau, suite)
  | s::t::q -> (* décroissant *)
     let morceau, suite = aux t false q in (retourne (s::t::morceau), suite)
;;

prochain_morceau [0;5;3;2;1;7;9;2;1;0;4;5;6];;
(* int list * int list = ([0; 5], [3; 2; 1; 7; 9; 2; 1; 0; 4; 5; 6]) *)

(* Troisième amélioration : Insertion *)

let taille_min = 3;;

(* 1 *)

let rec insertion x l croissant =
  match l with
  |[] -> [x]
  |t::q when (x<=t && croissant) || (x>=t && not croissant) -> x::t::q
  |t::q -> t::(insertion x q croissant )
;;

let prochain_morceau l =
  let rec aux prec croissant taille accu l =
    match l with
    |[] -> (accu, [])
    |t::q when (t>=prec && croissant) || (t<= prec && not croissant) -> (* On continue *)
                aux t croissant (taille+1) (t::accu) q
    |t::q  -> if taille<taille_min then
	              aux prec croissant (taille+1) (insertion t accu (not croissant)) q
              else
	              (accu, t::q)
  in

  match l with
  |[] -> [], []
  |[t] -> [t], []
  |s::t::q when s<= t -> (* croissant *)
              let morceau, suite = aux t true 2  [t; s] q in (retourne morceau, suite)
  |s::t::q -> (* décroissant *)
              let morceau, suite = aux t false 2 [s; t] q in ( morceau, suite)
;;

prochain_morceau [0;5;3;2;1;7;9;2;1;0;4;5;6];;
(* int list * int list = ([0; 3; 5], [2; 1; 7; 9; 2; 1; 0; 4; 5; 6]) *)

let prochain_morceau l =
  let rec aux prec croissant taille accu l = 
    match l with
    |[] -> (accu  , [], taille)
    |t::q when (t>=prec && croissant) || (t<= prec && not croissant) ->
        aux t croissant (taille +1) (t::accu) q
    |t::q  -> 
       if taille < taille_min then
	 aux prec croissant (taille+1) (insertion t accu (not croissant)) q
       else
	 (accu, t::q, taille)
  in
  match l with
  |[] -> [], [], 0
  |[t] -> [t], [], 1
  |s::t::q when s<= t -> (* croissant *)
     let morceau, suite, taille = aux t true 2  [ t; s] q in (retourne morceau, suite, taille)
					 
  |s::t::q -> (* décroissant *)
     let morceau, suite, taille = aux t false 2 [s;t] q in ( morceau, suite, taille)
;;

prochain_morceau [0;5;3;2;1;7;9;2;1;0;4;5;6];;
(* int list * int list * int = ([0; 3; 5], [2; 1; 7; 9; 2; 1; 0; 4; 5; 6], 3) *)



let rec decoupe_liste l =
  match l with 
  |[] -> Vide
  |l -> let morceau, suite, taille = prochain_morceau l in
      insertionTas (-taille, morceau) (decoupe_liste suite)
;;

decoupe_liste [0;5;3;2;1;7;9;2;1;0;4;5;6];;
(* (int * int list) arbre =
Noeud
 (Noeud (Vide, (-3, [2; 1; 7]), Vide), (-3, [4; 5; 6]),
  Noeud (Vide, (-3, [0; 3; 5]), Noeud (Vide, (-4, [0; 1; 9; 2]), Vide))) *)


  let tri l=
    let rec rassemble f=
      match max_extrait f with
      | (taille, l), Vide -> l
      | (taille1, l1), q  -> let (taille2, l2), q2 = max_extrait q in
          rassemble (insertionTas (taille1+taille2, fusion l1 l2) q2)
    in
    
    rassemble (decoupe_liste l )
  ;;

tri [0;5;3;2;1;7;9;2;1;0;4;5;6];;
(* int list = [0; 0; 1; 2; 1; 3; 4; 5; 5; 6; 7; 9; 2] *)

