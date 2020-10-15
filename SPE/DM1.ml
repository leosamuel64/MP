
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

