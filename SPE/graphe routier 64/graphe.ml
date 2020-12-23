(* etape 1 : numeroter les sommets *)
(* 
créer - une hashtbl sommet -> numero 
      un tableau int -> string
*)

(* Fonction auxiliaire *)

let strip c=
  (* renvoie c sans le dernier charactere *)
  let n = String.length c in
  String.sub c 0 (n-1)
;;

let concat s1 s2=
  let s3= String.make 1 s2 in
  String.concat s1 ["";s3]
;;



let split c=
  let res = ref [] in
  let temp = ref "" in
  let n = String.length c in
  for i=0 to n-1 do
    if c.[i]=';' || i=n-1 then
      (
        if i=n-1 then
        (
        res:=(concat !temp c.[n-1])::(!res);
        )
        else
        res:=!temp::(!res);
        temp:=""
      )
    else
      temp := concat !temp c.[i];
  done;
  List.rev !res
;;



let lecture chemin =
  let entree = open_in chemin in
  let rec aux ()=
    try
    let ligne = (input_line entree) in
      ligne :: aux ()
    with
      |End_of_file -> []
    in 
    aux ()
;;

let lect = lecture "/mnt/c/Users/leosa/Desktop/INFO/CPGE/MP/SPE/graphe routier 64/sommets.csv";;

let premier l=
  match l with
  | [] -> failwith "Liste Vide"
  | t::q -> t
;;

let rec splitListe l=
  match l with
  | [] -> []
  | t::q -> premier(split(strip t))::splitListe q
;;


let listeVille chemin=
  let lect = lecture chemin in
  splitListe lect
  ;;

listeVille "/mnt/c/Users/leosa/Desktop/INFO/CPGE/MP/SPE/graphe routier 64/sommets.csv";;

let creer_dico_tableau chemin=
  let liste = listeVille chemin in
  let n = List.length liste in
  let dico = Hashtbl.create n in
  let tab = Array.make n "" in

  let rec aux l i =
    match l with
    | [] -> ()
    | t::q -> Hashtbl.add dico t i ; tab.(i)<-t ; aux q (i+1)
  in
  aux liste 0;
  (tab,dico,n)
;;

creer_dico_tableau "/mnt/c/Users/leosa/Desktop/INFO/CPGE/MP/SPE/graphe routier 64/sommets.csv";;

let eclate l=
  match l with
  | [] -> failwith "Liste Vide"
  | [a;b;d] -> (a,b,float_of_string d)
  | _ -> failwith "Erreur"
;;


type 'a arbre = Vide | Noeud of ('a arbre * 'a * 'a arbre) ;;

let rec insertion x t=
  match t with
  | Vide -> Noeud(Vide,x,Vide)
  | Noeud(fg,e,fd) when x>e ->  Noeud(fd,x,insertion e fg) (* x est le nouveau max du tas *) 
  | Noeud(fg,e,fd) -> Noeud(fd,e,insertion x fg)
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

let construit_liste_dijkstra chemin dico n=
  let res = Array.make n [] in
  let entree = open_in chemin in
  let rec aux ()=
    try
    let ligne = (input_line entree) in
      let dep,arr,dist = eclate (split ( strip ligne) ) in
      let depnum = Hashtbl.find dico dep in
      let arrnum = Hashtbl.find dico arr in
      res.(depnum) <- (arrnum, dist)::res.(depnum);
        res.(arrnum) <- (depnum, dist)::res.(arrnum); 
      aux ()
    with
      |End_of_file -> ()
    in 
    aux ();
    res
;;

let dijkstra g sd sa=
  let n = Array.length g in
  let dejaVu = Array.make n false
  and dist = Array.make n infinity
  and chemin = Array.make n [] in

  let rec boucle_principale aVisiter =
    (* aVsiter est un tas de couple (-dist.(s) au moment où s a été entasser, s) *)
    if (aVisiter=Vide) || dejaVu.(sa) then
      (dist.(sa),chemin.(sa))
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
    | (t,lst)::autres when dist.(t) > dist.(s) +. lst -> dist.(t) <- dist.(s) +. lst;
                                                          chemin.(t) <- t::chemin.(s);
                                        parcours_voisins s (insertion (-.dist.(s) -. lst, t) aVisiter) autres
    | (t,lst)::autres -> parcours_voisins s aVisiter  autres
  in

  (* initialisation *)
  dist.(sd) <- 0.;
  chemin.(sd) <- [sd];
  boucle_principale (insertion (0.,sd) Vide)
  ;;

let cheminSommets = "/mnt/c/Users/leosa/Desktop/INFO/CPGE/MP/SPE/graphe routier 64/sommets.csv" ;;
let cheminAretes = "/mnt/c/Users/leosa/Desktop/INFO/CPGE/MP/SPE/graphe routier 64/aretes.csv" ;;

let tab64, dico64, n = creer_dico_tableau cheminSommets;;




let listeFinale = construit_liste_dijkstra cheminAretes dico64 n;;

let distanceEntre s1 s2 dico liste=
  let n1 = Hashtbl.find dico s1 in
  let n2 = Hashtbl.find dico s2 in
  dijkstra listeFinale n1 n2
;;


let d,chem =distanceEntre "PAU" "LESCAR" dico64 listeFinale;;

let rec retrace l tab=
  match l with
  | [] -> []
  | t::q -> (tab.(t))::(retrace q tab)
;;

retrace chem tab64;;


let sommet_of_int64 i=
  tab64.(i)
;;

let int_of_sommet64 nom=
  Hashtbl.find dico64 nom
;;

sommet_of_int64 398;;
int_of_sommet64 "LAC DE BIOUS-ARTIGUES";;

let voisins s g = 
  let numS = Hashtbl.find dico64 s in
  let rec aux l=
    match l with
    | [] -> []
    | (a,b)::q -> (sommet_of_int64 a)::(aux q)
  in aux g.(numS)
;;

voisins "TARBES" listeFinale;;