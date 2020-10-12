(* Tri par insertion *)

let rec insertion x l=
  (*  Entrées : x un élément
                l une liste triée
      Sortie : liste triée obtenue en insérant x dans l à la "bonne" place
       *)
  match l with
  | [] -> [x]
  | t::q when t>x -> x::t::q
  | t::q -> t::(insertion x q)
;;

insertion 4 [];;

let rec triInsertion l=
    match l with
    | [] -> []
    | t::q -> insertion t (triInsertion q)
;;

(* Exercice *)

let rec plusPetitQue l x=
  match l with
  | [] -> 0
  | t::q when t<x -> 1+(plusPetitQue q x)
  | _::q -> plusPetitQue q x
;;

let rec nbInversion l=
  match l with
  | [] -> 0
  | a::q -> plusPetitQue q a + nbInversion q
;;

nbInversion [4;1;3;2;0];;

(* Complexité en O(n²) *)


(* Tri par segmentation : *)

let segmentation l pivot=
  (* Renvoie un couple : (liste des éléments inferieurs ou égals au pivot,
                          liste des éléments strictement superieurs au pivot) *)
  let rec aux l pivot inf sup=
    match l with
    | [] -> (inf,sup)
    | t::q when t <= pivot -> aux q pivot (t::inf) sup
    | t::q -> aux q pivot inf (t::sup)
  in aux l pivot [] []
;;

segmentation [1;4;3;7;8;4;2;7;10;6] 5;;

let rec triSegmentation1 l=
  match l with
  | [] -> []
  | t::q -> let l1,l2 = segmentation q t in
            (triSegmentation1 l1)@(t::(triSegmentation1 l2))
;;


triSegmentation1 [1;4;3;7;8;4;2;7;10;6] ;;

let rec triSegmentation= function
  | [] -> []
  | t::q -> let petits,grands =  segmentation q t in
            let petitsTri,grandsTri = triSegmentation petits,triSegmentation grands in
            (petitsTri @ t::grandsTri)
;;

triSegmentation [1;4;3;7;8;4;2;7;10;6] ;;

(* NB : il faut retirer le pivot de la liste avant segmentation, sinon la fonction triSegmentation peut
        ne pas terminer (si le pivot est le max) *)

(*  *)

