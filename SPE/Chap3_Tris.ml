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