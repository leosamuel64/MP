
type automate = { initial : int; 
                  finals : int list; 
                  transitions : (char*int) list array}
                ;;



let tout_vers i alphabet=
  List.map (fun x -> (x,i) ) alphabet
;;

let list_of_string c=
  let n = String.length c in
  let rec aux i=
    if i = n then []
    else
    c.[i] :: aux(i+1)
  in aux 0
;;
  
let alphabetLatin = list_of_string "abcdefghijklmnopqrstuvwxyz";;


let a_infini =
  let defaut = tout_vers 0 alphabetLatin in
  {
  initial = 0;
  finals = [6];
  transitions = [|('i',1)::defaut;
                  ('n',2)::('i',1)::defaut;
                  ('i',1)::('f',3)::defaut;
                  ('i',4)::defaut;
                  ('n',5)::('i',1)::defaut;
                  ('i',6)::('f',3)::defaut;
                  tout_vers 6 alphabetLatin
                |]
  }
;;
(* Ecrivons une fonction qui prend l'état actuel et la lettre du texte lue et qui
renvoie l'état suivant *)

let delta i x a=
  (*  a : automate
      x : une lettre
      i : un état de a
  *)
  List.assoc x (a.transitions.(i))
;;



let delta_etoile i m a =
  (* Cette fois m est une chaine de caracteres.
      Renvoie l'etat atteint apres lecture de toutes les lettres de m *)
  
  let rec boucle q k =
      (* k : prochaine lettre de m à lire
          q : etat actuel *)
          if k = String.length m then
              q
          else
              boucle (delta q m.[k] a ) (k+1)
  in 
  boucle i 0
;;

delta_etoile a_infini.initial "gvgfvgdivfqinfinisdbjhfbjhdbfq" a_infini;;

(* Pour jeudi, faire la fonction qui renvoie un booléen *)

let reconnu m a =
  List.mem (delta_etoile a.initial m a ) a.finals
;;

reconnu "eygoihdbddfazaginfiddfinfiniigoihagoze" a_infini;;

reconnu "eygoihdbddfazaginfiddfinfteriniigoihagoze" a_infini;;