

type formule =
  | Variable of string
  | Non of formule
  | Et of (formule*formule)
  | Ou of (formule*formule)
  | Constante of bool;; 


let exemple1 = Et(Non(Ou(Variable "c",Variable "a")),Variable "b");;

type contexte = (string * bool) list;;

let rec evaluation contexte formule=
  match formule with
  | Constante b -> b
  | Variable x -> List.assoc x contexte
  | Non f ->not (evaluation contexte f)
  | Ou (f1,f2) -> (evaluation contexte f1) || (evaluation contexte f2)
  | Et (f1,f2) -> (evaluation contexte f1) && (evaluation contexte f2)
;;


let contexte = [("a", true);("b",false);("c",true)];;

evaluation contexte exemple1;;

(* Fonction pour récupérer la liste des variables apparaisant dans une formule *)

let rec fusion_stricte l1 l2=
  (* l1 et l2 sont strictemement croissante *)
  (* Sortie : la fusion, stictement croissante de ces listes *)
  match l1,l2 with
    | [], _-> l2
    | _, []-> l1
    | t1::q1, t2::q2 when t1=t2 -> t1::(fusion_stricte q1 q2)
    | t1::q1, t2::q2 when t1<t2 -> t1::(fusion_stricte q1 l2)
    | t1::q1, t2::q2 -> t2::(fusion_stricte l1 q2)
;;

let rec listeVariables formule=
  match formule with
  | Constante b -> []
  | Variable x -> [x]
  | Non f -> listeVariables f
  | Ou (f1,f2) -> fusion_stricte (listeVariables f1) (listeVariables f2)
  | Et (f1,f2) -> fusion_stricte (listeVariables f1) (listeVariables f2)
;;

listeVariables exemple1;;



let rec listeContexte listeVariables=
  match listeVariables with
  | [] -> [[]]
  | t::q -> (List.map (fun x -> (t,false)::x) (listeContexte q))
            @
            (List.map (fun x -> (t,true)::x) (listeContexte q))
;;
  
listeContexte ["a";"b"];;


(* faire les fonctions de la def *)