

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
  | Variable x -> (try
                    List.assoc x contexte
                  with
                  | Not_found -> false
                  )
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

let estSatisfiable formule=
  let variables = listeVariables formule in
  let contextes = listeContexte variables in
  
  let rec aux contexte =
    match contexte with
    | [] -> false
    | t::q -> (evaluation t formule) || aux q
  in
  aux contextes
;;

let exemple2 = Et(Non(Variable "p"),Variable "p");;
let exemple3 = Ou(Non(Variable "p"),Variable "p");;

estSatisfiable exemple1;;

let estTautologique formule=
  let variables = listeVariables formule in
  let contextes = listeContexte variables in
  
  let rec aux contexte =
    match contexte with
    | [] -> true
    | t::q -> (evaluation t formule) && aux q
  in
  aux contextes
;;
  
estTautologique exemple3;;

let plusLongue l1 l2=
  let a, b = List.length l1,List.length l2 in
  if a > b then
    l1
  else l2
;;

let sontEquivalentes f g=
  let variables = fusion_stricte ( listeVariables f) (listeVariables g) in

  
  let contextes = listeContexte variables in

  let rec aux contexte =
    match contexte with
    | [] -> true
    | t::q -> ((evaluation t f)=(evaluation t g)) && (aux q)
  in
  aux contextes
;;

sontEquivalentes exemple3 (Constante true);;

(* Dernière variante : liste des contexte où f est vraie *)

let contexte_verifiant f=
  let variable = listeVariables f in
  let contexte = listeContexte variable in
  List.filter 
  (fun c -> evaluation c f ) 
  contexte
;;



(* Exercice 8 *)

(* 1 *)

(* (A=>A)=>A *)
(* /(/A +A) + 1
A/A+A
0 + A
A *)

(* Alors ce n'est pas une tautologie *)

(* Par contre (A => /A) => /A  en est une*)


type formuleExo9 = Var of string | Cte of bool | NonEt of formuleExo9*formuleExo9;;

let rec vers_NonEt = function
  | Constante b -> Cte b
  | Variable x -> Var x
  | Et(a,b) -> NonEt(NonEt(vers_NonEt a,vers_NonEt b),NonEt(vers_NonEt a,vers_NonEt b))
  | Ou(a,b) -> NonEt(NonEt(vers_NonEt a,vers_NonEt a),NonEt(vers_NonEt b,vers_NonEt b))
  | Non(a) -> NonEt(vers_NonEt a,vers_NonEt a)
;;

let exempleconvert = vers_NonEt exemple1;;


(* Exercice 12 *)

type trileen = Vrai | Faux | Indetermine;;
type formule_12 = Var12 of int | Et12 of formule_12*formule_12 | Ou12 of formule_12*formule_12 | Non12 of formule_12;;

let ou a b =
  match a,b with
  | Faux,Faux -> Faux
  | Vrai,_ -> Vrai
  | _, Vrai -> Vrai
  | _ -> Indetermine
;;

let et a b=
  match a, b with
  | Vrai,Vrai -> Vrai
  | Faux,_-> Faux
  | _,Faux -> Faux
  | _ -> Indetermine
;;

let not a=
  match a with
  | Vrai -> Faux
  | Faux -> Vrai
  | Indetermine -> Indetermine
;;


let rec eval_paresseuse f v=
  match f with
  | Var12 b -> v.(b)
  | Et12 (a,b) -> let valeurdef = eval_paresseuse f v in
                  if valeurdef = Faux then Faux
                  else et (eval_paresseuse a v) (eval_paresseuse b v)
  | Ou12 (a,b) -> let valeurdef = eval_paresseuse f v in
                  if valeurdef = Vrai then Vrai
                  else ou (eval_paresseuse a v) (eval_paresseuse b v)
  | Non12 a -> not (eval_paresseuse a v)
;;

let test = Non12(Et12(Var12 0,Non12(Var12 1)));;
let contexte_test = [|Vrai;Faux;Indetermine|];;

type lit = V of int | F of int;;

let estSatisfiable12 f n=
  let tab = Array.make (n+1) Indetermine in
  let rec aux f=
    match f with
    | [] -> true
    | V i::q when tab.(i)=Faux -> false
    | F i::q when tab.(i)=Vrai -> false
    | V i::q -> tab.(i)<-Vrai; aux q
    | F i::q -> tab.(i)<-Faux; aux q
  in aux f
;;

let l = [V 0;V 1;F 2;F 1];;

estSatisfiable12 l 2;;

let fils noeud n=
  let lenNoeud = Array.length noeud in
  if lenNoeud < n then
    (
    let n1,n2 = Array.make (lenNoeud+1) Faux,Array.make (lenNoeud+1) Faux in
    for i=0 to lenNoeud-1 do
      n1.(i) <- noeud.(i);
      n2.(i) <- noeud.(i);
    done;
    n1.(lenNoeud) <- Faux;
    n2.(lenNoeud) <- Vrai;
    [n1;n2];
    )
  else
    [];
;;

fils [|Vrai;Vrai;Faux|] 3;;

let rec existe p noeud n=
  let len = Array.length noeud in
  if len < n then
    (
    let listeFils = fils noeud n in
      match listeFils with
      | [a;b] -> ou (existe p a n) (existe p b n)
      | _ -> failwith "Error"
    )
  else 
    p noeud
;;

let estSatisfiable12V2 p n=
  existe p [||] 10
  ;;
  

let predicat a=
  Indetermine;;

estSatisfiable12V2 predicat 10;;
  
