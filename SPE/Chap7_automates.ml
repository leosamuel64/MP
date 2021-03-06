
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
  
let alphabetLatin = list_of_string "abcdefghijklmnopqrstuvwxyz ABCDEFGHIJQLMNOPQRSTUVWXYZ-_+=-&,?.;:!";;


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
  List.mem (delta_etoile a.initial m a) a.finals
;;

reconnu "eygoihdbddfazaginfiddfinfiniigoihagoze" a_infini;;

reconnu "eygoihdbddfazaginfiddfinfteriniigoihagoze" a_infini;;




exception Blocage;;

let delta2 i x a=
  (*  a : automate
      x : une lettre
      i : un état de a
  *)
  let rec aux = function
    | [] -> raise Blocage
    | (lettre, etat)::_ when lettre = x -> etat 
    | _::q -> aux q
    in
    aux (a.transitions.(i))
;;

let delta_etoile2 i m a =
  (* Cette fois m est une chaine de caracteres.
      Renvoie l'etat atteint apres lecture de toutes les lettres de m *)
  
  let rec boucle q k =
      (* k : prochaine lettre de m à lire
          q : etat actuel *)
          if k = String.length m then
              q
          else
              boucle (delta2 q m.[k] a ) (k+1)
  in 
  boucle i 0
;;

let reconnu2 m a =
  try
    List.mem (delta_etoile2 a.initial m a) a.finals
  with
    | Blocage -> false
;;

let a_telAndore =
{
initial = 0;
finals = [6];
transitions = [|[('0',1)];
                tout_vers 2 (list_of_string("0123456789"));
                tout_vers 3 (list_of_string("0123456789"));
                []
              |]
}
;;

let completed a alphabet=
  let n = Array.length a.transitions in
  let p = n in
  let nv_trans = Array.make (p+1) (tout_vers p alphabet) in
  for i=0 to n-1 do
    nv_trans.(i) <- (a.transitions.(i)@ nv_trans.(i));
  done;
  {
    initial = a.initial;
    finals = a.finals;
    transitions=nv_trans
  }
;;

completed a_telAndore (list_of_string "0123456789");;
  

(* Exercice 21 *)

(* On garde les transitions de A mais on remplace les numéros dans finals par ceux qui n'y sont pas *)

let auto_complementaire_aux a=
  let n = Array.length (a.transitions) in
  let rec boucle i=
    if i=n-1 then
      []
    else
      if List.mem i a.finals then
        boucle (i+1)
      else
        i::(boucle (i+1))
    in
    {
    initial = a.initial;
    finals = (boucle 0);
    transitions=a.transitions
    }
;;

(* 2 - On le complete avant et on refait la même chose *)

let auto_complementair_auxe a alphabet=
  auto_complementaire_aux (completed a alphabet);;
  

(* Faire les exos 16 et 6 *)

let rec accessible trans i=
  let n = Array.length trans in
  let deja_vu = Array.make n false in
  let rec visite_sommet s=
    if deja_vu.(s) then []
    else (
      deja_vu.(s) <- true;
      s::visite_voisins trans.(s)
    )
  and visite_voisins = function
    | [] -> []
    | (_,q)::suite -> (visite_sommet q) @ (visite_voisins suite)
  in
    visite_sommet i
;;


accessible a_infini.transitions 2;;

let etats_utiles a =
  let trans = a.transitions in
  let n = Array.length trans in
  let deja_vu = Array.make n false in
  let coaccessible = Array.make n false in
    
    let rec visite_sommet s chemin_parcouru=
      if deja_vu.(s) then []
      else (
        deja_vu.(s) <- true;
        if List.mem s (a.finals) then
          List.iter (fun q -> coaccessible.(q)<- true) 
                    (s::chemin_parcouru);
        s::visite_voisins (s::chemin_parcouru) trans.(s)
      )
    and visite_voisins chemin_parcouru = function
      | [] -> []
      | (_,q)::suite -> (visite_sommet q chemin_parcouru) @ (visite_voisins chemin_parcouru suite)
    in
      List.filter (fun q -> coaccessible.(q)) 
                  (visite_sommet a.initial [])
;;

etats_utiles a_infini;;
  

(* terminer le travail

Supprimer les états inutiles (supprimer les transitions...) *)

let emonde a=
  let liste_etats_utiles = etats_utiles a in
  let n = Array.length a.transitions in

  let sans_transition_inutile l=
    List.filter (fun (_,q) -> List.mem q liste_etats_utiles) l
  in

  for i=0 to n-1 do
    if not (List.mem i liste_etats_utiles) then
      a.transitions.(i) <- []
    else
      a.transitions.(i) <- sans_transition_inutile a.transitions.(i)
  done;
;;


let ex_a_emonder = {
	initial = 0 ;
	finals = [4] ;
	transitions = [|[('b',1); ('a',3); ('c',2)];[('c',2)];[];[('b',3);('c',4)];[];[ ('c',3); ('a',4)]|]
};;

emonde ex_a_emonder;;

 ex_a_emonder;;

(* Exercice 22 : Langage vide ?*)

let langageReconnuEstVide a=
  emonde a;
  a.transitions.(a.initial)=[] && not (List.mem (a.initial) a.finals)
;;

let ex_vide = {
	initial = 0 ;
	finals = [4] ;
	transitions = [|[('a',3)];[];[];[];[];[]|]
};;

langageReconnuEstVide ex_vide;;



type afnd =
  {
  initiauxND : int list;
  transitionsND : (int*char) list array;
  finalsND : int list
  }   
;;
  (* On ne peut pas avoir les mêmes nom de champ que les afd *)



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

let rec partition = function
  | [] -> [],[]
  | [t] -> [t],[]
  | s::t::q -> let l1,l2 = partition q in s::l1,t::l2
;;

let rec tri_strict = function
  | [] -> []
  | [t] -> [t]
  | l -> let l1,l2 = partition l in
          fusion_stricte (tri_strict l1) (tri_strict l2)
;;

let union_sans_doublon l=
  List.fold_left fusion_stricte [] l
;;

let deltaND q x a=

  (* Transitions depuis q étiquetées par x : *)
  let transition_depuis_q_avec_un_x = List.filter   (fun(_,lettre) -> lettre=x) 
                                                    a.transitionsND.(q) in
  tri_strict (List.map fst transition_depuis_q_avec_un_x)
;;

let delta_etoileND q m a=
  let rec boucle i l=
    (*  i : indice de la prochaine lettre à lire.
        l : liste des états accessible en lisant *)
    (* On calcul pour tout r ∈ l, la liste δ(r,m.(i)) *)
    if i = String.length m then l
    else(
      let tous_les_delta_r_mi = List.map (fun r -> deltaND r m.[i] a) l in
      let nv_l = union_sans_doublon tous_les_delta_r_mi in
      boucle (i+1) nv_l
    )
  in boucle 0 q
;;

let delta_d l m a=
  (*  l : liste d'états
      m : mot
      a : un afnd

      Sortie : la réunion pour q dans l de delta_etoileND q m a
  *)

    let tous_les_delta_etoile_i_m = List.map (fun i -> delta_etoileND i m a) l in
    union_sans_doublon tous_les_delta_etoile_i_m 
;;
      


      (* Finir avec l'intersection pour jeudi
      + exercice 24 *)

let intersection q m a= 
  let rec aux l_delta l_f =
    match l_delta,l_f with
    | [],_ | _,[] -> []
    | t1::q1,t2::q2 when t1>t2 -> (aux (t1::q1) (q2))
    | t1::q1,t2::q2 when t1<t2 -> (aux q1 (t2::q2))
    | t1::q1, t2::q2 -> t1::aux q1 q2
  in
  let l_delta = delta_d q m a in
  let l_f = a.finalsND in
  
  aux l_delta l_f
;;

let reconnuND q m a=
  match intersection q m a with
  | [] -> false
  | _ -> true
;;

(* exemple contenant le facteur "ici" *)
let tout_versND i alphabet=
  List.map (fun x -> (i,x) ) alphabet
;;

let exemple_ici_ND = {initiauxND = [0];
                      finalsND = [3];
                      transitionsND = [|(1,'i')::(tout_versND 0 alphabetLatin);
                                      [(2,'c')];
                                      [(3,'i')];
                                      tout_versND 3 alphabetLatin
                                      |]
                      };;


(* Exercice 28 *)

let construit_Automate m =
  let n = String.length m in
  let t = Array.make (n+1) [] in
  t.(0) <- tout_versND 0 alphabetLatin;
  t.(n) <- tout_versND n alphabetLatin;
  for i=1 to n-1 do 
    t.(i)<-[(i+1,m.[i])]
  done;

  {
    initiauxND=[0];
    finalsND = [n];
    transitionsND = t
  }
;;
  
construit_Automate "Endomorphisme auto-adjoint";;


