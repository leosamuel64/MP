
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

let deltaEtoile_d l m a=
  (*  l : liste d'états
      m : mot
      a : un afnd

      Sortie : la réunion pour q dans l de delta_etoileND q m a
  *)
    let tous_les_delta_etoile_i_m = List.map (fun i -> delta_etoileND i m a) l in
    union_sans_doublon tous_les_delta_etoile_i_m 
;;

let delta_d l x a=
  (*  l : liste d'états
      m : mot
      a : un afnd

      Sortie : la réunion pour q dans l de delta_etoileND q m a
  *)
  union_sans_doublon (List.map (fun r -> deltaND r x a) l)
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
  let l_delta = deltaEtoile_d q m a in
  let l_f = a.finalsND in
  
  aux l_delta l_f
;;

let rec inter l_delta l_f =
  match l_delta,l_f with
  | [],_ | _,[] -> []
  | t1::q1,t2::q2 when t1>t2 -> (inter (t1::q1) (q2))
  | t1::q1,t2::q2 when t1<t2 -> (inter q1 (t2::q2))
  | t1::q1, t2::q2 -> t1::inter q1 q2
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



let numerotation_det a alphabet =
  (* a: AND 
  sortie : une table de Hashage qui associe à chaque état accessible de \mathcal{A}_d un numéro et un tableau
            qui associe à un numéro l'état correspondant *)

  let dico = Hashtbl.create 42 in
  let dico_inverse = Hashtbl.create 42 in
  (* Le tableau sera créer à la fin : pour l'instant on ne connait pas sa longueur *)
  let i = ref 0 in 
  (* prochain numéro à utiliser *)

  let rec visite_sommet q =
    if not (Hashtbl.mem dico q) then
      (
      Hashtbl.add dico q !i;
      Hashtbl.add dico_inverse !i q;
      incr i;
      visite_voisins (List.map (fun x -> delta_d q x a) alphabet)
      )
    
  and visite_voisins = function
  | [] -> ()
  | q::autres_voisins ->  visite_sommet q; 
                          visite_voisins autres_voisins;

  in 
  visite_sommet a.initiauxND;
(* A ce stade, nous avons ;
-le dico bien rempli
- !i est le nombre d'états accessibles de \mathcal{A}_d *)

(!i, dico, dico_inverse)
;;

numerotation_det exemple_ici_ND;;

let rec intervalle a b=
  if a>=b then []
    else a::(intervalle (a+1) b)
;;
  

let determinised a alphabet =
  let (n, dico, dico_inv) = numerotation_det a alphabet in
  
  let etat_of_int i= Hashtbl.find dico_inv i
  and int_of_etat q = Hashtbl.find dico q in

  let transitions_de_ad = Array.make n [] in

  for i=0 to n-1 do
    transitions_de_ad.(i) <-
                          List.map (fun x -> (x,int_of_etat (delta_d (etat_of_int i) x a))) 
                          alphabet;
  done;

  (* recherche des états finals de ad *)
  let etats_finals = List.filter (fun i -> inter (a.finalsND) (etat_of_int i) <> [] )
                                  (intervalle 0 n) in
let ad =
  {
    initial = int_of_etat a.initiauxND;
    finals = etats_finals;
    transitions = transitions_de_ad
  }
  in emonde ad;
  ad
;;

(* Application : ecrire un programme qui prend un mot m et qui renvera un AFD reconnaissant Σ* m Σ* *)

let construit_Automate m alphabet=
  let n = String.length m in
  let t = Array.make (n+1) [] in
  t.(0) <- (1,m.[0])::(tout_versND 0 alphabetLatin);
  t.(n) <- tout_versND n alphabetLatin;

  for i=1 to n-1 do 
    t.(i) <- (i+1,m.[i])::t.(i)
  done;

  let a_nd = {
    initiauxND=[0];
    finalsND = [n];
    transitionsND = t
  } in

  determinised a_nd alphabet
;;

let a= construit_Automate "hello_world" alphabetLatin;;

reconnu "lenjbkhvhjvdnvdshjbkqjhbdzjchvjqsvdjhvjdsv hello_world dbfhbdhfbhdfb" a;;
reconnu "lenjbkhvhjvdnvdshjbkqjhbdzjchvjqsvdjhvjdsv hell_world dbfhbdhfbhdfb" a;;

(* Exercice 33 *)

(* 1- c'est delta_d *) 

(* 2- *)

  let indice_fin_motif m a=
    let rec boucle i l=
      (*  i : indice de la prochaine lettre à lire.
          l : liste des états accessible en lisant *)
      (* On calcul pour tout r ∈ l, la liste δ(r,m.(i)) *)
      if inter (a.finalsND) l <> [] then
        (i,l)
      else if i =String.length  m then (String.length m,inter a.finalsND l)
      else
      (
        let tous_les_deltas_r_mi = List.map (fun r->deltaND r m.[i] a) l
        in
        let nv_l = union_sans_doublon tous_les_deltas_r_mi in
        boucle (i+1) nv_l
      )
      
    in boucle 0 a.initiauxND
  ;;

let on_peut_rester a alphabet=
  let nv_trans = Array.copy a.transitionsND in
  let rec aux l=
    match l with
    | [] ->()
    | t::q -> nv_trans.(t) <- (tout_versND t alphabet) @ nv_trans.(t);aux q
  in aux a.initiauxND;
  {
    initiauxND=a.initiauxND;
    finalsND = a.finalsND;
    transitionsND = nv_trans
  }
;;

let retourned a nv_initiaux=
  let n = Array.length a.transitionsND in
  let nv_trans = Array.make n [] in

  let rec ajoute l i=
    match l with 
    | [] -> ()
    | (n,c)::q -> nv_trans.(n) <- (i,c)::nv_trans.(n);
                  ajoute q i
  in

  for i=0 to n-1 do
    ajoute a.transitionsND.(i) i;
  done;

  {
    initiauxND=nv_initiaux;
    finalsND = a.initiauxND;
    transitionsND = nv_trans
  }
;;
    
let miroir mot=
  let rec aux i=
    if i=String.length mot then
      ""
    else
      (aux (i+1))^(String.make 1 mot.[i])
    in aux 0
;;


let juste_ici_ND = {initiauxND = [0];
                      finalsND = [3];
                      transitionsND = [|
                                        [(1,'i')];
                                        [(2,'c')];
                                        [(3,'i')]
                                      |]
                      };;



let place_motif m a alphabet=
  let j,l = indice_fin_motif m (on_peut_rester a alphabet) in
  let ret_a = retourned a l in
  let i,_=indice_fin_motif (miroir (String.sub m 0 j)) ret_a in
  (i,j)
;;

place_motif "bhkicidq;ghv" juste_ici_ND alphabetLatin;;

;;




(* PARTIE 2 *)

type regexp =
  | Lettre of char
  | Point of (regexp * regexp)
  | Plus of (regexp * regexp)
  | Etoile of regexp
;;


let rec contient_eps regexp =
  match regexp with
  | Lettre(_) -> false
  | Point(e,f) -> contient_eps e && contient_eps f
  | Plus(e,f) -> contient_eps e || contient_eps f
  | Etoile(_) -> true
;;

let exemple =
  Point(Etoile (Lettre 'a'), Point(Plus((Lettre 'b'),(Lettre 'c')),Etoile(Lettre 'b')));;


(* langages locaux *)

(* ecrivons des fonctions pour recup les paramètres d'un langage local *)

let rec trouve_P = function
(*  entrée : Une regexp e 
    sortie : Liste des premieres lettres de e *)
  | Etoile(f) -> trouve_P f
  | Plus(f,g) -> fusion_stricte (trouve_P f) (trouve_P g)
  | Lettre a -> [a]
  | Point(f,g) when contient_eps f -> fusion_stricte (trouve_P f) (trouve_P g)
  | Point(f,_) -> trouve_P f
;;

trouve_P exemple;;

let rec trouve_S = function
(*  entrée : Une regexp e 
    sortie : Liste des premieres lettres de e *)
  | Etoile(f) -> trouve_S f
  | Plus(f,g) -> fusion_stricte (trouve_S f) (trouve_S g)
  | Lettre a -> [a]
  | Point(f,g) when contient_eps f -> fusion_stricte (trouve_S f) (trouve_S g)
  | Point(_,g) -> trouve_S g
;;

let rec produit l1 l2 =
  let rec e_avec_l e l=
    match l with
    | t::q -> (e,t)::e_avec_l e q
    | [] -> []
  in
  match l1 with
  | t::q -> fusion_stricte (e_avec_l t l2) (produit q l2)
  | [] -> []
;;

let rec trouve_F = function
  (* Sortie : liste de couple de char *)
  | Lettre _ -> []
  | Etoile x -> fusion_stricte  (produit (trouve_P x) (trouve_S x)) 
                                (trouve_F x)
  | Plus(f,g) -> fusion_stricte (trouve_F f) 
                                (trouve_F g)
  | Point(f,g) -> fusion_stricte  (fusion_stricte   (trouve_F f) 
                                                    (trouve_F g)) 
                                  ((produit (trouve_P f) (trouve_S g)))
;;

trouve_F exemple;;


(* programmation de l'automate associé à un langage local *)

let rec lettre_dans_e e =
(* Finir la fx avec chaque lettre associé a un nombre int. *)
  let rec aux e=
    match e with
    | Lettre a -> [a]
    | Etoile x -> aux x
    | Plus (f,g) -> fusion_stricte (aux f) (aux g)
    | Point (f,g) -> fusion_stricte (aux f) (aux g)
  in
  let rec assoc l i=
    match l with
    | [] -> []
    | t::q -> (i,t)::assoc q (i+1)
  in
assoc (aux e) 0
;;

lettre_dans_e exemple;;



let carre x=
  x*x
;;

let cube x y=
  x*y*x
;;

let rond f g x =
  f (g x)
;;


let f = rond rond rond;;
(* f a b i1 i2 i3 = b (a i1 i2) i3 *)

f carre max (-5) 2;;






