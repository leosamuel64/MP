

let chemin sd sa l n p=
  (*  Entrées : <int> sd -> sommet de départ  
                <int> sa -> sommet d'arrivé
                <int list Array> l -> un labyrinthe
                <int> n,p -> dimension du labyrinthe l
      Sortie :  <int list> -> un chemin de l entre sa et sd
  *)
  let deja_vu = Array.make (n*p) false in
  let rec visite_sommet si path =
    deja_vu.(si)<-true;
    if si = sd then 
      path
    else
      visite_voisins si path l.(si) 
  and visite_voisins si path = function
    | [] -> []
    | t::q when not(deja_vu.(t)) -> 
                    let essai = visite_sommet t (t::path) in
                      if essai = [] then
                        visite_voisins si path q
                      else
                        essai
    |_::q -> visite_voisins si path q
    
    in visite_sommet sa [sa]
;;



let voisins n p k =
  (*  Entrées :   <int> n,p -> dimension du labyrinthe
                  <int> k -> sommet
  *)
  let var = [|p; -p; 1; -1|] 
  and prod = n*p
  and res = ref [] in

  for i=0 to 3 do
    let v=k+var.(i) in
      if v < prod && v>=0 then
        res:= v::!res
  done;
  !res
;;

voisins 3 3 4;;
(* int list = [3; 5; 1; 7] *)
voisins 3 3 0;;
(* int list = [1; 3] *)



let rec triInsertion l = 
  (*  Entrée :  <list> l -> liste en desordre
      Sortie :  <list> -> liste l triee 
  *)
  let rec insertion x = function
    | [] -> [x]
    | t::q when t>x -> x::t::q
    | t::q -> t::(insertion x q)
  in
  match l with
      | [] -> []
      | t::q -> insertion t (triInsertion q)
;;

triInsertion [1;3;3;5;4;8;2];;
(* int list = [1; 2; 3; 3; 4; 5; 8] *)

let voisins_desordre n p k=
  (*  Entrées :   <int> n,p -> dimension du labyrinthe
                  <int> k -> sommet
  *)
  let var = [|p; -p; 1; -1|] 
  and prod = n*p
  and res = ref [] in

  for i=0 to 3 do
    let v=k+var.(i) in
      if v < prod && v>=0 then
        res:= (Random.float 1.,v)::!res
  done;

  let melange = triInsertion !res in
  let rec decoupled= function
    | [] -> []
    | (a,b)::q -> b::(decoupled q)

  in decoupled melange
;;

voisins_desordre 6 6 14;;
(* int list = [8; 20; 15; 13] *)
voisins_desordre 6 6 14;;
(* int list = [13; 20; 8; 15] *)
voisins_desordre 6 6 14;;
(* int list = [15; 13; 20; 8] *)

let creer_Graphe n p=
  (*  Entrées : <int> n,p -> dimension du labyrinthe
      Sortie :  <int list array> -> le graphe du "labyrinthe" sans mur
  *)
  let prod = n*p in
  let res = Array.make prod [] in
  for k=0 to prod-1 do
      res.(k) <- voisins_desordre n p k;
  done;
  res 
;;

creer_Graphe 3 3;;
(* int list array = [|[3; 1]; [4; 0; 2]; [1; 5; 3]; 
                      [4; 2; 6; 0]; [7; 5; 1; 3]; [4; 2; 6; 8];
                      [3; 5; 7]; [4; 6; 8]; [7; 5]|] 
*)
creer_Graphe 2 2;;
(* int list array = [|[2; 1]; [0; 3; 2]; [1; 0; 3]; [2; 1]|] *)

let labyrinthe_aleatoire n p=
    (*  Entrées : <int> n,p -> dimension du labyrinthe
      Sortie :  <int list array> -> le graphe du labyrinthe avec des murs
  *)
  let prod = n*p in
  let deja_vu = Array.make prod false
  and laby = Array.make prod [] in

  let ajoute_arete sa sb=
    (* Ajoute une arete entre les sommets a et b *)
    laby.(sa) <- sb::laby.(sa);
    laby.(sb) <- sa::laby.(sb);
    in

  let rec visite_sommet k=
    deja_vu.(k) <- true;
    visite_voisins k (voisins_desordre n p k)

  and visite_voisins k = function
    | [] -> ()
    | t::q when not(deja_vu.(t)) -> ajoute_arete k t;
                                    visite_sommet t;
                                    visite_voisins k q;
    | t::q -> visite_voisins k q

    in visite_sommet 0;
    laby
  ;;

labyrinthe_aleatoire 3 3;;
(* int list array = [|[1]; [2; 0]; [3; 1]; 
                      [4; 2]; [7; 3]; [6; 8]; 
                      [5]; [8; 4]; [5; 7]|] 
*)
labyrinthe_aleatoire 2 3;;
(* int list array = [|[1]; [4; 0]; [3; 5]; [2]; [5; 1]; [2; 4]|] *)
labyrinthe_aleatoire 2 2;;
(* int list array = [|[1]; [3; 0]; [3]; [2; 1]|] *)
labyrinthe_aleatoire 2 2;;
(* int list array = [|[2]; [3; 2]; [1; 0]; [1]|] *)
labyrinthe_aleatoire 2 2;;
(* int list array = [|[1]; [2; 0]; [3; 1]; [2]|] *)





type graphe = int list array;;

let laby_exemple = [|[10]; [2; 11]; [12; 1]; [4]; [3; 14]; [15; 6]; [5; 7]; [6; 8; 17];
    [9; 7]; [19; 8]; [11; 0]; [1; 10]; [13; 2]; [23; 12]; [4; 24; 15];
    [14; 5]; [26]; [7; 27]; [19]; [18; 29; 9]; [30; 21]; [20; 22]; [21; 23];
    [22; 13]; [25; 14]; [26; 24]; [16; 25]; [17; 28]; [27; 38]; [39; 19];
    [31; 20]; [41; 30]; [33; 42]; [43; 32]; [35; 44]; [36; 34]; [37; 35];
    [38; 36]; [28; 37]; [49; 29]; [41]; [40; 42; 31]; [32; 41]; [44; 33];
    [34; 43]; [46]; [45; 47]; [46; 48]; [47; 49]; [48; 39]|] ;;


(*- Affichage -*)
let affiche laby n p =

  let rec aux k=
    (* 
       k : numéro de la case à lire
     *)
    if k= n*p then ()
    else begin
        let v = laby.(k) in
        if List.mem (k-1) v then print_char ' ' else print_char '|' ;
        if List.mem (k+p) v then print_char ' ' else print_char '_' ;
        if (k+1) mod p = 0 then print_string "|\n";
        aux (k+1)
      end

  in

  (* La ligne du dessus : *)
  for i =0 to p-1 do
    print_string " _"
  done ;
  print_char '\n';

  aux 0
;;
(*- fin -*)

affiche (labyrinthe_aleatoire 5 10) 5 10;;

