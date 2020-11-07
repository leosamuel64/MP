(* exercice 1 *)

type graphe1 = int array array;;
type graphe2 = int list array;; 

let exemple_mat = [|  [|0;1;0;0|];
                      [|0;0;1;0|];
                      [|0;0;0;1|];
                      [|1;0;0;0|]
|];;

let exemple_tab = [|[1];[2];[3];[0]|];;

(* Exercice 1 *)

(* 1 *)

let compte_Matrice m=
  let res = ref 0 in
  for i=0 to Array.length m-1 do
    for j=0 to Array.length m-1 do
      if m.(i).(j) = 1 then
        res:=!res+1
    done;
  done;
  !res
;;

compte_Matrice exemple_mat;;

let compte_liste t=
  let res=ref 0 in
  for i=0 to Array.length t-1 do
    res:=!res+ List.length t.(i)
  done;
  !res
;;

compte_liste exemple_tab;;


(* Pour Samedi : questions 2,3 *)

(* 2 *)
let tableau_i n i=
	let res = Array.make n 1 in
	for j = 0 to n-1 do
		if i=j then
			res.(j) <- 0
  done;
  res
;;
	
let clique_mat n=
	let res = Array.make n [||] in
	for i=0 to n-1 do
		res.(i) <- tableau_i n i
	done;
	res
;;

clique_mat 2;;

	
let rec liste_sans_i i n=
	match n with
	|0 -> []
	|t when i <> t -> t::(liste_sans_i i (t-1))
	|t -> liste_sans_i i (t-1)
;;
	
let rec clique_tab n=
	let res = (Array.make n [] ) in
	for i=0 to n-1 do
		res.(i) <- liste_sans_i (i+1) n
	done;
	res
;;

clique_tab 4;;	
	
	
(* 3 *)
let chemin_mat l g=
	let res = ref true in
	for i=0 to Array.length l -2 do
		if g.(l.(i)).(l.(i+1)) = 0 then
			res := false
	done;
	!res
;;

chemin_mat [|1;2;3|] exemple_mat;;

let rec appartient x l=
	match l with
	|[] -> false
	|t::q when t = x -> true
	|t::q -> appartient x q
;;

let rec chemin_tab l g=
	match l with
	|[] -> true
	|[x] -> true
	|a::b::q when appartient b g.(a) -> chemin_tab (b::q) g
	|_ -> false
;;

chemin_tab [1;2;3] exemple_tab;;	

(* 4 *)

let miroir g=
  let n = Array.length g in
  let h = Array.make n [] in
  for i=0 to n-1 do
    let l = ref [] in
    for j=0 to n-1 do
      if List.mem i g.(j) then
        l:=(i::!l)
    done;
    h.(i) <- !l
  done;
  h
;;






(* Exercice 2 *)


let matrice_of_liste t=
  let n = Array.length t in
  let m = Array.make_matrix n n 0 in
  
  let rec aux l i m=
    match l with
    | [] -> ()
    | t::q -> m.(i).(t) <- 1; aux q i m;
  in

  for i=0 to n-1 do
    aux t.(i) i m
  done;
  m
;;

matrice_of_liste exemple_tab;;
    


let liste_of_matrice m=
  let n = Array.length m in
  let t = Array.make n [] in
  for ligne=0 to n-1 do
    for colonne=0 to n-1 do
      if m.(ligne).(colonne)=1 then
        t.(ligne) <- colonne::(t.(ligne))
    done;
  done;
      
  t
  ;;

liste_of_matrice exemple_mat;;

(* Exercice 3 *)

let matriceAleatoire n=
  let m = Array.make_matrix n n 0 in

  for i=0 to n-1 do
    for j=0 to n-1 do
      m.(i).(j)<-Random.int 2
        
    done;
  done;
  m
;;

matriceAleatoire 4;;


let matriceAleatoireNonOriente n=
  let m = Array.make_matrix n n 0 in
  for i=0 to n-1 do
    for j=0 to n-1-i do
      let r = Random.int 2 in
      m.(i).(j)<-r;
      m.(j).(i)<-r;
    done;
  done;
  m
;;

matriceAleatoireNonOriente 4;;