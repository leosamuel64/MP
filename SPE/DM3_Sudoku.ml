(* 1.3 *)
(* 1 *)

let rec appartient x l =
  match l with
  | [] -> false
  | t::_ when t=x -> true
  | t::q -> appartient x q
;;

appartient 3 [1;4;3;2;8];;
(* bool = true *)
appartient 10 [1;4;3;2;8];;
(* bool = false *)

(* 2 *)

let rec suppression x l=
  match l with
  | [] -> []
  | t::q when t=x -> suppression x q
  | t::q -> t::(suppression x q)
;;

suppression 3 [3;4;4;3;1;4];;
(* int list = [4; 4; 1; 4] *)

(* 3 *)

let ajoute x l=
  let rec aux x l flag=
    match l with
    | [] when not flag -> [x]
    | [] -> []
    | t::q when t=x -> t::(aux x q true)
    | t::q -> t::(aux x q flag)
  in aux x l false
;;
    
ajoute 3 [1;2;3;4;5];;
(* int list = [1; 2; 3; 4; 5] *)

ajoute 3 [1;2;4;5];;
(* int list = [1; 2; 4; 5; 3] *)

(* 4 *)



let rec indice (b,r)=
  let i = (b mod 3)*3 + (r mod 3) in
  let j = (b/3)*3 + r/3 in
  (i,j)
;;

indice (3,6);;
(* int * int = (0, 5) *)
indice (4,2);;
(* int * int = (5, 3) *)
indice (1,8);;
(* int * int = (5, 2) *)

(* 2.1 *)

type litteral = X of int*int*int | NonX of int*int*int ;;

(* e *)

let case1 () =
  let res = ref [] in
  for i=0 to 8 do
    for j=0 to 8 do
      let temp = ref [] in
      for k=1 to 9 do
        temp:= (X(i,j,k)) :: !temp;
      done;
    res:= !temp :: !res;
    done;
  done;
  !res
;;

case1 ();;

(* 2 *)

let bloc1 ()=
  let res = ref [] in
  for b=0 to 8 do
    for k=1 to 9 do
      let temp = ref [] in
      for r=0 to 8 do
        let i,j = indice (b,r) in
        temp:= X(i,j,k) :: !temp;
      done;
      res:= !temp :: !res
    done;
  done;
  !res
;;

let ligne2 ()=
  let res = ref [] in
  for i=0 to 8 do
    for k=1 to 9 do
      for j1=0 to 7 do
        for j2=j1 to 8 do
          res:= [NonX(i,j1,k);NonX(i,j2,k)] :: !res;
        done;
      done;
    done;
  done;
  !res
;;
      (* if q=k then 
      res:= [X(i,j,q)] :: !res;
    else
      res:= [NotX(i,j,q)]::!res; *)

let donnees t=
  let res = ref [] in
  for i=0 to 8 do
    for j=0 to 8 do
      match t.(i).(j) with
      | 0 -> ()
      | k ->  for q=1 to 9 do
                if q=k then 
                  res:= [X(i,j,q)] :: !res
                else
                  res:= [NonX(i,j,q)]::!res
              done;
    done;
  done;
;;

let interdites_ij t i j=
  let res = ref [] in
  let b = 3*(i/3)+(j/3) in
  for l=0 to 8 do
    if t.(l).(j) <> 0 then
      res := ajoute (NonX(i,j,t.(l).(j))) !res;
    if t.(i).(l) <> 0 then
      res := ajoute (NonX(i,j,t.(i).(l))) !res;
    let i1,j1 = indice(b,l) in
    if t.(i1).(j1) <> 0 then
      res := ajoute (NonX(i,j,t.(i1).(j1))) !res;
  done;
!res ;;

let interdites t =
  let res = ref [] in
  for i = 0 to 8 do 
    for j = 0 to 8 do
      if t.(i).(j)=0 then
        res := (interdites_ij t i j) :: !res;
    done; 
  done;
!res;;


let rec nouveau_lit_isole f=
  match f with
  | [] -> X(-1,-1,-1)
  | [c]::q -> c
  | _::q -> nouveau_lit_isole q
;;

let non x=
  match x with 
  | X(i,j,k) -> NonX(i,j,k)
  | NonX(i,j,k) -> X(i,j,k)
;;

let rec simplification l f=
  match f with
  | [] -> []
  | t::q when appartient l t -> simplification l q
  | t::q -> suppression (non l) t :: simplification l q
;;

let rec propagation t f=
  match nouveau_lit_isole f with
  | X(-1,-1,-1) -> f
  | X(i,j,k) -> t.(i).(j) <-k; 
                propagation t (simplification (X(i,j,k)) f)
  | l -> propagation t (simplification l f)
;;

let variables f = 
  let res = ref [] in
  let rec ajouter_clause c = 
    match c with
    | [] -> ()
    | X(i,j,k)::q ->  res := ajoute (X(i,j,k)) !res; 
                      ajouter_clause q
    | NonX(i,j,k)::q -> res := ajoute (X(i,j,k)) !res; 
                        ajouter_clause q in
  let rec ajouter_formule f = 
    match f with
    | [] -> ()
    | t::q -> ajouter_clause t; 
                  ajouter_formule q
  in ajouter_formule f;
  !res
;;

let copie_matrice m=
  let n,p=Array.length m,Array.length m.(0) in
  let nvmat = Array.make_matrix n p 0 in
  for i=0 to n do
    for j=0 to p do
      nvmat.(i).(j) <- m.(i).(j);
    done;
  done;
  nvmat
;;

let deduction t x f = 
  let t1 = copie_matrice t in 
  match x with
  | X(i,j,k) when propagation t1 ([NonX(i,j,k)] :: f) = [[]] -> 1
  | _ -> let t2 = copie_matrice t 
  in
  if propagation t2 ([x] :: f) = [[]] then 
    -1 
  else 
  0
;;



