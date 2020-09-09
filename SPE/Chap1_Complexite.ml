(* -- Exercice 3 -- *)

(* -- 1 *)
let rec puissance x n=
  match n with
  | 0 -> 1
  | _ -> puissance x (n-1) * x
  ;;

puissance 2 3;;

(* Le cout du calcul de x^n est n  *)

let evalue p x =
  let rec aux p x n =
  match p with
  | [] -> 0
  | t::q -> t* (puissance x n) + aux q x (n+1)
  in aux p x 0
  ;;

evalue [1;3;2;5] 1;;

(* cout du calcul de P(x): sum(k=0 n)(k+1)=sum(k=1 n+1)(k)=(n+1)(n+2)/2 ~(n->+∞) n²/2 = Θ(n²)*)
(* La complexité est alors quadratique *)

(* -- 2 *)

let carre x=
  x*x;;

let rec puissance x n =
  match n with
  | 0 -> 1
  | 1 -> x
  | _ when n mod 2 = 0 -> carre (puissance x (n/2))
  | _ -> carre (puissance x (n/2))*x
;;

puissance 2 3;;

(* Le cout du calcul de P(x) par cette méthode est :  *)
(* sum(k=0 n)(ln(k)) = ln(prod(k=1 n k) = ln(n!) *)
(* Avec la formule de Stirling : n! ~ √(2π)√(n)n^n*e^{-n}*)
(* Cela ne tend pas vers 1 donc on peut appliquer ln et avoir une équivalence *)
(* ln(n!)= 1/2(ln(2π)+1/2ln(n)+nln(n)-n)= Θ(n*ln(n)) *)
(* La complexité est alors en Θ(nln(n)) -> le cout est quasi-linéaire *)

(* -- 3 Méthode de Horner *)

let rec evalue p x=
  match p with
  | [] -> 0
  | a::q -> a+x*(evalue q x)
;;

(* Le cout est linéaire : Θ(n) *)


(* -- Exercice 2 -- *)

let rec lineaire n=
  if n=0 then 0
  else lineaire (n-1)
  ;;


let quadratique n=
  let res = ref 0 in
  for i=0 to (n-1) do
    for j=0 to (n-1) do 
      res := !res + i - j
    done;
  done;
  !res
      ;;


let rec quasi n =
  match n with
  | 0 -> 1
  | 1 -> 0
  | _ when n mod 2 = 0 -> carre (quasi (n/2))
  | _ -> carre (quasi (n/2))*0
;;


let rec loga n =
  match n with
  | 1 -> 0
  | _ -> loga (n/2)
;;


(* -- Exercice 5 *)



(* -- 2 *)

let bouge i j =
  (*Affiche le deplacement d'un anneau*)
  print_int i;print_string"->";print_int j;print_char '\n';
;;

let rec hanoi n d a i=
  if n = 0 then
      ()
  else begin
      hanoi (n-1) d i a;
      bouge d a;
      hanoi (n-1) i a d; 
  end
;;

hanoi 3 0 2 1;;

(* On compte le nombre d'appel à bouge : on a C(n+1)=2*C(n)+1 donc C(n)=2**n-1) d'ou une complexité en O(2**n) *)





