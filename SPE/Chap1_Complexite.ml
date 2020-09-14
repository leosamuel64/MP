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

(* Dans une liste, on a 2**n sous ensemble *)

let rec slists = function
  | [] -> [[]]
  | x::s -> let l = slists s in
                  (List.map (fun sl -> x::sl) l)@l;; 

slists [0;1;2;3;4];;

(* ∀ n ∈ ℕ, soit C(n) le nombre de :: dans slists l lorsque |l|=n *)
(* C0= 0 *)
(* ∀ n ∈ ℕ*, C(n) = C(n-1)+ O(2**(n-1))+ O(2**(n-1))        le premier O pour le list.map et le deuxieme pour le @ *)
(* C(n)=C(0)+ Σ(k=1,n) O(2**k)= O(2**n) *)


(* -- Exercice 7 *)

(* -- 1 *)

let tTest = [|1;2;3;4;3;6;5;3|];;

(* let partitionTableau t=
  let a = Array.make (Array.length t/2) 0 in
  if (Array.length t/2) mod 2 = 0 then
    let b = Array.make (Array.length t/2) 0 in
  else let b = Array.make (Array.length t/2+1) 0
  
  for i=0 to (Array.length t)-1 do *)

(* Ne pas faire de nouveau tableau *)

let maxTab t =
  let rec aux deb fin =
    (* renvoie le maximum de t.(deb:fin) *)
    if deb=fin+1 then
      t.(deb)
    else (
      let m = (deb+fin)/2 in
      max 
        (aux deb m)
        (aux m fin)
        
    )
  in aux 0 (Array.length t)
;;

(* Comptons le nombre d'appel à aux. On a C(n)= C(n/2) + C(n+1/2) + O(1)  *)
(* On a α+β=2, α = log2(2)=1, β=0 donc α>β On a alors C(n)=O(n) *)

(* -- 3 *)

(* Soit t un tableau et n sa longueur.Pour calculer son max il faut avoir vu au moins 1 fois chaque éléments *)
(* Comme une comparaison fait intervernir 2 elements, il faut au moins n/2 comparaison soit un O(n) *)
(* Complexité au moins linéaire *)



(* maxTab tTest;;





  

(* -- Exercice 4 *)

(* -- 1 : Méthode 1 *)

(* - a *)

let rec dedoublonnage1 l x=
  let rec aux l x i=
    match l with
    | [] -> []
    | t::q when t=x && i=0 -> t::(aux q x 1)
    | t::q when t=x && i=1 -> aux q x i
    | t::q -> t::(aux q x i)
    in aux l x 0
    ;; *)

(* dedoublonnage1 [1;2;3;2;1;3;4;3;2;4;5] 3;; *)

let rec enleve l x=
  match l with
  | [] -> []
  | t::q when t=x -> enleve q x
  | t::q -> t::(enleve q x)
;;

enleve [1;2;3;2;1;3;4;3;2;4;5] 3;;

let rec enleveSaufUn l x=
  match l with
  | []	-> []
  | t::q when t=x -> t::(enleve q x)
  | t::q -> t::enleveSaufUn q x
  ;;

let rec dedoublonnage l=
  match l with
  | [] -> []
  | t::q -> t::(dedoublonnage (enleve q t))
  ;;

dedoublonnage [1;2;3;2;1;3;4;3;2;4;5];;

(* On note n la longueur de la liste : on a une complexité en O(n²) *)

(* Chaque application de "enleve" a un cout en O(n). On applique n fois cette fonction dans le pire des cas *)
(* D'où une complexité en O(n²) *)

(* -- 2 *)

(* - a *)

(* Une relation d'ordre sera representée par une fonction du type 'a -> 'a -> bool *)

let rec divise = function
    | []-> [],[]
    | [a]->[a],[]
    | a::b::q-> let l1,l2 = divise q in 
                                      a::l1,b::l2
;;

let rec fusion l1 l2 o=
  match l1,l2 with
  | [],_->l2
  | _,[]->l1
  | t1::q1,t2::q2 when  o t1 t2 -> t1::(fusion q1 l2 o)
  | t1::q1,t2::q2 -> t2::(fusion l1 q2 o)
;;

let rec tri l o=
  match l with
  | []->[]
  | [a]->[a]
  | _->let l1,l2 = divise l in fusion (tri l1 o) (tri l2 o) o
;;

let numerote l=
  let rec aux l i=
    match l with
    | [] -> []
    | t::q -> (t,i)::(aux q (i+1))
    in aux l 0
;;

numerote [1;2;3;4;5;6];;

let rec dedoublonne l =
  match l with
  | [] -> []
  | [e] -> [e]
  | (_,a)::(n,b)::q when a=b -> dedoublonne((n,b)::q)
  | t::q -> t::dedoublonne q
;;

dedoublonne (numerote [1;2;3;4;5;6;4;3;5;6]);;

(* Pour le 14/09 FINIR Ex7 et Faire le début de Ex 8 (1,2,3) *)

(* -- Exercice 8 *)

(* -- 1 *)
(* On a une complexité en on note k le nombre de multiplication 
alors on a une complexité en O(n*k) *)


(* -- 2 *)

(* On calcul αγ, βδ et (α-β)(γ-δ). Donc il nous faut 3 appels récursifs *)
(* ∀ n ∈ ℕ, Cn ≤ 3*C((n+1)/2)+O(1) *)
(* α = log2(3) et β=0 < α *)
(* O(n^α)=O(n^{log2(2)}) et log2(3)< 2 donc c'est mieux que la premiere méthode *)

(* Programmons cette méthode : *)



let karatsuba nombre1 nombre2=
  let rec aux x y n=
    (* argument suplémentaire : n tq x et y s'ecrivent en <= n bits *)
    let k = n/2 in
    let bpk = (1 lsl k) in
    let beta = x mod bpk  (* (1 lsl n) correspond a b**k(On décale le 1) *)
    and alpha = x/ bpk
    and delta = y mod bpk  (* (1 lsl n) correspond a b**k(On décale le 1) *)
    and gamma = y/ bpk in

    let ag = aux alpha gamma ((n+1)/2)
    and bd = aux beta delta ((n+1)/2) 
    and autre = aux (alpha-beta) (gamma-delta) ((n+1)/2) in

    (* Cas d'arret : *)
    if n=1 then
      x*y
    else
    
     ag * (bpk*bpk) + 
    (
      ag +
      bd -
      autre )*bpk + 
      bd
  in aux nombre1 nombre2 64
;;

karatsuba 2 3;;