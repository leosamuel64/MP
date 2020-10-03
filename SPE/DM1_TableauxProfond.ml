type 'a grosTableau = Elem of 'a | Noeud of 'a grosTableau array;;

let id3 = Noeud( [|Elem 1;Elem 0;Elem 0|];
                 [|Elem 0;Elem 1;Elem 0|];
                 [|Elem 0;Elem 0;Elem 1|];
               );;

let rec lecture t l=
  match l,t with
  |[],Elem x -> x
  |a::q,Noeud tab ->
