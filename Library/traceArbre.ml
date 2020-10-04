

type tree = Noeud of tree * int * tree | Vide;;

let draw tree =
     let rec print indent tree =
       match tree with
        | Vide -> print_string ""
        |  Noeud(Vide,n,Vide) -> 
           Printf.printf "%s%d\n" indent n
        | Noeud (left, n, right) ->
           Printf.printf "%s----\n" indent;
           print (indent ^ "| ") left;
           Printf.printf "%s%d\n" indent n;
           print (indent ^ "| ") right;
           Printf.printf "%s----\n" indent
     in
     print "" tree
;;

let feuille x = Noeud(Vide, x, Vide);;
let exemple = Noeud (Noeud(Vide, 2, feuille 1),5,Noeud(Vide,3,Noeud(Noeud(Vide, 0, Vide),2,Vide)));;

draw exemple;;

let exemple2 = Noeud(Noeud(Noeud(Vide,5,Vide),7,(Noeud(Vide,4,Vide))),10,
              Noeud(Noeud(Vide,3,Vide),9,(Noeud(Vide,1,Vide))));;

draw exemple2;;
