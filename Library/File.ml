type 'a file = {entree :'a list; sortie : 'a list};;
let fileVide = {entree=[]; sortie=[]};;

let enfile x f =
  {entree = (x::f.entree) ; sortie = f.sortie}
  ;;

let rec defile f =
  match f.sortie with
  | [] -> if f.entree <> [] then defile {entree = [] ; sortie = List.rev f.entree}
            else failwith "Erreur : File Vide"
  | t::q -> (t,{entree = f.entree ; sortie = q})
  ;;

let testfile = {entree = [] ; sortie = []};; 
defile testfile;;

let enfile_liste l f =
  List.fold_left (fun file elem -> enfile elem file) f l;;

let egales f1 f2=
  ((f1.sortie)@(List.rev f1.entree)) = ((f2.sortie)@(List.rev f2.entree))
;;