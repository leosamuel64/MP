
type 'a tabRedim = {mutable longueur : int; mutable donnees : 'a array};;


let rec creeTabRedim n x =
  (* Créer un tableau de longueur n remplie de x *)

  {longueur = n;donnees = Array.make (2*n) x}
  ;;

let lire t i=
  (* Lit la valeur de la case i d'un tableau t *)

  if i < t.longueur then
    t.donnees.(i)
    else failwith "Error : Out of Range"
  ;;

let ecrire t i x = 
  (* Ecrit dans la case i d'un tableau t la valeur x *)

  if i < t.longueur then
  t.donnees.(i)<-x
  else failwith "Error : Out of Range"
  ;;

let append t x=
  (* Ajoute un élément x a la fin d'un tableau t *)

  if t.longueur < (Array.length t.donnees) then begin
    t.longueur <- t.longueur+1;
    ecrire t (t.longueur-1) x;
  end
  else begin
    let nouvt = Array.make ((2*Array.length t.donnees) +1) x in
    for i= 0 to (t.longueur-1) do
      nouvt.(i)<-lire t i;
    done;
    t.donnees<- nouvt;
    t.longueur <- t.longueur+1;
  end
  ;;

let pop t=
  (* Enlève le dernier element d'un tableau t et le renvoie *)

  if t.longueur > 0 then begin
  t.longueur <- t.longueur-1;
  t.donnees.(t.longueur)
  end
  else
    failwith "Erreur : Tableau Vide"
  ;;

let egale t1 t2=
  (* Indique si deux tableaux de type tabRedim sont égaux *)

  if t1.longueur = t2.longueur then
    let res = ref true in
    for i=0 to t1.longueur-1 do
      if t1.donnees.(i)!=t2.donnees.(i) then
        res := false
    done;
    !res
  else false
  ;;

let nouveauTab () = 
  (* Creer le tableau vide *)

  {longueur = 0;donnees=[||]}
;;
