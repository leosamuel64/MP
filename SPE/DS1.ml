
let tas_initial t=
  let k = Array.length t in
  let rec aux i =
    if i=k then
      Vide
    else 
      insertion (prochain t i) (aux (i+1))
    in aux 0
;;

let k_fusion t=
  let k = Array.length t in
  let tas0 = tas_initial t in
  
  let rec aux tas =
    (* La boucle *)
    if tas = Vide then
      []
    else 
      let (mini,i),suiteTas = extrait_min tas in
      let nvTas = 
      if t.(i) = [] then
        suiteTas
      else
        insertion (prochain t i) suiteTas
    in
    mini::(aux nvTas)
  in aux tas0
;;

(* 
Pour une réc sur un arbre :
On fait une rec sur la hauteur
∀ p ∈ ⟦-1,∞⟦, P(p):"∀ a arbre de hauteur p ..."

heridité : soit p ∈ ⟦-1,∞⟦ tq ∀ k ∈ ⟦-1,p⟧ , p(k)

*)




    
let aCycle g=
  let n = Array.length g in
  let deja_vu = Array.make n false in

  let rec visite_sommet s =
    if deja_vu.(s) then
      true
    else 
    (
      deja_vu.(s) <- true;
      visite_voisins s g.(s)
    )
    
  and visite_voisins s = function
    | [] -> false
    | t::autre_Voisin when not(deja_vu.(t)) -> visite_sommet t || visite_voisins s autre_Voisin
    | t::autre_Voisin when List.mem t g.(s) -> visite_voisins s autre_Voisin
    | t::autre_Voisin ->  visite_voisins s autre_Voisin || visite_sommet t s

  in
  visite_sommet 0
;;


