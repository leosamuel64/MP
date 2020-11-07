
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




    

