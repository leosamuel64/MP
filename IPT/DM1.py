

def fusionEntre(t, deb, m, fin):
    """ Fusionne en place t[deb:m] et t[m:fin], qui doivent être triés au préalable."""
    temp=[]# Contiendra la fusion de t[deb:m] et t[m:fin]
    i, j = deb, m

    while i < m and j < fin :
        # Imvariant de boucle : temp contient la fusion de t[deb:i] et t[m:j]
        if t[i] < t[j]:
            temp.append(t[i])
            i+=1
        else:
            temp.append(t[j])
            j+=1

    temp.extend(t[i:m])
    # Les éléments de t[j:fin] sont déjà dans leur place finale : inutile de les mettre dans temp pour les remettre ensuite dans t

    # On recopie temp dans t:
    for k in range(len(temp)):
        t[deb+k]=temp[k]