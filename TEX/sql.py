def executeRequete(requete):
    print(requete)

def NombreDePeaux(année):
    t=executeRequete("""
    SELECT *
    FROM(
        SELECT annee,SUM(QuantitéLièvre) AS PeauxLièvre,SUM(QuantitéLynx) AS PeauxLynx
        FROM(
		SELECT annee, SUM(nbPeaux) AS QuantitéLièvre,0 AS QuantitéLynx
		from Achats
		WHERE espece = "lievre"
		GROUP BY annee
	  
	  UNION
	  
	  SELECT annee, 0 AS QuantitéLièvre,SUM(nbPeaux) AS QuantitéLynx
		from Achats
		WHERE espece = "lynx"
		GROUP BY annee
    ) group by annee
    WHERE annee="""+str(année)
    )
    lièvre,lynx = t[1],t[2]

    return(lièvre, lynx)
import random

def tab(deb,fin):
    res=[]
    for i in range(deb,fin+1):
        res.append(i)
    res.pop(random.randint(0,fin-deb))
    # res.pop(2)

    return res

t = tab(1845,1935)
# print(t)

tabl = [1,2,3,4,5,6,7,8,10]


def anneeManquante(T):
    d=0
    f=len(T)
    trouvé=False
    while not trouvé:
        m=(d+f)//2
        if d==f:
            trouvé=True
        elif T[m]==1845+m:
            d=m+1
        elif T[m]>1845+m:
            f=m
    return T[m]-1
        

print(anneeManquante(t))
