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
    n=random.randint(1,fin-deb-1)
    res.pop(n)
    # print(n+1845)
    # res.pop(2)

    return res,n+deb

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
        elif T[m]==T[0]+m:
            d=m+1
        elif T[m]== T[0]+m+1:
            f=m
    return T[m]-1
        



pasdiff=True
while pasdiff:
    t,n = tab(1845,1935)
    n2=anneeManquante(t)
    if n!=n2:
        pasdiff = False
        print(n)
        print(n2)



