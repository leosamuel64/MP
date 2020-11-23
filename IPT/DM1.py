# 2.1 - Fusion

def fusionEntre(t, deb, m, fin):
    res=[]
    i, j = deb, m

    while i < m and j < fin :
        if t[i] < t[j]:
            res.append(t[i])
            i+=1
        else:
            res.append(t[j])
            j+=1
    res.extend(t[i:m])

    for k in range(len(res)):           # On peut aussi faire t[deb:j] = res
        t[deb+k]=res[k]
    

t = [1,2,3,2,3,4,5,6]
fusionEntre(t,0,3,5)
# (t = [1, 2, 2, 3, 3, 4, 5, 6])


def cherche_a(t, deb, fin, x):
    d, f = deb, fin
    while d < f:
        m=(d+f)//2
        if x<t[m]:
            f=m
        else: 
            d=m+1
    return d

def cherche_b(t, deb, fin, x):
    if deb==fin:
        return deb
    else:
        m=(deb+fin+1)//2
        if x<=t[m]:
            return cherche_a(t, deb, m-1, x)
        else:
            return cherche_a(t, m, fin, x)

def dicho_gauche(t, deb, m):
    def aux(d, f):
        if d==f:
            return d
        else:
            milieu=(d+f)//2
            if t[milieu]<=t[m]:
                return aux(milieu, f)
            else:
                return aux(d, milieu)


def fusionOpti(t, deb, m, fin):
    a= cherche_a(t, deb, m, t[m])
    b= cherche_b(t, m-1, fin-1, t[m-1])

    fusionEntre(t, a
                 , m
                 , b+1 
                 )

t = [1,2,3,2,3,4,5,6]
fusionOpti(t,0,3,5)
# t = [1, 2, 2, 3, 3, 4, 5, 6]

""" Exemple avec [1,2,3,2,8,9,4,5,6]

aFusionner= [(0,3)]
aFusionner= [(0,3),(3,6)] -> [(0,6)]         
aFusionner= [(0,6),(6,9)] -> [(0,9)]
"""

"""
Par récurrence, on montre que f_i-d_i ≥ 2^i(f_0,d_0)
"""

"""
Le nombre d'élément qui ont été lu correspond a la somme des longueurs des intervalles présent dans la pile
On a \sum{i=0, k-1} (f_i-d_i)
avec la question 4.a. On a 
\sum{i=0, k-1} 2^i(f_0,d_0)
et f_0-d_0 ≥ 1
le nombre d'élément lu est alors supérieurs a 2^k-1 
"""

"""
Le nombre d'élément lu est inférieur à n.
On a alors 2^k-1 ≤ n
d'où k ≤ log2(n+1)
"""

def prochainePlage(t,deb):
    res=deb+1
    n=len(t)
    while res < n and t[res-1]<=t[res]:
        res+=1
    return res

prochainePlage([1,2,3,0,4,3,9],0)
# res=3

def ecrase(t, aFusionner):
    if len(aFusionner)==1:
        None #Cas d'arrêt
    else:
        (d0, f0) = aFusionner.pop()
        (d1, f1) = aFusionner.pop()
        if 2*(f0-d0) > f1-d1 :
            fusionEntre(t, d1, f1, f0)
            aFusionner.append( (d1, f0) )
            ecrase(t, aFusionner)
        else:
            # Sinon : cas d'arrêt, l'invariant de boucle est vérifié.Remettre la pile en état
            aFusionner.append( (d1,f1) )
            aFusionner.append( (d0,f0) )

def fusion(t, aFusionner):
    while len(aFusionner) >1:
        (d0, f0) = aFusionner.pop()
        (d1, f1) = aFusionner.pop()
        fusionOpti(t, d1, f1, f0)
        aFusionner.append( (d1, f0) )

def timsort(t):
    n=len(t)
    i=0
    aFusionner=[]
    while i <n:
        fin = prochainePlage(t,i)
        aFusionner.append( (i,fin) )
        ecrase(t, aFusionner)
        i=fin
    fusion(t, aFusionner)

t=[1,2,6,4,8,2,3,9,0,5]
timsort(t)
print(t)


def renverse(t, deb, fin):
    for i in range(0, (fin-deb)//2):
        t[deb+i], t[fin-1-i] = t[fin-1-i], t[deb+i]


def prochainePlageAmélioré(t,deb):
    res=deb+1
    n=len(t)
    if n==res : 
        return res
    else:
        croissant= t[deb]<=t[deb+1]
        # Relation d'ordre selon l'ordre des éléments :
        if croissant :
            ordre = lambda x,y : x<=y
        else:
            ordre=lambda x,y : x >=y
            
        while res < n and ordre (t[res-1], t[res]):
            res+=1
        if not croissant : 
            renverse(t,deb,res)
        return res

def timsortAmélioré(t):
    n=len(t)
    i=0
    aFusionner=[]
    while i <n:
        fin = prochainePlageAmélioré(t,i)
        aFusionner.append( (i,fin) )
        ecrase(t, aFusionner)
        i=fin

    fusion(t, aFusionner)

t=[1,2,6,4,8,2,3,9,0,5]
timsortAmélioré(t)
print(t)