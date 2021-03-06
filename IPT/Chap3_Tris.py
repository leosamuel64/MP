# Exercice 3 : Copie Profonde

def nouvelleMatrice(a,b,n):
  res=[]
  for ligne in range(1,a+1):
    res.append([])
    for colone in range(1,b+1):
      res[ligne-1].append(n)
  return res

def copie(A):
  res = nouvelleMatrice(len(A),len(A[0]),-1)
  for i in range(len(A)):
    for j in range(len(A[0])):
      res[i][j]=A[i][j]
  return res

def affiche(A):
  for i in range(len(A)):
    print(A[i])
  print(" ")

A= [[1,2,3],
    [4,5,6],
    [7,8,9]]



def deepCopy(A):
  if type(A)==list:
    return [deepCopy(x) for x in A]
  else:
    return A

# B=deepCopy(A)
# B[0][0]=10
# affiche(A)
# affiche(B)
# affiche(A)

# Exercice 4 : Distance minimale entre deux éléments d'une liste

# Pour verifier naivement il faut parcourir une fois la liste pour chaque élement.
# On note |l| la longueur de la liste. On a une complexité en O(|l|²)



def distanceMin(l):
  l1=sorted(l)
  min=abs(l1[0]-l1[len(l1)-1])

  for i in range(1,len(l1)):
    if l1[i]-l1[i-1]<min:
      min =abs(l1[i]-l1[i-1])
  return min


# print(distanceMin([0,4,2,8,6,10]))
# On a une compllexité en O(|l|*log|l| + |l|) = O(|l|*log|l|)


# tri fusion



# Exercice 5 : Elément dans un maximum d'intervalles

def intervalle(i):
  res = []
  for k in range (i[0],i[1]+1):
    res.append(k)
  return res

# O(n) 

# print(intervalle((1,10)))

  
def grandeListe(l):
  listeDesEntiers=[]
  for i in l:
    listeDesEntiers += intervalle(i)
  return listeDesEntiers

# O(n*len(l)) 

def incr_dico(dico,cle):
  try:
    dico[cle]+=1
  except:
    dico[cle]=1

def maximum(l):
  gl = grandeListe(l)
  dico={}
  res = ""
  for x in gl:
    incr_dico(dico,str(x))
  maxi = 0
  
  for cle in dico:
    if dico[cle]>maxi:
      maxi = dico[cle]
      res=cle
  return int(res)
  
# print(maximum([(1,10),(3,15),(2,17),(8,15)]))

# O(max(taille intervalle)*(nombre d'intervalle))

# Exercice 6 : tri fusion

def partition(t):
  return t[0:len(t)//2],t[len(t)//2:len(t)]

# print(partitionne([1,4,3,2,4]))

def fusion(t1,t2):
  if len(t1)==0:
    return t2
  elif len(t2)==0:
    return t1
  elif t1[0]<t2[0]:
    return [t1[0]]+fusion(t1[1:],t2)
  else:
    return [t2[0]]+fusion(t1,t2[1:])

def triFusion(t):
  if len(t)<=1:
    return t
  t1,t2=partition(t)
  return fusion(triFusion(t1),triFusion(t2))

# print(triFusion([0,4,3,9,0,10,4,2]))


# Cours

# Introduction

# On a déjà étudié des tris dans les chapitres précedants : fusion et par tas

# 1-Pourquoi trier ?
# Application : pouvoir utiliser une recherche dichotomique

def dicho(t,x):
  deb,fin,trouvé = 0,len(t),False
  
  while deb < fin and (not trouvé):
    m=(deb+fin)//2
    if x<t[m]:
      fin=m
    elif x>t[m]:
      deb=m+1
    else:
      trouvé=True
  return trouvé

# print(dicho([1,2,3,4,5],10))


def dichoRec(t,x):
  def aux(t,x,deb,fin):
    m=(deb+fin)//2
    if deb==fin:
      return False
    elif x<t[m]:
      return aux(t,x,deb,m)
    elif x>t[m]:
      return aux(t,x,m+1,fin)
    else:
      return True
  return aux(t,x,0,len(t))

# print(dichoRec([1,2,3,4,5,6],5))

# 2-Ccritère d'apreciation d'un tri:
 
# - La complexité : on compte le nombre de comparaisons (mais en gardant en tête les autres opérations)
# - La complexité mémoire : Evitons, si on le peut, de créer de nombreuse copies du tableau à trier

# Un algo sera dit "en place" si on arrive a rester toujours dans le même tableau (Tri par tas mutable)
# 
# En géneral, un tri en place sera une procédure, un tri pas en place une fonction.

# utilisation de fonction procedure
# Calcul de la mediane d'un tableau

# Avec une fonction de tri pas en place
def mediane(t):
  return sorted(t)[len(t)//2]

# Avec une fonction de tri en place
def mediane2(t):
  t.sort()
  return t[len(t)//2]

# Cette version tri le tableau
# Copie d'un tableau t2=t[:]

# 3 - Tri par insertion
# 3.1- Sur listes en caml
#   Fait

# 3.2- complexité

# insere x l -> O(n) dans le pire des cas   (Quand x > max(l))
#               O(1) dans le meilleur cas   (Quand x < min(l))

# triInsertion -> O(n(n+1)/2)=O(n²) dans le pire cas
#                 O(n) dans le meilleur cas


# 3.3 Avec des tableaux en python

# On peut pogrammer le tri par insertion en place

# Principe :
# [ , , , , , , , , , ]
#          ^
#  {Trié}  i  
# soit t un tableau et n=len(t)
# ∀ i in ⟦0,n⟦ :
# inserer t[i] "à sa place" dans t[0:i]

def cherche(t,v,i):
  for i in range(i):
    if t[i]>v:
      return i
    
def insere(t,i):
  """
  Précondition : t[0:i] est trié
  Effet de bord : déplace t[i] pour que t[0:i+1] devienne trié 
  """
  j=i       # Contiendra la place finale de x
  x=t[i]
  while j!=0 and t[j-1]>x:
    t[j]=t[j-1]
    j-=1
  t[j]=x

def triInsertion(t):
  """
  Procédure qui tri t
  """
  for i in range(len(t)):
    #ici, t[0:i] est trié
    insere(t,i)
    #ici, t[0:i+1] est trié

# t=[1,2,4,3,5,8,7,6]
# triInsertion(t)
# print(t)
     

# Pour le 12/10 : Faire l'exercice 13

# 4- Tri fusion sur tableaux python


# print(partitionne([1,4,3,2,4]))

# def fusion(t1,t2):
#   res = []
#   for i in range(max(len(t1),len(t2))):
#     a,b=min(t1[i],t2[i]),max(t1[i],t2[i])
#     res.append(a)
#     res.append(b)
#   return res
  #  Ne marche que si les tableaux ont la même dimension

def fusion(t1,t2):
  res=[]
  i1=0
  i2=0

  while i1<len(t1) and i2<len(t2):
    if t1[i1]<t2[i2]:
      res.append(t1[i1])
      i1+=1
    elif t2[i2]<t1[i1]:
      res.append(t2[i2])
      i2+=1

  return res+t1[i1:]+t2[i2:]


# print(fusion([1,3,5],[2,4,6]))

def triFusion(t):
  n=len(t)
  if n<2:
    return t

  t1, t2= t[0:n//2], t[n//2:n]
  
  t1Trié= triFusion(t1)
  t2Trié= triFusion(t2)

  return fusion(t1Trié,t2Trié)

# print(triFusion([1,4,2,5,7]))

# Analyse:
# complexité : O(nlogn)
# Memoire : pas en place -> occupe de la mémoire

# Exemple : triFusion([1,4,0,9,8,12])
            # [1,4,0]                 [9,8,0]
          # [1]     [4,0]          [9]      [8,0]
          # [1]    [4]  [0]        [9]     [8]  [0]

# Exercice 7 : Nombre d'inversion

# Rappel : Soit n ∈ ℕ et σ ∈ 𝔖n
# Une inversion de de σ est un couple (i,j) ∈ ⟦0,n⟦²
# tq :i<j et σi<j

# Signature de σ : ε(σ)=(-1)**(nb d'inversion)
# Informatiquement : σ sera représenté par le tableau
# [σ(0),σ(1),σ(2),σ(3),...,σ(n-1)]

        #  (0,1,2,3,4)
# Exemple: [4,1,3,2,0] (n=5)
#       Combien d'inversion
# 0 est en inversion avec 1,2,3,4
# 1 est en inversion avec 4
# 2 est en inversion avec 3,4
# 3 est en inversion avec 4
# Il y a alors 8 inversions et ε(σ)=(-1)**8=1

def nb_inversion(σ):
  """
  Entrée : σ un tableau qui représente une permutation
  Sortie : Le nombre d'inversion de σ
  """
  res=0
  n=len(σ)
  for i in range(0,n):
  # Cherchons les j tq (i,j) est une inversion
    for j in range(i+1,n):
      if σ[i]>σ[j]:
        res+=1
  return res

# print(nb_inversion([4,1,3,2,0]))

# Complexité en O(n²)

# Methode diviser pour regner : pour le nombre d'inversions

# Soit t un tableau et n =len(t)
# Si n <= 1: 0 inversion
# Si n >= 1:    - diviser : soient t1[:n//2] et t2[n//2:]
              # - Appel rec : On recupere le nombre d'inversion ds t1 et t2
              # - regner : En deduire le nombre d'inversion de t

              # On partitionne l'une des inversions de t en 3
                  # -Les inversions de t1
                  # -                de t2
                  # - les couples i,j tq i ≤  ⌞n/2⌟
                                          # j ≥  ⌞n/2⌟
                                          # t[i]>t[j]
              # Pendant la fusion
              # len(t1)-i1 inversions
              # 
# Le programmer pour le  12/10 + exo 13 de tri

def partition(t):
  return t[0:len(t)//2],t[len(t)//2:len(t)]

def permute(t,a,b):
  """
  Effectue sur t une permutation circulaire vers la droite des composantes
  t[a:b] cad de t[a] à t[b-1]
  """
  x=t[b-1]
  for k in range(0,b-a-1):
    t[b-1-k]=t[b-2-k]
  t[a]=x


def nbInvDansFusion(t1,t2):
  i1,i2=0,0
  res=0
  while i1<len(t1) and i2<len(t2):
    if t1[i1]<t2[i2]:
      i1+=1
    else:
      i2+=1
      res+=len(t1)-i1
  return res

def nbInversion2(t):
  if len(t)<=1: 
    return 0
  else:
    t1,t2=partition(t)
    res1=nbInversion2(t1)
    res2=nbInversion2(t2)
    res3=nbInvDansFusion(t1,t2)
    return res1 + res2 + res3
  
# print(nbInversion2([4,1,3,2,0]))


# Exercice 13 : insertion dichotomique

def InsereEtDecale(t,i,j):
    """ Met t[j] en t[i] et decale les éléments vers la droite """
    tmp=t[j]
    for k in range (j,i,-1):
        t[k]=t[k-1]
    t[i]=tmp

# Soit n la longueur de t. Cn = O(n)

def cherchePlaceEntre(t,deb,fin,x):
  m=(deb+fin)//2
  if deb==fin:
    return deb
  elif t[m]==x:
    return m+1
  elif x<t[m]:
    return cherchePlaceEntre(t,deb,m,x)
  else:
    return cherchePlaceEntre(t,m+1,fin,x)

# Soit n la longueur de t. α=log2(1)=0 et β=0 d'où α>β d'où Cn=O(log(n)).
# Il n'y a plus de comparaison a part l'appel à la fonction auxiliaire.

def cherchePlaceDicho(t,i):
    return cherchePlaceEntre(t,0,i,t[i])

# Soit n la longueur de t. Cn = O(log(n))

def triInsertionDicho(t):
  for i in range(1,len(t)):
    k=cherchePlaceDicho(t,i)
    InsereEtDecale(t,k,i)

# Soit n la longueur de t. Cn=O(n*(log(n)))=O(nlog(n))


# t=[9,3,2,7,8,4,3]
# triInsertionDicho(t)
# print(t)

# On n'a compté que le nombre de comparaison et on a plas compté l'insertion.

# La complexité est quadratique donc le tri par insertion dichotomique n'est pas intérésant. 
# (Cependant, il est en place...)

# Au pire, l'insertion du ième élément coute O(i) operation sur sous tableau

# 5- Tri par segmentation /ou/ tri rapide (Quick Sort) /ou/ tri de Hoare :
# 5.1- Principe
# 
# semblable au tri fusion, mais pour la partie "diviser", on choisit un élément, qu'on appelle le "pivot"
# et on separe le tableau ou la liste à trier en les élements ≤ pivot
# 
# 5.2- Sur listes Chainés (En Ocaml) 

# 5.3- Complexité

# - Supposons que les segmentations sont équilibrées au mieux à chaque appels
# |petit|-|grand| ≤ 1
# La complexité est Cn = O(n)     +     C(⌞(n-1)/2⌟)+C(⌞(n)/2⌟) + O(n)     
#                    (segmentation)
# Cn=O(nlog(n)) (comme le tri fusion)
# On admet que ce cas est le meilleur

# - Sinon, la découpe est déséquilibré à chaque segmentation (l'une des deux liste renvoyé est vide)
# C'est le cas où la liste est déjà triée (dans l'ordre croissant ou décroissant)
# A chaque appel réc on aura t<min q ou t> max q donc petits=[] ou grands=[]
# Dans ce cas, la relation de récurrence devient :
# C(n) = O(n) + C(0) + C(n-1) + O(n)
#      = O(n²)

# Le tri par segmentation est au mieux comme le tri fusion et au pire il est mauvais si le tableau est déjà
# trié

# Mais la complexité moyenne est de O(nlogn) a condition que tous les ordres sur la liste en entrée sont 
# équiprobables

# 5.4- Sur tableau
# L'interêt principal du tri par segmentation est qu'il peut etre implementer
# efficacement en place.

# Pour gerer les appels récursifs, sans copie de tableaux, on utilise un fonction auxiliaire triEntre
# qui pour un tableau t et 2 indices deb et fin et qui trie en place t[deb:fin]

# L'etape clé est la segmentation

# [  \\  ; deb ; ...  ...  ; fin ; \\ ]
#         pivot

# On veut arriver à la situation suivante
# [ // ;         ;pivot;         ; // ]
#       ≤ pivot         ≥ pivot

# Invariant de boucle : 
# - Le pivot es en [deb]
# - les éléments de t[deb+1:i] sont ≤ pivot
# - les éléments de t[i:j] sont > pivot

# Pour le 15/10 : Faire le fonction triEntre et finir segmentation



def transpose(t,init,dest):
  t[init],t[dest]=t[dest],t[init]

def segmenteEntre(t,iPivot,deb,fin):
  """
  Segmente t[deb:fin] en utilisant t[iPivot] comme pivot
  """
  transpose(t, deb, iPivot) # On met le pivot en case deb

  pivot=t[deb]
  i=deb+1
  j=fin

  while i!=j:
    # Invariant de boucle
    # t[deb] contient le pivot
    # les éléments de t[deb+1:i] sont ≤ pivot
    # les éléments de t[i:j] sont > pivot
    if t[i] <= pivot:
      i+=1
    else:
      transpose(t,i,j-1)
      j-=1
  transpose(t,deb,i-1)
  # Mettre le pivot a sa place
  return i-1


def triEntre(t,deb,fin):
  if fin-deb>1:
    iPivot=segmenteEntre(t,deb,deb,fin)
    triEntre(t,deb,iPivot)
    triEntre(t,iPivot+1,fin)

def triSegmentation(t):
  triEntre(t,0,len(t))

# t=[1,3,2,4,3,6,5,8,9,6,7]
# triSegmentation([3,2,8,9,0,4,3,2,1,6,7,8,9,4,3,2,9])
# print(t)

# Exercice 15 : Calcul de médiane

def iemeElement(T,i,deb,fin):
  """
  (i ∈ ⟦0, fin-deb⟦)
  Renvoie le (i+1)-eme plus petit element
  """
  if deb-fin<1:
    iPivot = segmenteEntre(T,i,deb,fin)
    if i==iPivot:
      return T[i]
    elif i>iPivot:
      return iemeElement(T,i,deb,iPivot)
    else:
      return iemeElement(T,i,iPivot+1,fin)
      

# print(iemeElement([1,2,5,6],2,0,4))

def mediane(t):
  return iemeElement(t,len(t)//2,0,len(t))

# print(mediane([7,3,2,4,5,6,1]))

def minimum(t):
  return iemeElement(t,1,0,len(t))

# print(minimum([1,2,3,4,5,0]))


# Cxt : au pire, si t est trié,
# Alors à chaque appel on aura iPivot=deb.
# iPivot=fin-1       On aura une relation de recurrence du type C(n)=C(n-1)+O(n)
#  Soit O(n²)
# dans le meilleur cas si le premier pivot est la mediane/minimum/maximum/ce que l'on veut... 
# Cn=O(n) car premiere segmentation

# Si a chaque segmentation, le tableau est à peu pres coupé en 2, C(n)=C( ⌞n/2⌟)+O(n)
#                                                                     = O(n)
     
# Exercice 18 : Complexité moyenne du tri rapide

# Cn = complexité moyenne pour un tableau à n élément 
#    = E(nb de comparaison pour trié le tableau)

#1-
# C0= 0
# C1= 0

#2-
# Complexité de la segmentation d'une plage de n éléments n-1 comparaison = O(n)

#3-
# Soit n ∈ ⟦2,+∞⟦
# On segmente une plage de n éléments
# ∀ i ∈ ⟦0,n⟦,
# Soit A(i) : "le pivot arrive en position i"   
# A0,A1,...,An est un systeme complet d'evenement

# Soit X la variable aléatoire donnant le nombre de comparaison pour un tableau de taille n
# On a E(Xn)=Σ(i)(E(Xn|Ai)*P(Ai)
# En notant E(X|Ai)=Σ(v∈X(Ω))(v*P(X=v|Ai))

# Or , dans le cas Ai, on  Effectue les deux appels recursifs:
# [         ,i,            ]
#     (i)         (n-i-1)

# Sur des plages de tableau de longueur i et n-i-1 d'ou une complexité de
# Cn=C(i)+C(n-i-1)+O(n)

# Si on suppose toutes les permutations equiprobables, alors toutes les positions finales
# du pivot sont équiprobable donc P(Ai)=1/n

#  Exercice 16 : dérécursifier le tri rapide
def triEntre(t,deb,fin):
  if fin-deb>1:
    iPivot=segmenteEntre(t,deb,deb,fin)
    triEntre(t,deb,iPivot)
    triEntre(t,iPivot+1,fin)

def triRapideNonRec(t):
  deb=0
  fin=len(t)
  pile=[(deb,fin)]
  trié=False

  while pile!=[] and not(trié):
    a,b=pile.pop()
    
    if a!=b:
      ipivot=segmenteEntre(t,a,a,b)
      pile.append((a,ipivot))
      pile.append((ipivot+1,b))



t=[1,3,2,7,6,5]
triRapideNonRec(t)
# print(t)

import random  
  
def GrandeListe(n,trié=False):
  res=[]
  for i in range (n):
    if trié:
      res.append(i)
    else:
      res.append(random.randint(0,100))
  return res

import time

a=GrandeListe(100000)
print("done")


deb=time.time()
triRapideNonRec(a)
print(time.time()-deb)