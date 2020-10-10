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
      permute()
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
  
print(nbInversion2([4,1,3,2,0]))


# Exercice 13 : insertion dichotomique

def InsereEtDecale(t,i,j):
    """ Met t[j] en t[i] et decale les éléments vers la droite """
    tmp=t[j]
    for k in range (j,i,-1):
        t[k]=t[k-1]
    t[i]=tmp

# Soit n la longueur de t. Cn = O(n)

def cherchePlaceEntre(t,deb,fin,x):
  m= (deb+fin)//2
  if deb==fin:
    return deb
  elif t[m]==x:
    return m+1
  elif x<t[m]:
    return cherchePlaceEntre(t,deb,m,x)
  else:
    return cherchePlaceEntre(t,m+1,fin,x)

# Soit n la longueur de t. α=log2(2)=1 et β=0 d'où α>β d'où Cn=O(n)


def cherchePlaceDicho(t,i):
    return cherchePlaceEntre(t,0,i,t[i])

# Soit n la longueur de t. Cn = O(n)


def triInsertionDicho(t):
  for i in range(1,len(t)):
    k=cherchePlaceDicho(t,i)
    InsereEtDecale(t,k,i)

# Soit n la longueur de t. Cn=O(n*n*n)=O(n**3)


# t=[9,3,2,7,8,4,3]
# triInsertionDicho(t)
# print(t)

# La complexité est quadratique donc le tri par insertion dichotomique n'est pas intérésant. 
# (Cependant, il est en place...)
