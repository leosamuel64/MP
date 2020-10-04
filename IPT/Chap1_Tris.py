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

print(triFusion([0,4,3,9,0,10,4,2]))




