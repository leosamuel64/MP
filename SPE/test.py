def A(i,j,n):
    if i==j:
        return "a"
    elif (1<=i) and (i<n) and (j==i+1):
        return -1
    elif (i==n) and (j in [1,2]):
        return -1
    else:
        return 0
    
def matriceVide(a,b):
    res=[]
    for ligne in range(1,a+1):
        res.append([])
        for colone in range(1,b+1):
            res[ligne-1].append("x")
    return res

# print(matriceVide(3,2))

def construitMatriceExo(a,b,n):
    res=matriceVide(a,b)
    for i in range(1,a+1):
        for j in range(1,b+1):
            res[i-1][j-1]=A(i,j,n)
    return res

def affiche(A):
    for i in range(len(A)):
        print(A[i])
    
affiche(construitMatriceExo(10,10,5))








    
    