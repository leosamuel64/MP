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

B=copie(A)
B[0][0]=10
affiche(A)
affiche(B)
