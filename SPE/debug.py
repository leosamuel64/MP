import math

def somme(n):
    res=0
    for n in range(1,n):
        res+=(-1)**n*(math.log(n+1)-math.log(n))
    return res

print(somme(100000))