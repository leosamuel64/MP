def calcul(x,eps):
    reste = x
    res=[x**2]
    while reste > eps:
        reste=reste**2
        i = int(reste)
        res.append(i)
        reste-=i
    return res

def r(x):
    return x**(1/2)

print(calcul(r(3)+r(7),0))