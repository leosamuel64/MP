# 1 - Runge_Kutta d'ordre 2

def Runge_Kutta(h,t0,y0,tf,F):
    tt,ty=[t0],[y0]
    t=t0
    y=y0
    while t<tf:
        y= y + h*F(y+h/2*F(y))
        t+=h
        tt.append(t)
        ty.append(y)
    return tt,ty

# Complexité : Le cout est de 2N appels alors que euler coute N
# Mais la précision de RK est alors en O(1/N^2) alors que euler est en O(1/N)

#  2 - Les Equations Proies-prédateurs

import matplotlib.pyplot as plt

def pp_euler_semi(a,b,c,d,x0,y0,h,t0,tf):
    y=y0
    x=x0
    t=t0
    Y=[y0]
    X=[x0]
    T=[t0]
    while t<tf:
        x = x + h*(a*x-b*x*y)
        y = y + h*(-c*y+d*x*y)
        t += h
        Y.append(y)
        X.append(x)
        T.append(t)
    return X,Y,T

def pp_euler(a,b,c,d,x0,y0,h,t0,tf):
    y=y0
    x=x0
    t=t0
    Y=[y0]
    X=[x0]
    T=[t0]
    while t<tf:
        x = x + h*(a*X[-1]-b*X[-1]*Y[-1])
        y = y + h*(-c*Y[-1]+d*X[-1]*Y[-1])
        t += h
        Y.append(y)
        X.append(x)
        T.append(t)
    return X,Y,T

X,Y,T = pp_euler_semi(1,1,1,1,1/2,1/2,0.5,0,50)

def plot(X,Y,T):
    plt.plot(X,Y,label='predateurs')
    plt.plot(Y,X,label='proies')
    plt.legend()
    plt.show()

# plot(X,Y,T)

def pp_RK(a,b,c,d,x0,y0,h,t0,tf):
    y=y0
    x=x0
    t=t0
    Y=[y0]
    X=[x0]
    T=[t0]
    while t<tf:
        ym = y+(h/2)*(-c*y+d*x*y)
        xm = x+(h/2)*(a*x-b*x*y)
        x += h*(a*xm-b*xm*ym)
        y += h*(-c*ym+d*xm*ym)
        t += h
        Y.append(y)
        X.append(x)
        T.append(t)
    return X,Y,T

# X,Y,T=pp_RK(1,1,1,1,1/2,1/2,0.1,0,50)

# plot(X,Y,T)



def valeur_moyenne(a,b,c,d,x0,y0,h):
    y=y0
    x=x0
    nbPassages=0
    sommex=0
    sommey=0
    nbpoint=0
    while nbPassages<2 :
        ym = y+(h/2)*(-c*y+d*x*y)
        xm = x+(h/2)*(a*x-b*x*y)
        nvx = x + h*(a*xm-b*xm*ym)
        nvy = y + h*(-c*ym+d*xm*ym)

        sommex+=nvx
        sommey+=nvy
        nbpoint+=1

        if (x-x0)*(nvx-x0)<0:
            nbPassages+=1
        x=nvx
        y=nvy
    
    return sommex/nbpoint, sommey/nbpoint

print(valeur_moyenne(1,1,1,1,1/2,1/2,0.01))

def valeur_moyenne_peche(a,b,c,d,e,x0,y0,h):
    y=y0
    x=x0
    nbPassages=0
    sommex=0
    sommey=0
    nbpoint=0
    while nbPassages<2 :
        ym = y+(h/2)*(-c*y+d*x*y)
        xm = x+(h/2)*(a*x-b*x*y)
        nvx = x + h*(a*xm-b*xm*ym)
        nvy = y + h*(-c*ym+d*xm*ym)

        sommex+=nvx
        sommey+=nvy
        nbpoint+=1

        if (x-x0)*(nvx-x0)<0:
            nbPassages+=1
        x=nvx
        y=nvy
    
    return sommex/nbpoint, sommey/nbpoint

