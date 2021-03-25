import numpy as np
from math import *
import matplotlib.pyplot as plt

deb=145
β=(23.44*pi)/180
ω=2*pi/(364.25/365.25)
θ=(43.2951*pi)/180
Ω=2*pi/365.25

P1= np.array([  [cos(β) ,0 ,sin(β)],
                [0,      1,     0],
                [-sin(β) ,0 ,cos(β)]])

def P2(t):
    return np.array([   [cos(ω*t),-sin(ω*t),0],
                        [sin(ω*t),cos(ω*t) ,0],
                        [0,       0        ,1]])

P3= np.array([  [cos(θ),0,-sin(θ)],
                [0,     1, 0     ],
                [sin(θ),0, cos(θ)]])

def X(t):
    return np.array([   [-cos(ω*t)],
                        [-sin(ω*t)],
                        [0]])

def z(t):
    P1P2=np.dot(P1,P2(t))
    P1P2P3 = np.dot(P1P2,P3)
    P1P2P3T= np.transpose(P1P2P3)
    Y=np.dot(P1P2P3T,X(t))
    return Y[0,0]

def trace(deb,fin,pas):
    Z,T=[],[]
    while deb<fin:
        Z.append(z(deb))
        T.append(deb)
        deb+=pas
    plt.plot(T,Z)
    plt.show()

trace(145,146,0.01)