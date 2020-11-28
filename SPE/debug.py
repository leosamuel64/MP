def ligne(l1,l2,k):
    # Fait k*l1-l2
    res = []
    for i in range(len(l1)):
        res.append(k*l1[i]-l2[i])
    return res


print(ligne(  [0,2,3,1,4],
        [0, 12, 15, 1, 48],
        6
))
