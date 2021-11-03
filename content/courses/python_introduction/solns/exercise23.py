import functools

V=[-1,0,1,2,3,4,5]

L=list(map(lambda x:x**2, V))
R=functools.reduce(lambda x,y:x+y, V)
F=list(filter(lambda x:x>0, V))

print(L)
print(R)
print(F)

L2=[vel**2 for vel in V]
R2=sum(V)
F2=[vel for vel in V if vel>0]

print(L2)
print(R2)
print(F2)
