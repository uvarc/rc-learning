import numpy as np

N=4
M=5

incr=0.01

A=incr*np.arange(1.,N*M+1).reshape(N,M)

B=0.25*A+.01

C=A+B

D=sum(C,1)

if B.shape[0]>1 and B.shape[1]>4:
    print(B[:N-1,:M-4])
else:
    print("B is too small.")
