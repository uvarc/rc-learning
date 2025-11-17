import numpy as np
import random
import sys

if len(sys.argv)>1:
   Nsteps=int(sys.argv[1])
else:
   print(0, 0., 0.)
   sys.exit()

pos=np.array([0,0])

moves=[np.array([0,1]),np.array([0,-1]),np.array([1,0]),np.array([-1,0])]

for n in range(Nsteps):
    move=random.choice(moves)
    pos+=move

distance=np.sqrt(pos[0]**2+pos[1]**2)
print("{:.2f},{:.2f},{:.2f}".format(Nsteps, np.sqrt(Nsteps), distance))
