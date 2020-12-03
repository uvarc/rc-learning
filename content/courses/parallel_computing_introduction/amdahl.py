import numpy as np
import matplotlib.pyplot as plt

def amdahl(f,p):
    return 1./(f+(1-f)/p)

plist=list(range(1,1025))

fig,ax=plt.subplots()
ax.set_title("Speedup for Representative Parallel Fractions")
ax.set_xlabel("Process Count")
ax.set_ylabel("Speedup")

f=0.5
speedup=[]
for p in range(1,1025):
    speedup.append(amdahl(f,p))

ax.plot(plist,speedup,label="50% parallel")

f=0.25
speedup=[]
for p in range(1,1025):
    speedup.append(amdahl(f,p))

ax.plot(plist,speedup,label="75% parallel")

f=0.10
speedup=[]
for p in range(1,1025):
    speedup.append(amdahl(f,p))

ax.plot(plist,speedup,label="90% parallel")

f=0.05
speedup=[]
for p in range(1,1025):
    speedup.append(amdahl(f,p))

ax.plot(plist,speedup,label="95% parallel")

f=0.01
speedup=[]
for p in range(1,1025):
    speedup.append(amdahl(f,p))

ax.plot(plist,speedup,label="99% parallel")

ax.legend()
plt.savefig("strong_speedup.png",format="png")


plt.show()

