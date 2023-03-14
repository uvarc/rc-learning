import numpy as np
import matplotlib.pyplot as plt

lplist=np.arange(0,33,2)
lplist[0]=1
plist=np.delete(lplist,1)

times=np.zeros((plist.size,),dtype='float')

for i,p in enumerate(plist):
    times[i]=1./float(p)

ctimes=np.array([0.,.01,.015,.018,.02,.022,.024,.026,.03,.034,.038,.043,.048,.05,.055,.06])

fig,ax=plt.subplots()
plt.ylim(0.,1.1)
ax.set_title("Compute and Communication Time")
ax.set_xlabel("Process Count")
ax.set_ylabel("Time")

ax.bar(plist,times,label="Perfect Scaling")
ax.bar(plist,ctimes,label="Communication Overhead",bottom=times)
ax.legend()

plt.savefig("parallel_speedup.png",format="png")

plt.show()

