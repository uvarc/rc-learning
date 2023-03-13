import numpy as np
import matplotlib.pyplot as plt

#Strong scaling: same for all comparisons
#serial_time=np.zeros(4)
#serial_time[:]=73.0535
#p_time=np.array([73.0535,37.625,19.5843,10.005])

#Weak scaling: set serial time for each core count
serial_time=np.array([6.481,14.488,28.636,58.334])
p_time=np.array([6.481,7.6239,7.713,7.986])

ncores=np.array([1,2,4,8])
speedup=serial_time/p_time
perfect=ncores

parallel_efficiency=speedup/ncores

fig,(ax1,ax2)=plt.subplots(1,2)
fig.tight_layout(pad=2.5)
ax1.plot(ncores,speedup,label="Actual")
ax1.set_xlabel("Number of Cores")
ax1.set_ylabel("Speedup")
ax1.plot(ncores,perfect,label="Perfect")
ax1.legend()

ax2.set_xlabel("Number of Cores")
ax2.set_ylabel("Efficiency")
ax2.plot(ncores,parallel_efficiency)

plt.show()

