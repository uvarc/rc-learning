import numpy as np
import matplotlib.pyplot as plt

#1 core
serial_time=3*60.+25.
serial_core_hour_time=3.*60+25
serial_core_eff=99.98

#5 core
five_core_time=69.1
five_core_hour_time=5.*60+47
five_core_eff=99.67

#10 core
ten_core_time=48.5
ten_core_hour_time=8*60+5
ten_core_eff=95.40

#25 core
tf_core_time=27.3
tf_core_hour_time=11*60+23
tf_core_eff=98.8

walltime=np.array([serial_time,five_core_time,ten_core_time,tf_core_time])
ncores=np.array([1,5,10,25])

parallel_scaling=walltime/serial_time
perfect=np.array([1.,1./5,1./10,1./25.])

speedup=serial_time/walltime
parallel_efficiency=serial_time/(ncores*walltime)
print(speedup)
print(parallel_efficiency)

plt.plot(ncores,parallel_scaling)
plt.plot(ncores,perfect)

plt.figure()
plt.plot(ncores,speedup)
plt.plot(ncores,1./perfect)

plt.show()
