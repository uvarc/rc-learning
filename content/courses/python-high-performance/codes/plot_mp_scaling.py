import matplotlib.pyplot as plt

nc=[1,4,8,16]
ts=[402,109.5,60.5,32.5]
tp=[402.,402./4,402./8,402./16]
plt.xlabel("Number of Processes")
plt.ylabel("Time to Solution")
plt.title("Scaling for Multiprocessing Pool")
plt.plot(nc,ts,label="Actual time")
plt.plot(nc,tp,c='r',label="Perfect scaling")
plt.legend()
plt.show()
