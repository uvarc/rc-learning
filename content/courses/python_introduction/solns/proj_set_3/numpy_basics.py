import numpy as np

x = np.arange(0.0,50.0,0.01)
y = np.sin(x) + np.cos(1.4*x) + 0.1*x
mean_y = y.mean()
min_index = np.argmin(y) #np.where(y==y.min())
max_index = np.argmax(y) #np.where(y==y.max())
print (f"mean y:{mean_y}")
print (f"max y:{y[min_index]} at x={x[min_index]}")
print (f"max y:{y[max_index]} at x={x[max_index]}")

