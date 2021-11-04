import numpy as np
import matplotlib.pyplot as plt
fig = plt.figure()
ax = fig.add_subplot(111)
ax.plot(10*np.random.randn(100),10*np.random.randn(100), 'o')
ax.set_title('Scatter Plot')
plt.show()
