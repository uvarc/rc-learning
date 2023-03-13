import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import Circle, Rectangle

n=1e3
x = 1.-2.*np.random.random(int(n))
y = 1.-2.*np.random.random(int(n))
insideX,  insideY  = x[(x*x+y*y)<=1],y[(x*x+y*y)<=1]
outsideX, outsideY = x[(x*x+y*y)>1],y[(x*x+y*y)>1]

fig, ax = plt.subplots(1)
ax.scatter(insideX, insideY, c='b', alpha=0.8, edgecolor=None, s=20)
ax.scatter(outsideX, outsideY, c='r', alpha=0.8, edgecolor=None, s=20)
ax.set_aspect('equal')
circle = Circle((0, 0), 1, facecolor='none',
                edgecolor='black', linewidth=3, alpha=0.5)
ax.add_patch(circle)
rect = Rectangle((-1.0, -1.0), 2, 2, linewidth=3, edgecolor='black', facecolor='none')
ax.add_patch(rect)
plt.axis('off')
plt.show()
