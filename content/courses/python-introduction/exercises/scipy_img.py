import numpy as np
import matplotlib.pyplot as plt
from scipy import misc

plt.figure()
image=misc.face()
plt.imshow(image)

img_array=np.array(image,dtype='float')
print(img_array.shape)

retint=np.array(img_array*[0.95,0.9,1.0],dtype='int')

plt.figure()
plt.imshow(retint)
plt.show()
