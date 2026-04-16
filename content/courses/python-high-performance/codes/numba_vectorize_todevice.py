import math
from numba import vectorize, cuda
import numpy as np

@vectorize(['float32(float32, float32, float32)',
            'float64(float64, float64, float64)'],
           target='cuda')
def cu_discriminant(a, b, c):
    return math.sqrt(b ** 2 - 4 * a * c)

N = 10000
dtype = np.float32

# prepare the input
A = np.array(np.random.sample(N), dtype=dtype)
B = np.array(np.random.sample(N) + 10, dtype=dtype)
C = np.array(np.random.sample(N), dtype=dtype)

a_device = cuda.to_device(A)
b_device = cuda.to_device(B)
c_device = cuda.to_device(C)

#sets up but doesn't initialize
d_device = cuda.device_array(shape=A.shape, dtype=np.float32)  

#"hidden" keyword argument instead of regular return
cu_discriminant(a_device,b_device,c_device,out=d_device)

#copy result back and print
D = d_device.copy_to_host()
print(D)


