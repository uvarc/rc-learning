import numpy as np
import cupy as cp
import time

x_cpu = np.array([1, 2, 3])
x_gpu = cp.array([1, 2, 3])
  
l2_cpu = np.linalg.norm(x_cpu)
l2_gpu = cp.linalg.norm(x_gpu)
  
print("Norm output using Numpy: ", l2_cpu)
print("Norm output Using Cupy: ", l2_gpu)
print()

print("Setting up arrays on host and device")
s = time.time()
x_cpu = np.ones((1000,1000,100))
e = time.time()
print("CPU:",e - s)
s = time.time()
x_gpu = cp.ones((1000,1000,100))
cp.cuda.Stream.null.synchronize()
e = time.time()
print("GPU:",e - s)

print()
N=10000000
print("FFT on arrays on host and device")
### Numpy and CPU
s = time.time()
a = np.random.random(N).astype(complex)
b = np.fft.fft(a) 
e = time.time()
print("CPU:",e - s)
s = time.time()
a = cp.random.random(N).astype(complex)
b = cp.fft.fft(a) 
cp.cuda.Stream.null.synchronize()
e = time.time()
print("GPU:",e - s)
