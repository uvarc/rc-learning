---
title: PyTorch
date: "2024-06-06T00:00:00"
type: docs 
weight: 2850
toc: true
menu: 
    deep-learning-distributed:
        parent: PyTorch
---

Pytorch is another widely-used deep-learning platform known for its flexibility and speed. It is a popular library used by academics and researchers.

* It is a software library, developed by Facebook and maintained by Mega AI.
* Torch is an open-source project for Deep Learning written in C and generally used via the Lua interface. Torch is no longer actively developed but libraries are used in Pytorch.
* Because PyTorch and Tensorflow use some common underlying codes, many of the required functions (e.g., activation, loss, optimizer) will be the same.

## Installation

* Conda
```bash
conda create --name torch-env pytorch torchvision pytorch-cuda=12.1 -c pytorch –c nvidia
```
* Container (NGC)
  * https://catalog.ngc.nvidia.com/orgs/nvidia/containers/pytorch
* Building from source
  * https://github.com/pytorch/pytorch#from-source


## Performance and Utilization:
  * Use DataLoader and try increasing the value of cpus-per-task in tandem with num_workers  to prepare the data and keep the GPU busy. This was shown to dramatically increase the GPU utilization.
    * Writing a custom dataloader: https://www.youtube.com/watch?v=PXOzkkB5eH0
    * External datal loading libraries: https://github.com/libffcv/ffcv, https://developer.nvidia.com/dali
  * Mixed precision training requires either the V100 or A100 GPU and is included in PyTorch as torch.cuda.amp. PyTorch will perform FP32 matrix multiplications using TF32 by default.
    * Automatic Mixed Precision: https://pytorch.org/docs/stable/amp.html
  * gpustat, Line_profiler, nvprof or nsys (if on GPU)
    * For example, `./nvprof python mnist_classify.py --epochs=3`
  * TensorBoard is a useful tool for tracking the training process of a PyTorch model. Available through conda and container version.

__Performance Tuning__
* [Pytorch Performance Tuning Guide](https://pytorch.org/tutorials/recipes/recipes/tuning_guide.html)
* [Performance Tuning Guide Slides](https://tigress-web.princeton.edu/~jdh4/PyTorchPerformanceTuningGuide_GTC2021.pdf)



### Torch Tensors
A __tensor__ is a multidimensional array (like a numpy ndarray) which can be used on GPUs

```python
import torch

x = torch.rand(5,3, dtype=torch.long, device='cuda') # if not specified, uses cpu
y = torch.zeros(5,3)
z = torch.add(x+y) # or z=x+y
w = z.numpy()  # convert to numpy array, same memory location
t = torch.from_numpy(w)	# numpy to torch tensor (on cpu)
```

### CUDA tensors

Tensors can be moved onto any device using the `.to` method.

```python
if torch.cuda.is_available():
    device = torch.device("cuda")
    y = torch.rand(3,5, device=device)
    x = torch.rand(3,5).to(device)
    z = x + y
    print(z)
    print(z.to("cpu", torch.double)) # ``.to`` can also change dtype together!
```

## Coding a Pytorch Model: General Steps

1\. Import the torch package <br>
2\. Read in the data <br>
3\. Preprocess the data <br>
  &nbsp;&nbsp;&nbsp;&nbsp;3a. Scale the data <br>
  &nbsp;&nbsp;&nbsp;&nbsp;3b. Split the data <br>
  &nbsp;&nbsp;&nbsp;&nbsp;3c. Convert data to tensors <br>
  &nbsp;&nbsp;&nbsp;&nbsp;3d. Load the tensors <br>
4\. Design the Network Model <br>
5\. Define the Learning Process <br>
6\. Train the model <br>
7\. Apply the model to the test data <br>
8\. Display the results

Make sure that you can run the PyTorch code:

  * PT_CNN_SingleGPU.ipynb


