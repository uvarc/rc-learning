---
title: PyTorch
date: "2022-06-09T00:00:00"
type: docs 
weight: 550
toc: true
menu: 
    python-machine-learning:
---



PyTorch is another interface for example of TensorFlow. It is a software library, developed by Facebook and maintained by Mega AI.

Because PyTorch uses Tensorflow as the underlying code, many of the required functions (e.g., activation, loss, optimizer) will be the same.


## Activation Function

* The _activation function_ will be determine if a node should "fire".
* Examples include nn.ReLU, nn.Sigmoid, and nn.Softmax.
* A complete list is available at
  * [https://pytorch.org/docs/stable/nn.html#non-linear-activations-weighted-sum-nonlinearity](https://pytorch.org/docs/stable/nn.html#non-linear-activations-weighted-sum-nonlinearity)
  * [https://pytorch.org/docs/stable/nn.html#non-linear-activations-other](https://pytorch.org/docs/stable/nn.html#non-linear-activations-other)


## Loss Function

* The _loss function_ will be optimized to improve the performance of the model.
* Examples include nn.BCELoss (Binary CrossEntropy) and nn.CrossEntropyLoss.
* A complete list is available at [https://pytorch.org/docs/stable/nn.html#loss-functions](https://pytorch.org/docs/stable/nn.html#loss-functions)


## Optimizer functions
* The _optimizer function_ is used for tweaking the weights.
* Examples include SGD, Adam, and RMSprop.
* A complete list is available at [https://pytorch.org/docs/stable/optim.html?highlight=optimizer#torch.optim.Optimizer](https://pytorch.org/docs/stable/optim.html?highlight=optimizer#torch.optim.Optimizer)


