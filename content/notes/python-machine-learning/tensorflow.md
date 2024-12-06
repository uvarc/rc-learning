---
title: TensorFlow
date: "2022-06-09T00:00:00"
type: docs 
weight: 450
toc: true
menu: 
    python-machine-learning:
---


TensorFlow is an example of deep learning, a neural network that has many layers. It is a software library developed by the Google Brain Team.


## Deep Learning Neural Network

{{< figure src=/notes/python-machine-learning/img/deep_neural_network.jpg caption="Image borrowed from: http://www.kdnuggets.com/2017/05/deep-learning-big-deal.html" width=50% height=50% >}}



## Terminology: Tensors

A __Tensor__ is a multi-dimensional array.

Example:  A sequence of images can be represented as a 4-D array: [image_num, row, col, color_channel]

{{< figure src=/notes/python-machine-learning/img/tensors.png caption="" width=60% height=60% >}}

## Terminology:  Computational Graphs

* Computational graphs help to break down computations.
  * For example, the graph for $y=(x1+x2)*(x2 - 5)$:

{{< figure src=/notes/python-machine-learning/img/computational_graph.png caption="The beauty of computational graphs is that they show where computations can be done in parallel." width=40% height=40% >}}


## The Need for GPUs

With deep learning models, you can have hundreds of thousands of computational graphs. A GPU has the ability to perform a thousand or more of the computational graphs simultaneously.  This will speed up your program significantly.

Note:  Most algorithms can run without GPUs, but they will be slower.

