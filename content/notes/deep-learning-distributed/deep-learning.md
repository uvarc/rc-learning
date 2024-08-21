---
title: Deep Learning
date: "2024-06-06T00:00:00"
type: docs 
weight: 350
toc: true
menu: 
    deep-learning-distributed:
        parent: Machine Learning and Deep Learning
---

Deep learning is a branch of artificial intelligence, where programs use multiple layers of **neural networks** to transform a set of input values to output values.

{{< figure src=/notes/deep-learning-distributed/img/dnn_overview.png width=65% height=65% >}}


### Nodes

Each "node" in the neural network performs a set of computations.

{{< figure src=/notes/deep-learning-distributed/img/node_weights.png width=50% height=50% >}}

The weights, $𝑤_𝑖$, and the bias, $b$, are not known.
Each node will have its own set of unknown values. 
During training, the _“best”_ set of weights are determined that will generate a value close to $y$ for the collection of inputs $𝑥_𝑖$.

### Network of Neurons 

{{< figure src=/notes/deep-learning-distributed/img/multi_perceptron.png caption="Multi-layer Perceptron" width=55% height=55% >}}

* Different computations with different weights can be performed  to produce different outputs.
* This is called a feedforward network – all values progress from the input to the output.
* A neural network has a single hidden layer
* A network with two or more hidden layers is called a “deep neural network”.


### Deep Learning Neural Network

{{< figure src=/notes/deep-learning-distributed/img/neural_network.png caption="Multilayer Perceptron/ Fully Connected NN; Image borrowed from: http://www.kdnuggets.com/2017/05/deep-learning-big-deal.html" width=75% height=75% >}}

## DL Algorithm

During the training or “fitting” process, the Deep Learning algorithm is fed __a set of measurements/features__ and the __expected outcome__ (e.g., a label or classification). 

{{< figure src=/notes/deep-learning-distributed/img/dl_algorithm.png width=60% height=60% >}}

The algorithm determines the best weights and biases for the data.

### How Does the Machine Learn?
* Start with a random guess for the weights and biases.
  * The output values or “predicted” values of the network can be compared with the expected results/categories/labels.
* Another function, called a “loss” or “cost” function can be used to determine the overall error of the model.  
  * That error can be used to work backwards through the network and tweak the weights/biases.
    * This step is called __backward propagation__.

## Overview of the Learning Process
{{< video src="/notes/deep-learning-distributed/video/learning_process.mp4" controls="yes" >}}


## Deep Neural Network
{{< figure src=/notes/deep-learning-distributed/img/dnn_ex.gif caption="An example of a Deep Neural Network (DNN)" width=70% height=70% >}}

### DNN Examples

* Feed-Forward NN
  * Consist of an input layer, an output layer and many hidden layers that are fully connected, and can be used to build speech-recognition, image-recognition, and machine-translation software.
* Recurrent NN
  * RNNs are commonly used for image captioning, time-series analysis, natural-language processing, machine translation, etc.
* Convolution NN
  * Consist of multiple layers and are mainly used for image processing and object detection.
* And many more such as RBFNs, GANs, Modular NN, etc.

## Activation Function
{{< figure src=/notes/deep-learning-distributed/img/activation_function.png caption="" width=40% height=40% >}}
* The _activation function_ introduces nonlinearity into the model.
* Can choose different activation functions for each layer.
* Examples include ReLU, Sigmoid(binary classification), and Softmax(multiclass classification).
* A complete list is available at
  * [https://pytorch.org/docs/stable/nn.html#non-linear-activations-weighted-sum-nonlinearity](https://pytorch.org/docs/stable/nn.html#non-linear-activations-weighted-sum-nonlinearity) and [https://pytorch.org/docs/stable/nn.html#non-linear-activations-other](https://pytorch.org/docs/stable/nn.html#non-linear-activations-other)


## Loss Function
{{< figure src=/notes/deep-learning-distributed/img/loss_function.png caption="" width=50% height=50% >}}
* The _loss function_ tells us how good our model is at relating the input to the output.
* A function that will be optimized to improve the performance of the model.
* The cost value is the difference between the neural nets predicted output and the actual output from a set of labeled training data.
* The choice of loss function is based on the task.
* Examples include
  * Classification: BCELoss (Binary Cross Entropy) and Cross Entropy Loss.
  * Regression: Mean Squared Error (MSE)
    * A complete list is available at [https://pytorch.org/docs/stable/nn.html#loss-functions](https://pytorch.org/docs/stable/nn.html#loss-functions) and [https://www.tensorflow.org/api_docs/python/tf/keras/losses](https://www.tensorflow.org/api_docs/python/tf/keras/losses)


## Optimizer Function
* The _optimizer function_ is a function for tweaking/adjusting the parameters during training so that the best weights and biases are efficiently reached.
* Examples include SGD, Adam, and RMSprop.
* A complete list is available at  [https://pytorch.org/docs/stable/optim.html?highlight=optimizer#torch.optim.Optimizer](https://pytorch.org/docs/stable/optim.html?highlight=optimizer#torch.optim.Optimizer) and [https://www.tensorflow.org/api_docs/python/tf/keras/optimizers
](https://www.tensorflow.org/api_docs/python/tf/keras/optimizers)

