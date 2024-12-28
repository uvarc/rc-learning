---
title: Neural Networks
date: "2022-06-09T00:00:00"
type: docs 
weight: 350
toc: true
menu: 
    python-machine-learning:
---


Neural networks are a computational model used in machine learning which is based on the biology of the human brain.

The building blocks of a neural network are neurons, also known as nodes. Within each node is a very basic algebraic formula that transforms the data.

## Simulation of a Neuron

{{< figure src=/notes/python-machine-learning/img/neuron_simulation.png caption="" width=60% height=60% >}}

The "incoming signals" would be values from a data set.

A simple computation (like a weighted sum) is performed by the "nucleus".
Then, an "activation" function is used to determine if the output is "on" or "off".

The weights, $w_i$, and the bias $b$, are not known at first. Random guesses are chosen. During training, the "best" set of weights are determined that will generate a value close to $y$ for the collection of inputs $x_i$.

## Network of Nodes

A single node does not provide much information (often times, a 0 or 1 value), but creating a network or layer of nodes will provide more information.

{{< figure src=/notes/python-machine-learning/img/node_network.png caption="" width=60% height=60% >}}

Different computations with different weights can be performed  to produce different outputs. This is called a feedforward network – all values progress from the input to the output.

## The Layers of a Network

{{< figure src=/notes/python-machine-learning/img/neuron_network_layers.png caption="" width=60% height=60% >}}

A neural network has a single hidden layer. A neural network with two or more hidden layers is called a "deep neural network".

## How does the machine learn?

The output values or "predicted" values of the network can be compared with the expected results/categories/labels.

* Start with a random guess for the weights and biases.
* Another function, called a "loss" or "cost" function can be used to determine the overall error of the model.
* That error can be used to work backwards through the network and tweak the weights/biases.
    * This step is called  _backward propagation_ .

## Overview of the Learning Process

{{< video src="/notes/python-machine-learning/video/learning_process.mp4" controls="yes" >}}

## Activation Function

* An _activation function_ will determine if a node should "fire".
* Examples include relu, sigmoid, and softmax.
* A complete list is available at [https://keras.io/api/layers/activations/](https://keras.io/api/layers/activations/) .


## Loss Function

* A _loss function_ is a function that will be optimized to improve the performance of the model.
* Examples include BinaryCrossEntropy and CategoricalCrossEntropy.
* A complete list is available at [https://keras.io/api/losses/](https://keras.io/api/losses/) .

__Metrics:__ A formula for measuring the accuracy of the model.
* Examples include Accuracy and MeanSquaredError.
* A complete list is available at [https://keras.io/api/metrics](https://keras.io/api/metrics).

## Optimizer functions

* The function for tweaking the weights.
* Examples include SGD, Adam, and RMSprop.
* A complete list is available at [https://keras.io/api/optimizers/](https://keras.io/api/optimizers/) .

## Epochs and Batch Size

__Epochs:__   Number of loops – how many times the forward/backward process should be performed.

__Batch Size:__  Within an epoch, the training data are divided into small batches and sent through the network.  All training data are processed in an epoch.

