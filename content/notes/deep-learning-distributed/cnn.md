---
title: Convolutional Neural Networks
date: "2024-06-06T00:00:00"
type: docs 
weight: 600
toc: true
menu: 
    deep-learning-distributed:
      parent: Machine Learning and Deep Learning
---

## What are Convolutional Neural Networks?
* Originally, convolutional neural networks (CNNs) were a technique for analyzing images.
* Applications have expanded to include analysis of text, video, and audio.
* CNNs apply __multiple neural networks__ to _subsets_ of a whole image in order to identify parts of the image.

### The idea behind CNN
{{< figure src=/notes/deep-learning-distributed/img/cnn_idea.jpg width=55% height=55% >}}
* Recall the old joke about the blind-folded scientists trying to identify an elephant.
* A CNN works in a similar way. It breaks an image down into smaller parts and tests whether these parts match known parts.
* It also needs to check if specific parts are within certain proximities.
   For example, the tusks are near the trunk and not near the tail.

**Is the image on the left most like an X or an O?**
{{< figure src=/notes/deep-learning-distributed/img/most_alike.png caption="Images borrowed from http://brohrer.github.io/how_convolutional_neural_networks_work.html" width=40% height=40% >}}

**What features are in common?**

{{< figure src=/notes/deep-learning-distributed/img/features_in_common.png width=65% height=65% caption="" >}}

## Building blocks of CNN

CNN performs a combination of **layers**
* Convolution Layer
  * This layer compares a feature with all subsets of the image
  * It creates a map showing where the comparable features occur
* Rectified Linear Units (ReLU) Layer
  * This layer goes through the features maps and replaces negative values with $0$
* Pooling Layer
  * This layer reduces the size of the rectified feature maps by taking the maximum value of a subset

The CNN ends with a **final layer**
* Classification (Fully-connected layer) layer
  * This combines the specific features to determine the classification of the image


{{< figure src=/notes/deep-learning-distributed/img/cnn_steps.png caption="Convolution → Rectified Linear → Pooling" width=70% height=70% >}}

These layers can be repeated multiple times. The final layer converts the final feature map to the classification.

{{< figure src=/notes/deep-learning-distributed/img/final_layer.png width=60% height=60% >}}

## Example: MNIST Data

{{< figure src=/notes/deep-learning-distributed/img/mnist.jpg caption="Image borrowed from Getting Started with TensorFlow by Giancarlo Zaccone" width=40% height=40% >}}

* The MNIST data set is a collection of hand-written digits (e.g., 0-9).
* Each digit is captured as an image with 28x28 pixels.
* The data set is already partitioned into a training set (60,000 images) and a test set (10,000 images).
* __The tensorflow packages have tools for reading in the MNIST datasets.__
* More details on the data are available at [http://yann.lecun.com/exdb/mnist/](http://yann.lecun.com/exdb/mnist/)


## Why Use GPUs? 
{{< figure src=/notes/deep-learning-distributed/img/params_overtime.png width=60% height=60% >}}

Over time, bigger models have been developed to handle more complex tasks, and consequently, to handle more computations. The training process involves hundreds of thousands of computations, and we need a form of parallelization to speed up the process. 

HPC systems can help meet this demand through specialized hardware, like GPUs which can provide the needed parallelization, and other hardware. 



<!-- ## Coding a Model with CNN: General Steps
1. Load the data
2. Preprocess the data.
    1. Capture the sizes
    2. Reshape the data
3. Design the Network Model
4. Train the model
5. Apply the model to the test data
6. Display the results

Here is some good example code: https://machinelearningmastery.com/tensorflow-tutorial-deep-learning-with-tf-keras/

The next page will cover an exercise with Tensorflow and CNN where we will load data, pre-process the data, design the network model, train the model, and apply the model to test data.  -->
