---
title: Metrics and Training Techniques
date: "2024-06-06T00:00:00"
type: docs 
weight: 500
toc: true
menu: 
    deep-learning-distributed:
        parent: Machine Learning and Deep Learning
---


Metrics are a formula for measuring the accuracy of the model.
* Examples include Accuracy and MeanSquaredError.
  * A complete list is available at [https://keras.io/api/metrics](https://keras.io/api/metrics).


## Epochs and Batch Size
* An epoch is one pass through the training loop over all of the training data.
* During each epoch, training generally occurs on batches of data at a time instead of on the full dataset all at once. The number of samples in each batch is called the batch size.
* Smaller batch sizes
  * guarantee that a batch can fit on the GPU
  * allow the model to generalize well, but increase training time

## Training, Validation, and Test Sets
* Training set: inputs/outputs used during NN training.
* Validation set:
  * used during training to see how well the network is generalizing to unseen data, but the NN parameters are not affected by these inputs/outputs.
  * Inputs are sent to the NN during each epoch and the loss/metrics are calculated on this set of inputs/outputs.
  * If NN performance on the validation set is poor, this is a sign that the NN hyperparametes (e.g. number of layers, number of neurons, etc.) need to be adjusted.
* Test set: Unseen data used to measure the performance of the NN.


## Callbacks 
* Generally, we set the number of epochs for a model to train.
* However, if model performance on the validation set is not improving, it would be nice if training could end early to avoid overfitting.
* __Callback:__ inputs to the `model.fit` function, dictate actions to take during training or inference
* __EarlyStopping:__ callback that stops the training process once model performance on the validation set no longer improves.
* __ModelCheckpoint:__ callback that saves the best model, e.g., the model with the lowest loss on the validation set. This is probably not the model produced at the last training epoch.


## Dropout Regularization
* Can be used on input or hidden layers
* During each training epoch a percentage of random neurons in the layer are dropped out.
* Prevents overfitting
* Neurons are only dropped out during training, not inference.