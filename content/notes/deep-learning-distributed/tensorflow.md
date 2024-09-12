---
title: TensorFlow
date: "2024-06-06T00:00:00"
type: docs 
weight: 1100
toc: true
menu: 
    deep-learning-distributed:
---

Tensorflow is a software library, developed by the Google Brain Team.

TensorFlow already has the code to assign the data to the GPUs and do the heavy computational work; we simply have to give it the specifics for our data and model.

* Tensorflow is an example of __deep learning__ \(a neural network that has many layers\).
* Keras is an open-source deep-learning library in Python that provides an easy-to-use interface to TensorFlow.
* _tf.keras_ is the Keras API integrated into TensorFlow 2
* More information can be found: https://www.tensorflow.org/guide/mixed_precision

### Teminology:  Tensors
* Tensor: A multi-dimensional array
* Example: A sequence of images can be represented as a 4-D array: [image_num, row, col, color_channel]
* Tensors can be used on a GPU

{{< figure src=/notes/deep-learning-distributed/img/tensors.png caption="" width=60% height=60% >}}

## Install Tensorflow

* Conda
  * `conda create -n tf2-gpu tensorflow-gpu -c conda-forge`
* Container (available as module and OOD)
  * `apptainer exec --nv $CONTAINERDIR/tensorflow-2.13.0.sif python mycode.py`
* Build from source
  * https://www.tensorflow.org/install/source
  * TensorFlow will run GPU-enabled operations on the GPU by default. However,  more than one GPU requires appropriate changes within the TensorFlow script.


## Performance and Profiling

* Multithreading should give a substantial speed-up for large input pipelines via tf.data where the ETL takes place on the (multicore) CPU only. Use multiple CPU-cores to prepare the data and keep the GPU busy.
```python
 train_dataset = datasets['train'].map(preprocess_data, num_parallel_calls=tf.data.AUTOTUNE)\
 .cache()\
 .shuffle(SHUFFLE_SIZE)\
 .batch(BATCH_SIZE)\
 .prefetch(tf.data.AUTOTUNE)
```

* TensorBoard:
  * It can be used to view your graph, monitor training progress and more. Included in a Conda installation of TensorFlow.
  * Instructions available on the RC site: [Tensorflow and UVA HPC](https://www.rc.virginia.edu/userinfo/hpc/software/tensorflow/)

## Profiling
* line_profiler
  * An excellent starting point for profiling any Python script is line_profiler. This will provide high-level profiling data such as the amount of time spent on each line of your script.
* Tensorflow Profiler and debugger embedded within tensorboard (GPU required).
  * Additional information can be found: https://www.tensorflow.org/tensorboard/tensorboard_profiling_keras
* Nsys and ncu
* TensorRT is an SDK for high-performance deep learning inference. You can either use the container from NVIDIA with Singularity or build from source.

> [A Guide to Tensorflow Performance Optimization](https://tigress-web.princeton.edu/~jdh4/TensorflowPerformanceOptimization_GTC2021.pdf)


## Coding a Tensorflow Model: General Steps

1. Import Modules
2. Read in the data
3. Divide the data into a training set and a test set.
4. Preprocess the data
5. Design the Network Model
6. Train the model: Compile, Checkpointing, EarlyStopping and Fitting
7. Apply the model to the test data and display the results
8. Loading a checkpointed model


Make sure that you can run the CNN code:
  * TF_CNN_SingleGPU.ipynb


<!-- The next page will provide an exercise with Tensorflow where we will import modules, read in data, split the data, pre-process the data, design the model, and apply the model to test data and evaluate. Additionally, we will provide instructions on how to **load a checkpointed NN model** and **load a saved model**. (TAKE OUT THIS PAGE AND ACTIVITY) -->

