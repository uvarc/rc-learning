---
title: Machine Learning Overview
date: "2022-06-09T00:00:00"
type: docs 
weight: 100
toc: true
menu: 
    python-machine-learning:
---

Machine learning is a branch of artificial intelligence where computers _learn_ from data, and adapt the computational models to enhance performance. It is a method of analysis that allows computers to reveal information within data. The "learning" is not the type of learning that you and I do. Instead, it is a systematic approach to finding an appropriate data transformation from inputs to output.

## Why Machine Learning?

* Computers can sort through data faster than humans can. 
* Computers can identify patterns quickly and use these patterns for predictions or classifications. 
* Machine learning can handle noisy data – it doesn't find a perfect answer, but rather a "really good" answer.

## Problems that ML can solve

* __Regression__
    * Regression models determine a mathematical model for the relationship among features or attributes so that an outcome can be predicted.
    * Results can be any value within a possible range  (e.g., what will the average Earth temperature be in 2050?)
* __Classification__
    * Classification techniques identify a combination of attributes that best fits a class or category so that an object can be classified.
    * Results can be from a list of known possibilities  (e.g., is the tumor benign or malignant?)


## Types of Machine Learning

* __Supervised Learning:__
  * A data set exists where the samples can be categorized into two or more classifications.
  * The computer uses the data set to learn how to predict the classification of an unknown sample.
  * Examples include Decision Trees and Deep Learning
* __Unsupervised Learning:__
  * The collected data has no known classification or pattern.
  * The computer must identify the groups or hidden structures within the data.
  * Examples include Dendograms, K-means clustering, Self-organizing Maps
* __Reinforcement Learning:__
  * Computer learns from positive or negative feedback
  * Example includes Swarm intelligence


## Data for Machine Learning
* Data are plural, and the singular is datum
* For many Machine Learning algorithms, the data are expected to be in a table format, where:
    * each row represents an object, and
    * each column has the measurements for a specific attribute or feature of the object
* For supervised learning, the classifications of the objects must be known.
* The data with known classifications are divided into a training set and a testing set.
* The data are used to develop a model.
    * The training data are submitted to an algorithm that will fit a model to the data.
    * The test data are submitted to the model to produce predicted classifications and determine the accuracy of the model.
* Finally, the model can be used to predict classifications for "unknown" data.


## Ideas behind Machine Learning

The algorithm determines the best mathematical model for the code. However, you still need to provide a "framework" for the algorithm.The framework provides the algorithm with tools for performing the learning:
* Machine Learning Algorithm
* Data Measurements
* Model defining the relationship between the input and the output
* Label or Classification

{{< figure src=/notes/python-machine-learning/img/ml_overview.png caption="" width=75% height=75% >}}