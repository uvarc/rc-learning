---
title: Decision Trees
date: "2022-06-09T00:00:00"
type: docs 
toc: true
weight: 150
menu: 
    python-machine-learning:
---


Decision trees are a classification algorithm within supervised learning. The algorithm determines a set of questions or tests that will guide it toward a classification of an observation and it organizes a series of attribute tests into a tree-structure to help determine classification of the unlabeled data.

> Motivating Question:
> Given a set of data, can we determine which attributes should be tested first to predict a category or outcome (i.e., which attributes lead to "high information gain")?

## Simple Scenario 

Suppose we have:
* a group of people, each one with a tumor, and
* two measurements (x, y) for each tumor.

Plotting the data, and coloring the points red for malignant tumors and blue for benign tumors, we might see a plot as follows:

{{< figure src=/notes/python-machine-learning/img/pre_decision_plot.png caption="" width=60% height=60% >}}

Clearly, something happens near x=3.

{{< figure src=/notes/python-machine-learning/img/decision_plot.png caption="" width=60% height=60% >}}

With very few errors, we can use x=3 as our "decision" to categorize the tumor as malignant versus benign.

__Resulting decision tree:__

{{< figure src=/notes/python-machine-learning/img/result_decision_tree.png caption="" width=30% height=30% >}}

Unfortunately, it is not always this easy, especially if we have much more complex data. More layers of questions can be added with more attributes.


## Example: What should you do this weekend?

{{< table >}} 
| Weather | Parents Visiting | Have extra cash | Weekend Activity |
| :-: | :-: | :-: | :-: |
| Sunny | Yes | Yes | Cinema |
| Sunny | No | Yes | Tennis |
| Windy | Yes | Yes | Cinema |
| Rainy | Yes | No | Cinema |
| Rainy | No | Yes | Stay In |
| Rainy | Yes | No | Cinema |
| Windy | No | No | Cinema |
| Windy | No | Yes | Shopping |
| Windy | Yes | Yes | Cinema |
| Sunny | No | Yes | Tennis |
{{< /table >}}

This table can be represented as a tree. 

{{< figure src=/notes/python-machine-learning/img/tree_first.png caption="" width=65% height=65% >}}

This tree can be made more efficient. 

{{< figure src=/notes/python-machine-learning/img/tree_second.png caption="" width=50% height=50% >}}

Also with complex data, it is possible that not all features are needed in the Decision Tree.

## Decision Tree Algorithms

There are many existing Decision Tree algorithms. If written correctly, the algorithm will determine the best question/test for the tree.

> How do we know how accurate our decision tree is?

## Decision Tree Evaluation

* A confusion matrix is often used to show how well the model matched the actual classifications.
  * The matrix is not confusing – it simply illustrates how "confused" the model is!
* It is generated based on test data.

{{< figure src=/notes/python-machine-learning/img/decision_tree_chart.png caption="" width=70% height=70% >}}


