---
title: Random Forest
date: "2022-06-09T00:00:00"
type: docs 
weight: 250
toc: true
menu: 
    python-machine-learning:
---


Random forest is a classification algorithm within supervised learning. It uses an __Ensemble Technique__.
  * Ensemble techniques combine a group of "weaker" learning techniques to build a stronger technique.
  * Random Forest combines the results of multiple decision trees to create a more robust result.

## Random Forest:  How does it work?

{{< figure src=/notes/python-machine-learning/img/random_forest_dt.png caption="Image borrowed from [https://ai-pool.com/a/s/random-forests-understanding](https://ai-pool.com/a/s/random-forests-understanding)" width=70% height=70% >}}



Different decision tree algorithms can produce different results.The random forest aggregates the decisions from the trees to determine an overall solution.

Suppose that the data fall into one of two categories (blue or orange) depending on two values, x and y, as shown in this figure:
{{< figure src=/notes/python-machine-learning/img/linear_decision.png caption="" width=25% height=25% >}}

A decision tree could choose a relationship between x, y, and the categories that matches one of the following figures:
{{< figure src=/notes/python-machine-learning/img/decision_tree_choices.png caption="" width=50% height=50% >}}

By combining the many, many outcomes, the random forest can approach the desired mapping.
{{< figure src=/notes/python-machine-learning/img/random_forest.png caption="" width=60% height=60% >}}

Random Forests can use different techniques for selecting features for computing each decision value. This can lead to the choice of different features.

{{< figure src=/notes/python-machine-learning/img/random_forest_tree.png caption="" width=70% height=70% >}}


## Random Forest:  Feature Importance

{{< figure src=/notes/python-machine-learning/img/feature_importance.png caption="" width=50% height=50% >}}

* We would like to know the "importance" of the features (e.g., which features are the most important for making decisions).
* Different algorithms use various metrics to determine the importance of the features.

The value of the measurements are not as important as the order of the features.

