---
title: Deep Learning in the Optimization Stage
date: 2025-03-11T13:24:11Z
type: docs 
weight: 302
menu: 
    dl-drug-discovery:
        
---

In the optimization stage, the goal is to understand and improve properties of small molecule compounds.

Deep learning can enhance prediction accuracy and efficiency.

**Directed Message Passing Neural Networks (D-MPNNs)** are deep learning models that learn molecular structures by passing information along directed edges in the molecular graph.

{{< figure src=/notes/dl-drug-discovery/img/Deep-Learning-Drug-Discovery_8.png width=65% height=65% caption="Learned vs. fixed molecular representations. Traditional models use fixed descriptors, while models like Chemprop use learned representations tailored to the prediction task. (Adapted from Yang et al., JCIM 2019)">}}

**Chemprop**, developed at MIT, uses D-MPNNs for predicting molecular properties.

-------------------------------------------------------------------------------
Note: Chemprop is not well-suited for peptides.
