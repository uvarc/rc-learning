---
title: Chemprop
date: 2025-03-11-13:24:11Z
type: docs 
weight: 350
menu: 
    dl-drug-discovery:
    parent: Deep Learning in the Optimization Stage
---

{{< figure src=/notes/dl-drug-discovery/img/Deep-Learning-Drug-Discovery_9.png >}}

Chemprop is a deep learning tool for predicting small molecule properties using a Directed Message Passing Neural Network (D-MPNN).

D-MPNNs operate in two phases:

* Message Passing Phase – Transmits information across atoms and bonds to build a learned representation of the molecule.

* Readout Phase – Uses this final representation to make property predictions.
  
D-MPNNs use edge-based information in an accumulative way. 

They start with a binary representation of the chemical structure, where the position of each substructure is encoded. Carbon atoms are implied when not explicitly shown, and the compound of interest is often nitrogen. The encoding captures directionality of both atoms and bonds, generating vectors that are merged and used to train the model. This approach is enhanced by deep learning.