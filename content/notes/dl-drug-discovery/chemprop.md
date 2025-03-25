---
title: Deep learning in drug discovery Chemprop
date: 2025-03-11-13:24:11Z
type: docs 
weight: 350
menu: 
    dl-drug-discovery:
---


d-Message passing neural network (MPNN)

{{< figure src=/notes/dl-drug-discovery/img/Deep-Learning-Drug-Discovery_8.png >}}

Original Article:  _Analyzing learned Molecular Representations for Property Prediction, _  _Yang et al, _  _JCIM 2019_

Chemprop – a directed MPNN

MPNN operates in two phases:

(1) a message passing phase – in the context of chemical structures it is transmitting information across the molecule to build a neural representation of the molecule

(2) a readout phase – This uses the final representation of the molecule for predictions

D-MPNN uses information associated with edges – accumulative type

{{< figure src=/notes/dl-drug-discovery/img/Deep-Learning-Drug-Discovery_9.png >}}

