---
title: Smile Format
date: 2025-03-11-13:24:11Z
type: docs 
weight: 450
menu: 
    dl-drug-discovery: 
        parent: Deep Learning in the Optimization Stage
        
---

The same chemical compound can be represented in multiple ways — by its structural diagram, its full chemical name, or in a format interpretable by machine learning models. The **SMILES format** (Simplified Molecular Input Line Entry System) is a line notation that encodes molecular structures as strings, allowing computers to process chemical information efficiently.

{{< figure src=/notes/dl-drug-discovery/img/Deep-Learning-Drug-Discovery_15.png width=85% height=85% >}}

In the example shown above, **2-thiobarbituric acid** is displayed in three forms:

- **Chemical name**: 2-thioxodihydropyrimidine-4,6(1H,5H)-dione  
- **SMILES string**: `O=C1CC(=O)NC(=S)N1`  
- **Structural diagram**

When preparing data for training models, it's important to **split the dataset meaningfully** based on chemical properties. Instead of random splits, a **scaffold-based split** groups compounds by their core structure, which provides a more realistic and chemically meaningful distribution for testing model performance.

