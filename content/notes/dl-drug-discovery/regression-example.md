---
title: Regression Example in Drug Discovery
date: 2025-03-11-13:24:11Z
type: docs 
weight: 410
menu: 
    dl-drug-discovery:
    parent: Classification and Regression in ML
---

{{< figure src=/notes/dl-drug-discovery/img/Deep-Learning-Drug-Discovery_13.png >}}

The input data shown above is in CSV file format. It shows the small molecule in computer-readable format using SMILES notation. Each line includes a compound and its associated lipophilicity value.

Lipophilicity is important when evaluating whether a drug component has the potential to fail, especially if it prevents a compound from advancing through preclinical stages.

There’s a data engineering aspect involved: each compound is paired with a corresponding numeric value. The value can be positive or negative depending on the context. A negative value may indicate an undesirable compound, while a positive value may suggest one worth pursuing—or the reverse, depending on the specific goal of the model.

The main concern is creating a model that, when given unknown compounds, can accurately predict the lipophilicity. 

{{< figure src=/notes/dl-drug-discovery/img/Deep-Learning-Drug-Discovery_14.png >}}

This code example shows the training setup in a Jupyter notebook. 



