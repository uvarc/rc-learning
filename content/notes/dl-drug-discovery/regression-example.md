---
title: Regression in Drug Discovery
date: 2025-03-11-13:24:11Z
type: docs 
weight: 410
menu: 
    dl-drug-discovery:
        parent: Deep Learning in the Optimization Stage
         
---

{{< figure src=/notes/dl-drug-discovery/img/Deep-Learning-Drug-Discovery_13.png width=45% height=45% >}}

The sample input data shown above is in CSV file format. It shows the small molecule in computer-readable format using SMILES notation. Each line includes a compound and its associated lipophilicity value.

Lipophilicity is important when evaluating whether a drug component has the potential to fail, especially if it prevents a compound from advancing through preclinical stages.

There’s a data engineering aspect involved: each compound is paired with a corresponding numeric value. The value can be positive or negative depending on the context. A negative value may indicate an undesirable compound, while a positive value may suggest one worth pursuing—or the reverse, depending on the specific goal of the model.

The main concern is creating a model that, when given unknown compounds, can accurately predict the lipophilicity. 

```python
import pandas as pd
from pathlib import Path
from lightning import pytorch as pl 
from chemprop import data, featurizers, models, nn
```

>This code example shows the training setup in a Jupyter notebook. 



