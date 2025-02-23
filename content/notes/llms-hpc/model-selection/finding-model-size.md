---
title: Finding Model Size on Hugging Face
date: 2025-02-23-19:06:23Z
type: docs 
weight: 1600
menu: 
    llms-hpc:
        parent: Model Selection
---


{{< figure src=/notes/llms-hpc/img/LLMS_on_HPC_7.png >}}

* Use the information on the model card

$$
\text{Model size (B)} = (\# \text{ of parameters}) \times (\text{bytes/parameter})
$$

---

{{< figure src=/notes/llms-hpc/img/LLMS_on_HPC_8.png >}}

* Use information on the Files and versions tab

* Look for the pytorch model in the list of files.  (It will have a .bin extension.)

* The size of the model will be given

