---
title: Exercise 1 
date: 2025-02-23-19:06:23Z
type: docs 
weight: 650
menu: 
    llms-hpc:
      parent: Setup
---

## Log On, Copy Materials

1. Log in to Rivanna using the Interactive partition.
  * 2 hours, 4 cores
  * Allocation: hpc_training
  * GPU: yes, 1
  * Show Additional Options: Yes 
  * Optional Slurm Option: ``` --reservation=llm_workshop ```
2. Copy the workshop folder /project/hpc_training/llms_on_hpc to your home or scratch account.
   
```cp –r /project/hpc_training/llms_on_hpc ~/<…>```

OR

```cp –r /project/hpc_training/llms_on_hpc /scratch/<ID>/<…>```

3. Open a Jupyter Notebook for PyTorch 2.4.0.
4. In the first cell of the notebook run the command pip list to see a list of software (i.e., packages) available in the PyTorch  2.4.0 kernel.

Do you see a package called “transformers” or “datasets”?

