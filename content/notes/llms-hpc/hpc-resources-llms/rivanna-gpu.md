---
title: GPU Access on Rivanna
date: "2025-02-23T00:00:00"
type: docs 
weight: 1200
menu: 
    llms-hpc:
      parent: HPC Resources for LLMs
---

__General__

Choose “GPU” or “Interactive” as the Rivanna Partition in OOD.

Optional: choose GPU type and number of GPUs.

__POD nodes__

POD nodes are contained in the gpu partition with a specific Slurm constraint.

Slurm script:

```bash
#SBATCH -p gpu
#SBATCH --gres=gpu:a100:X	# X number of GPUs
#SBATCH -C gpupod

```

Open OnDemand:

```bash
 --constraint=gpupod
```

---

Only one person can be using a GPU at a time.

