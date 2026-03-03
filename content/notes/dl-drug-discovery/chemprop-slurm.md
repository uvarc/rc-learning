---
title: Chemprop With Slurm Scripts
date: 2025-03-11T13:24:11Z
type: docs 
weight: 550
menu: 
    dl-drug-discovery:
        
---

Chemprop can be run on an HPC cluster using SLURM scripts. Depending on your setup, there are two main ways to launch Chemprop jobs:

- **Using Miniforge/Conda environments** (as shown in the examples below)
- **Using Apptainer/Docker containers**, which requires pulling a Docker image and using `module load apptainer` and related modifications

Chemprop supports multiple core functions such as training, prediction, interpretation, and hyperparameter optimization. The exact featurization and dataset configuration depends on your specific task and dataset type (e.g., small therapeutics, regression vs. classification, etc.).

### Slurm Resource Setup

* -A: allocation
* -p: partition
* --gres=gpu:1 : use 1 gpu
* -c: number of cores
* -t: time limit
* -J: job name
* -o: standard output file (%A is the job #)
* -e: standard error file (%A is the job #)
  
### Chemprop: SLURM Script for Training

```bash
#!/bin/bash
#SBATCH -A mygroup
#SBATCH -p gpu          #1
#SBATCH --gres=gpu:1    #1
#SBATCH -c 1
#SBATCH -t 00:01:00
#SBATCH -J pytorchtest
#SBATCH -o pytorchtest-%A.out
#SBATCH -e pytorchtest-%A.err

module purge​
module load anaconda/2023.07-py3.11​
conda activate chemprop​

chemprop_train --data_path <input.csv> --dataset_type regression/classification --save_dir checkpoint --features_generator rdkit_2d_normalized --no_features_scaling​
```


### Chemprop: SLURM Script for Prediction


```bash
#!/bin/bash
#SBATCH -A mygroup
#SBATCH -p gpu          #1
#SBATCH --gres=gpu:1    #1
#SBATCH -c 1
#SBATCH -t 00:01:00
#SBATCH -J pytorchtest
#SBATCH -o pytorchtest-%A.out
#SBATCH -e pytorchtest-%A.err

module purge​
module load anaconda/2023.07-py3.11​
conda activate chemprop​

chemprop_predict --test_path <test_set.csv> --checkpoint_dir checkpoint –prediction.csv
```


### Chemprop: SLURM Script for Interpretation


```bash
#!/bin/bash
#SBATCH -A mygroup
#SBATCH -p gpu          #1
#SBATCH --gres=gpu:1    #1
#SBATCH -c 1
#SBATCH -t 00:01:00
#SBATCH -J pytorchtest
#SBATCH -o pytorchtest-%A.out
#SBATCH -e pytorchtest-%A.err

module purge​
module load anaconda/2023.07-py3.11​
conda activate chemprop​

chemprop_interpret --data_path <input.csv> --checkpoint_dir checkpoint/location --property_id 1
```

Sample output: 

`"auc": [
0.7532467532467532 ]`

### Chemprop: SLURM Script for Hyperparameter Optimization

```bash
#!/bin/bash
#SBATCH -A mygroup
#SBATCH -p gpu          #1
#SBATCH --gres=gpu:1    #1
#SBATCH -c 1
#SBATCH -t 00:01:00
#SBATCH -J pytorchtest
#SBATCH -o pytorchtest-%A.out
#SBATCH -e pytorchtest-%A.err

module purge​
module load anaconda/2023.07-py3.11​
conda activate chemprop​

chemprop_hyperopt --data_path <input.csv> --dataset_type regression/classification --num_iters 20 --config_save_path <path> --depth 5 --hidden_size 1600 --ffn_num_layers 1 --dropout 0.35 --epochs 30 --hyperopt_checkpoint_dir HYPEROPT_CHECKPOINT_DIR --num_folds 20
```

