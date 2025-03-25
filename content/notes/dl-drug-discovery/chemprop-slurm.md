---
title: Chemprop using slurm script - train
date: 2025-03-11-13:24:11Z
type: docs 
weight: 550
menu: 
    dl-drug-discovery:
---


__Set Up Resources__

-A: allocation

-p: partition

--gres=gpu:1 :use 1 gpu

-c: number of cores

-t: time limit

-J: job name

-o: standard output file (%A is the job #)

-e: standard error file (%A is the job #)

{{< figure src=/notes/dl-drug-discovery/img/Deep-Learning-Drug-Discovery_19.png >}}

module purge

module load anaconda/2023.07-py3.11

conda activate chemprop

chemprop_train --data_path <input.csv> --dataset_type regression/classification --save_dir checkpoint --features_generator rdkit_2d_normalized --no_features_scaling

Chemprop using slurm script - predict

__Set Up Resources__

-A: allocation

-p: partition

--gres=gpu:1 :use 1 gpu

-c: number of cores

-t: time limit

-J: job name

-o: standard output file (%A is the job #)

-e: standard error file (%A is the job #)

{{< figure src=/notes/dl-drug-discovery/img/Deep-Learning-Drug-Discovery_20.png >}}

module purge

module load anaconda/2023.07-py3.11

conda activate chemprop

chemprop_predict --test_path <test_set.csv> --checkpoint_dir checkpoint –prediction.csv

Chemprop using slurm script - interpret

__Set Up Resources__

-A: allocation

-p: partition

--gres=gpu:1 :use 1 gpu

-c: number of cores

-t: time limit

-J: job name

-o: standard output file (%A is the job #)

-e: standard error file (%A is the job #)

{{< figure src=/notes/dl-drug-discovery/img/Deep-Learning-Drug-Discovery_21.png >}}

module purge

module load anaconda/2023.07-py3.11

conda activate chemprop

chemprop_interpret --data_path <input.csv> --checkpoint_dir checkpoint/location --property_id 1

"auc": [

0.7532467532467532 ]

Chemprop using slurm script – hyperparameter Opt

__Set Up Resources__

-A: allocation

-p: partition

--gres=gpu:1 :use 1 gpu

-c: number of cores

-t: time limit

-J: job name

-o: standard output file (%A is the job #)

-e: standard error file (%A is the job #)

{{< figure src=/notes/dl-drug-discovery/img/Deep-Learning-Drug-Discovery_22.png >}}

module purge

module load anaconda/2023.07-py3.11

conda activate chemprop

chemprop_hyperopt --data_path <input.csv> --dataset_type regression/classification --num_iters 20 --config_save_path <path> --depth 5 --hidden_size 1600 --ffn_num_layers 1 --dropout 0.35 --epochs 30 --hyperopt_checkpoint_dir HYPEROPT_CHECKPOINT_DIR --num_folds 20

