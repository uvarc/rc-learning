#!/bin/bash
#SBATCH -n 1
#SBATCH -p gpu
#SBATCH -A hpc_training
#SBATCH -t 00:05:00
#SBATCH --gres=gpu:1

module purge
module load apptainer pytorch
apptainer run --nv $CONTAINERDIR/pytorch-2.0.1.sif print(torch.cuda.is_available())
