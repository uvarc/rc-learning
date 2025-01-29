#!/bin/bash
#SBATCH -n 1
#SBATCH -p gpu
#SBATCH -A hpc_training
#SBATCH -t 00:05:00
#SBATCH --gres=gpu:1

nvidia-smi
