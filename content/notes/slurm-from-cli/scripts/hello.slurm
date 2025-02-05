#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1 # total cores per task
#SBATCH --mem=32000 # mb total memory
#SBATCH --time=2:00:00
#SBATCH --partition=interactive
#SBATCH --account=hpc_training

module purge
module load miniforge

python hello.py
