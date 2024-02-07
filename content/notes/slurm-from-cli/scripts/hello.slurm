#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --mem=32000 # mb total memory
#SBATCH --time=1-12:00:00
#SBATCH --partition=standard
#SBATCH --account=myalloc

module purge
module load anaconda

python hello.py
