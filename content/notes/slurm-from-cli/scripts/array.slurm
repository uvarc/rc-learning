#!/bin/bash
#SBATCH --ntasks=1
#SBATCH --partition=interactive
#SBATCH -A hpc_training
#SBATCH --time=3:00:00
#SBATCH -o out%A.%a
#SBATCH -e err%A.%a

python myscript.py myinput.${SLURM_ARRAY_TASK_ID}.in
