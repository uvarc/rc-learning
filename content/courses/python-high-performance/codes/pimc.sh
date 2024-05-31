#!/bin/bash
#SBATCH -N 1
#SBATCH --cpus-per-task=4
#SBATCH -A hpc_training  #use a valid allocation for your login
#SBATCH -p interactive
#SBATCH -t 1:00:00

echo Running on `hostname`

module purge
module load anaconda

# set the NUM_PROCS env variable for the Python script
export NUM_PROCS=${SLURM_CPUS_PER_TASK}
python MonteCarloPiMC.py 100000000
