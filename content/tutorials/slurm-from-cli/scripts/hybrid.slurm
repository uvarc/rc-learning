#!/bin/bash
#SBATCH -N 3
#SBATCH --ntasks-per-node=4
#SBATCH -c 10
#SBATCH -p parallel
#SBATCH -A hpc_training
#SBATCH -t 05:00:00
#SBATCH --mail-user=mst3k@virginia.edu
#SBATCH --mail-type=END

module load gcc
export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK

srun ./heatedplate .0000001 plate${SLURM_NTASKS_PER_NODE}

