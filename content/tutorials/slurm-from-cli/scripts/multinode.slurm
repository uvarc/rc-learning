#!/bin/bash
#SBATCH --nodes=3
#SBATCH --ntasks-per-node=40
#SBATCH --account=hpc_training
#SBATCH -p parallel
#SBATCH -t 10:00:00
#SBATCH --mail-user=mst3k@virginia.edu
#SBATCH --mail-type=END

module load gcc openmpi

srun ./mpiheatedplate .0000001 plate${SLURM_NTASKS_PER_NODE}
