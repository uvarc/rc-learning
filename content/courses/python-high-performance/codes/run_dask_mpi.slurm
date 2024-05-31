#!/bin/bash
#SBATCH --ntasks=4
#SBATCH -p standard
#SBATCH -A hpc_build
#SBATCH -t 00:10:00

module load gcc/11.4.0
module load openmpi
module load anaconda

export OMPI_MCA_mpi_warn_on_fork=0
srun python dask_df_mpi.py
