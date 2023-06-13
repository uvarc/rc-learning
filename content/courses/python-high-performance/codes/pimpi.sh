#!/bin/bash
#SBATCH -N 1
#SBATCH --ntasks-per-node=8
#SBATCH --account=your_allocation
#SBATCH -p instructional
#SBATCH -t 10:00:00

echo Running on `hostname`

module purge

#Load the versions of gcc, MPI, and Python/Anaconda in which you installed mpi4py
module load gcc
module load openmpi
module load anaconda

srun python MonteCarloPiMPI.py 100000000
