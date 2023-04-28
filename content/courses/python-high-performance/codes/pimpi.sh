#!/bin/bash
#SBATCH -N 1
#SBATCH --ntasks-per-node=8
#SBATCH --account=rivanna-training
#SBATCH -p instructional
#SBATCH --reservation=rivanna-training
#SBATCH -t 10:00:00

echo Running on `hostname`

module purge
module load gcc/7.1.0
module load openmpi/3.1.4
module load mpi4py/3.0.0-py3.6

srun python MonteCarloPiMPI.py 100000000
