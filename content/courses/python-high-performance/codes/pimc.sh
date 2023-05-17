#!/bin/bash
#SBATCH -N 1
#SBATCH --cpus-per-task=4
#SBATCH --account=rivanna-training
#SBATCH -p instructional
#SBATCH --reservation=rivanna-training
#SBATCH -t 10:00:00

echo Running on `hostname`

module purge
module load anaconda/5.2.0-py3.6

# set the NUM_PROCS env variable for the Python script
export NUM_PROCS=$SLURM_CPUS_PER_TASK
python MonteCarloPiMC.py 100000000
