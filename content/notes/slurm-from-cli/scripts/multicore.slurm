#SBATCH -n 1
#SBATCH -c 25
#SBATCH -p standard
#SBATCH -A myalloc
#SBATCH -t 05:00:00
#SBATCH --mail-user=mst3k@virginia.edu
#SBATCH --mail-type=END

module load gcc
export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK

./ompheatedplate .0000001 plate
