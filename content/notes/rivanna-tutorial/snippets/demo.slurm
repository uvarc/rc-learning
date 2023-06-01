#!/bin/bash 

#SBATCH --ntasks=1
#SBATCH --mem=6000
#SBATCH --partition=standard
#SBATCH --account=your_account
#SBATCH --time=00:10:00

echo "Hello $USER, this is node $(hostname)."
