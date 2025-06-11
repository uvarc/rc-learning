---
title: Parallelization with Job Arrays
date: 2025-05-20-00:23:54Z
type: docs 
weight: 3800
menu: 
    rivanna-alphafold:
---

This SLURM script shows how to run multiple AlphaFold jobs in parallel using a job array. This example uses multiple input `.fasta` files: `seq1`, `seq2`, `seq3` ... `seqN`. 

```bash
#!/bin/bash​
#SBATCH -A rivanna-training      # your allocation account​
#SBATCH -p gpu          # partition​
#SBATCH --gres=gpu:1    # number of GPUs​
#SBATCH -C "v100|a100"  # request a V100 or A100 GPU​
#SBATCH -N 1            # number of nodes​
#SBATCH -c 8            # number of cores​
#SBATCH -t 10:00:00     # time​
#SBATCH --array=1-2     # number of jobs to run​

​
module purge​
module load singularity alphafold​
module load anaconda​


run --fasta_paths=$PWD/fasta/seq${SLURM_ARRAY_TASK_ID}.fasta \​

    --output_dir=$PWD/outdir \​

    --model_preset=monomer_ptm \​

    --db_preset=full_dbs \​

    --bfd_database_path=/data/bfd/bfd_metaclust_clu_complete_id30_c90_final_seq.sorted_opt \​

    --pdb70_database_path=/data/pdb70/pdb70 \​

    --uniref30_database_path=/data/uniref30/UniRef30_2021_03 \​

    --max_template_date=2023-03-29 \​

    --use_gpu_relax=True​

​
conda run -p "/scratch/$USER/alphafold/conda/env" python $PWD/alphapickle/run_AlphaPickle.py \​

    -od outdir/seq${SLURM_ARRAY_TASK_ID}​

​
conda run -p "/scratch/$USER/alphafold/conda/env" python $PWD/sequence_coverage_plot.py \​

    --input_dir outdir/seq${SLURM_ARRAY_TASK_ID} \​

    --output_dir outdir/seq${SLURM_ARRAY_TASK_ID} \​

    --name seq${SLURM_ARRAY_TASK_ID}

```