---
title: SLURM Script for Multimers - Reduced DBs
date: 2025-05-20-00:23:54Z
type: docs 
weight: 2450
menu: 
    rivanna-alphafold:
        parent: Sample SLURM Scripts
---

This multimer script runs AlphaFold using reduced databases to save time and storage. 

> **Note:** As a consequence of the `singularity --pwd` flag, the `--fasta_paths` and `--output_dir` arguments must use **full paths** (e.g., `/scratch/$USER/mydir`) instead of relative paths (e.g., `./mydir`).  
> You can use `$PWD` to reference the directory where your SLURM script is located.  
> Make sure to **change anything marked in red** (e.g., filenames or directories) to suit your job.

```bash
#!/bin/bash​
#SBATCH -A rivanna-training      # your allocation account​
#SBATCH -p gpu          # partition​
#SBATCH --gres=gpu:1    # number of GPUs​
#SBATCH -C "v100|a100"  # request a V100 or A100 GPU​
#SBATCH -N 1            # number of nodes​
#SBATCH -c 8            # number of cores​
#SBATCH -t 10:00:00     # time​

module purge​
module load singularity alphafold​
module load anaconda​

run --fasta_paths=$PWD/fasta/ubb_multimer.fasta \​

    --output_dir=$PWD/outdir \​

    --model_preset=multimer \​

    --db_preset=reduced_dbs \​

    --pdb_seqres_database_path=/data/pdb_seqres/pdb_seqres.txt \​

    --uniprot_database_path=/data/uniprot/uniprot.fasta \​

    --small_bfd_database_path=/data/small_bfd/bfd-first_non_consensus_sequences.fasta \​

    --max_template_date=2023-04-10 \​

    --use_gpu_relax=True​


conda run -p "/scratch/$USER/alphafold/conda/env" python $PWD/alphapickle/run_AlphaPickle.py -od outdir/ubb_multimer​

conda run -p "/scratch/$USER/alphafold/conda/env" python $PWD/sequence_coverage_plot.py --input_dir outdir/ubb_multimer \​

	--output_dir outdir/ubb_multimer --name ubb_multimer​


```