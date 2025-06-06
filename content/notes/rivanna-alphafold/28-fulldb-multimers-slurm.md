---
title: Slurm Script for Multimers - Full DBs
date: 2025-05-20-00:23:54Z
type: docs 
weight: 2900
menu: 
    rivanna-alphafold:
    parent: Sample Slurm Scripts
---

This script runs AlphaFold Multimer using the full database preset. 

```bash
#!/bin/bash​

#SBATCH -A rivanna-training      # your allocation account​
#SBATCH -p gpu          # partition​
#SBATCH --gres=gpu:1    # number of GPUs​
#SBATCH -C "v100|a100"  # request a V100 or A100 GPU​
#SBATCH -N 1            # number of nodes​
#SBATCH -c 8            # number of cores​
#SBATCH -t 10:00:00     # time​

​
module purge​
module load singularity alphafold​
module load anaconda​


run --fasta_paths=$PWD/fasta/ubb_multimer.fasta \​

    --output_dir=$PWD/outdir \​

    --model_preset=multimer \​

    --db_preset=full_dbs \​

    --pdb_seqres_database_path=/data/pdb_seqres/pdb_seqres.txt \​

    --uniprot_database_path=/data/uniprot/uniprot.fasta \​

    --bfd_database_path=/data/bfd/bfd_metaclust_clu_complete_id30_c90_final_seq.sorted_opt \​

    --uniref30_database_path=/data/uniref30/UniRef30_2021_03 \​

    --max_template_date=2023-04-10 \​

    --use_gpu_relax=True​


conda run -p "/scratch/$USER/alphafold/conda/env" python $PWD/alphapickle/run_AlphaPickle.py \​

	-od outdir/ubb_multimer​

conda run -p "/scratch/$USER/alphafold/conda/env" python $PWD/sequence_coverage_plot.py \​

	--input_dir outdir/ubb_multimer --output_dir outdir/ubb_multimer --name ubb_multimer

```
