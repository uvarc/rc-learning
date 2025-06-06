---
title: Monomer PTM with Reduced DBs
date: 2025-05-20-00:23:54Z
type: docs 
weight: 3350
menu: 
    rivanna-alphafold:
    parent: Sample Slurm Scripts
---

This script runs AlphaFold in monomer PTM mode using reduced databases, providing a faster option for predicting the structure of a single protein, such as Ubiquitin B (UBB).

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


run --fasta_paths=$PWD/fasta/ubb.fasta \​

    --output_dir=$PWD/outdir \​

    --model_preset=monomer_ptm \​

    --db_preset=reduced_dbs \​

    --small_bfd_database_path=/data/small_bfd/bfd-first_non_consensus_sequences.fasta \​

    --pdb70_database_path=/data/pdb70/pdb70 \​

    --max_template_date=2023-04-17 \​

    --use_gpu_relax=True​


conda run -p "/scratch/$USER/alphafold/conda/env" python $PWD/alphapickle/run_AlphaPickle.py \​

	-od outdir/ubb​

conda run -p "/scratch/$USER/alphafold/conda/env" python $PWD/sequence_coverage_plot.py \​

	--input_dir outdir/ubb --output_dir outdir/ubb --name UBB


```

Below are runtime examples for the same UBB input file run under different settings on Rivanna:

{{< table >}}
| Setup                              | Job ID     | Runtime    |
|-----------------------------------|------------|------------|
| Reduced databases (8 cores)       | 49474005   | 00:35:26   |
| Reduced databases (16 cores)      | 49472327   | 00:46:38   |
| Full databases                    | 49066363   | 00:54:00   |
| Reduced databases (unspecified)   | 49062003   | 00:39:00   |
{{< \table >}}

> Using reduced databases significantly shortens runtime.  