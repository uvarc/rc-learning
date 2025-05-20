---
title: SBATCH -t 100000     # time
date: 2025-05-20-00:23:54Z
type: docs 
weight: 3750
menu: 
    rivanna-alphafold:
---


module purge

module load singularity alphafold

module load anaconda

run --fasta_paths=$PWD/fasta/ubb.fasta \

--output_dir=$PWD/outdir \

--model_preset=monomer_ptm \

--db_preset=reduced_dbs \

--small_bfd_database_path=/data/small_bfd/bfd-first_non_consensus_sequences.fasta \

--pdb70_database_path=/data/pdb70/pdb70 \

--max_template_date=2023-04-17 \

--use_gpu_relax=True

conda run -p "/scratch/$USER/alphafold/conda/env" python $PWD/alphapickle/run_AlphaPickle.py \

-od outdir/ubb

conda run -p "/scratch/$USER/alphafold/conda/env" python $PWD/sequence_coverage_plot.py \

--input_dir outdir/ubb --output_dir outdir/ubb --name UBB

---

UBB reduced_dbs = 39 minutes (job 49062003)
UBB full_dbs = 54 minutes (job 49066363)

UBB reduced_dbs with 16 cores (job 49472327): 00:46:38
UBB reduced_dbs with 8 cores (job 49474005): 00:35:26

