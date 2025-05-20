---
title: SBATCH -t 100000     # time
date: 2025-05-20-00:23:54Z
type: docs 
weight: 2850
menu: 
    rivanna-alphafold:
---


module purge

module load singularity alphafold

module load anaconda

run --fasta_paths=$PWD/fasta/ubb_multimer.fasta \

--output_dir=$PWD/outdir \

--model_preset=multimer \

--db_preset=reduced_dbs \

--pdb_seqres_database_path=/data/pdb_seqres/pdb_seqres.txt \

--uniprot_database_path=/data/uniprot/uniprot.fasta \

--small_bfd_database_path=/data/small_bfd/bfd-first_non_consensus_sequences.fasta \

--max_template_date=2023-04-10 \

--use_gpu_relax=True

conda run -p "/scratch/$USER/alphafold/conda/env" python $PWD/alphapickle/run_AlphaPickle.py -od outdir/ubb_multimer

conda run -p "/scratch/$USER/alphafold/conda/env" python $PWD/sequence_coverage_plot.py --input_dir outdir/ubb_multimer \

--output_dir outdir/ubb_multimer --name ubb_multimer


---

As a consequence of the Singularity --pwd flag, the fasta and output paths must be full paths (e.g. /scratch/$USER/mydir, not relative paths (e.g. ./mydir). You may use $PWD as demonstrated.

Remember $PWD points to the directory your slurm script is in


Change red text as needed

