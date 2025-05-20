---
title: SBATCH --array=1-2     # number of jobs to run
date: 2025-05-20-00:23:54Z
type: docs 
weight: 4250
menu: 
    rivanna-alphafold:
---


module purge

module load singularity alphafold

module load anaconda

run --fasta_paths=$PWD/fasta/seq${SLURM_ARRAY_TASK_ID}.fasta \

--output_dir=$PWD/outdir \

--model_preset=monomer_ptm \

--db_preset=full_dbs \

--bfd_database_path=/data/bfd/bfd_metaclust_clu_complete_id30_c90_final_seq.sorted_opt \

--pdb70_database_path=/data/pdb70/pdb70 \

--uniref30_database_path=/data/uniref30/UniRef30_2021_03 \

--max_template_date=2023-03-29 \

--use_gpu_relax=True

conda run -p "/scratch/$USER/alphafold/conda/env" python $PWD/alphapickle/run_AlphaPickle.py \

-od outdir/seq${SLURM_ARRAY_TASK_ID}

conda run -p "/scratch/$USER/alphafold/conda/env" python $PWD/sequence_coverage_plot.py \

--input_dir outdir/seq${SLURM_ARRAY_TASK_ID} \

--output_dir outdir/seq${SLURM_ARRAY_TASK_ID} \

--name seq${SLURM_ARRAY_TASK_ID}

---

For files named “seq1, seq2, seq3…seqN”

import argparse

parser = argparse.ArgumentParser()

parser.add_argument('--input_dir',dest='input_dir',required=True)

parser.add_argument('--name',dest='name')

parser.set_defaults(name='')

parser.add_argument('--output_dir',dest='output_dir')

parser.set_defaults(output_dir='')

args = parser.parse_args()

def generate_seq_cov_plot(name, in_dir, out_dir):

import pickle

import numpy as np

import matplotlib.pyplot as plt

feature_dict = pickle.load(open(f'{in_dir}/features.pkl','rb'))

msa = feature_dict['msa']

seqid = (np.array(msa[0] == msa).mean(-1))

seqid_sort = seqid.argsort()

non_gaps = (msa != 21).astype(float)

non_gaps[non_gaps == 0] = np.nan

final = non_gaps[seqid_sort] * seqid[seqid_sort, None]

