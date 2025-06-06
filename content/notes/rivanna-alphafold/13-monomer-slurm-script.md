---
title: Slurm Script for Monomers 
date: 2025-05-20-00:23:54Z
type: docs 
weight: 1200
menu: 
    rivanna-alphafold:
---

This script uses the full AlphaFold database set.

> **Note:** This workflow runs AlphaFold inside a Singularity container using a **custom wrapper script**, not the official DeepMind `run_alphafold.py`. Be sure to follow this documentation rather than other online guides.

```bash
#!/bin/bash​
#SBATCH -A rivanna-training    # your allocation account​
#SBATCH -p gpu          # partition​
#SBATCH --gres=gpu:1    # number of GPUs (only 1 because Alphafold isn’t multi-GPU enabled)​
#SBATCH -C "v100|a100"  # request a V100 or A100 GPU​
#SBATCH -N 1            # number of nodes​
#SBATCH -c 8            # number of cores (jackhmmr is hardcoded to use 8 cores, more does not speed it up)​
#SBATCH -t 10:00:00     # time​

module purge​
module load singularity alphafold​
module load anaconda​
​
run --fasta_paths=$PWD/fasta/cb1.fasta \​

    --output_dir=$PWD/outdir \​

    --model_preset=monomer_ptm \​

    --db_preset=full_dbs \​

    --bfd_database_path=/data/bfd/bfd_metaclust_clu_complete_id30_c90_final_seq.sorted_opt \​

    --pdb70_database_path=/data/pdb70/pdb70 \​

    --uniref30_database_path=/data/uniref30/UniRef30_2021_03 \​

    --max_template_date=2023-03-29 \​

    --use_gpu_relax=True​

​
conda run -p "/scratch/$USER/alphafold/conda/env" python $PWD/alphapickle/run_AlphaPickle.py -od outdir/cb1​

conda run -p "/scratch/$USER/alphafold/conda/env" python $PWD/sequence_coverage_plot.py --input_dir outdir/cb1 --output_dir outdir/cb1 --name CB1​

```

### Important Notes

- **Container behavior:**  
  The Singularity container uses the `--pwd` flag, which means **you must provide full (absolute) paths** for both the FASTA input and output directory. Use `$PWD` as shown in the example to simplify this.

- **GPU usage:**  
  AlphaFold is **not multi-GPU enabled**, so only **one GPU** should be requested.

- **CPU usage:**  
  The `jackhmmer` tool used by AlphaFold is hardcoded to use **8 CPU cores**, so requesting more than 8 cores will not improve performance.

- **Editable Parameters:**
  - **Red text in the script**: Replace with the path to your own FASTA file and desired output directory.
  - **Yellow-highlighted options**:
    - `--model_preset=monomer_ptm`: Use this to generate monomer models with PAE (Predicted Aligned Error) metrics. Do **not** use `monomer` if you want PAE included.
    - `--max_template_date=YYYY-MM-DD`: This must be set. If you don’t need to avoid specific PDB templates, you can use the current date.


### Important Options

#### `--model_preset=​`
* `monomer`: This is the original model used at CASP14 with no ensembling.​

* `monomer_casp14`: This is the original model used at CASP14 with num_ensemble=8, matching our CASP14 configuration. This is largely provided for reproducibility as it is 8x more computationally expensive for limited accuracy gain (+0.1 average GDT gain on CASP14 domains).​

* `monomer_ptm`: This is the original CASP14 model fine tuned with the pTM head, providing a pairwise confidence measure. It is slightly less accurate than the normal monomer model. Running with `monomer_ptm` is required to get QC metric PAE,which can be important / necessary to plot and visualize QC metrics. Otherwise you can run with plain `monomer`, which seems to be faster.

* `multimer`: This is the AlphaFold-Multimer model. To use this model, provide a multi-sequence FASTA file. In addition, the UniProt database should have been downloaded.​

#### `--max_template_date=`

You must provide a value for `--max_template_date`. If you are predicting the structure of a protein that is already in PDB and you wish to avoid using it as a template, then `max_template_date` must be set to be before the release date of the structure. If you do not need to specify a date, by default you can set today’s date. For example, if you are running the simulation on August 7th, 2021, set `--max_template_date = 2021-08-07`. ​[More Information](https://nostrumbiodiscovery.github.io/nbd_central_docs/software/alphafold/alphafold.html)

#### `--db_preset=​`
This option controls the MSA speed/quality tradeoff. 
* `reduced_dbs` is optimized for speed and lower hardware requirements. It runs with a reduced version of the BFD database. It requires 8 CPU cores (vCPUs), 8 GB of RAM, and 600 GB of disk space.​
* `full_dbs` runs with all genetic databases used at CASP14.​
If you change `db_preset`, you must change database path arguments too.

