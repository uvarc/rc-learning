---
title: SBATCH -t 100000     # time
date: 2025-05-20-00:23:54Z
type: docs 
weight: 1600
menu: 
    rivanna-alphafold:
---


module purge

module load singularity alphafold

module load anaconda

run --fasta_paths=$PWD/fasta/cb1.fasta \

--output_dir=$PWD/outdir \

--model_preset=monomer_ptm \

--db_preset=full_dbs \

--bfd_database_path=/data/bfd/bfd_metaclust_clu_complete_id30_c90_final_seq.sorted_opt \

--pdb70_database_path=/data/pdb70/pdb70 \

--uniref30_database_path=/data/uniref30/UniRef30_2021_03 \

--max_template_date=2023-03-29 \

--use_gpu_relax=True

conda run -p "/scratch/$USER/alphafold/conda/env" python $PWD/alphapickle/run_AlphaPickle.py -od outdir/cb1

conda run -p "/scratch/$USER/alphafold/conda/env" python $PWD/sequence_coverage_plot.py --input_dir outdir/cb1 --output_dir outdir/cb1 --name CB1

---

It’s in a singularity container with a custom wrapper run script, so you need follow our documentation rather than official deepmind github repo or other google-able resources

8 cores are requested because jackhmmr is hardcoded to have 8 cores so adding more cores won’t speed up
Alphafold isn’t multi-GPU enabled so we will only request 1 GPU


As a consequence of the Singularity --pwd flag, the fasta and output paths must be full paths (e.g. /scratch/$USER/mydir, not relative paths (e.g. ./mydir). You may use $PWD as demonstrated. 
Remember $PWD points to the directory your slurm script is in

Change red text to point to your chosen proteins’ fasta file & output directories
Yellow: parameters to change if desired
	monomer_ptm = to generate monomer models including the PAE QC metric (have to use monomer_ptm, not plain monomer, or else PAE info isn’t generated)
	max_template_date = You must provide a value for --max_template_date. If you are predicting the structure of a protein that is already in PDB and you wish to avoid using it as a template, then max_template_date must be set to be before the release date of the structure. If you do not need to specify a date, by default you can set today’s date. For example, if you are running the simulation on August 7th 2021, set -–max_template_date = 2021-08-07. 


--model_preset=

__monomer__ : This is the original model used at CASP14 with no ensembling.

__monomer_casp14__ : This is the original model used at CASP14 with num_ensemble=8, matching our CASP14 configuration. This is largely provided for reproducibility as it is 8x more computationally expensive for limited accuracy gain (+0.1 average GDT gain on CASP14 domains).

_monomer_ptm_ : This is the original CASP14 model fine tuned with the pTM head, providing a pairwise confidence measure. It is slightly less accurate than the normal monomer model.  _Required to generate PAE values useful for QC purposes._

_multimer_ : This is the [AlphaFold](https://github.com/deepmind/alphafold#citing-this-work)[-Multimer](https://github.com/deepmind/alphafold#citing-this-work) model. To use this model, provide a multi-sequence FASTA file. In addition, the UniProt database should have been downloaded.

---

Running with monomer_ptm is required to get QC metric PAE which can be important / necessary to plot and visualize QC metrics 
Otherwise you can run with plain monomer, which seems to be faster

Only one option for multimers, which we will cover later on in the workshop

--max_template_date=

You must provide a value for --max_template_date. If you are predicting the structure of a protein that is already in PDB and you wish to avoid using it as a template, then max_template_date must be set to be before the release date of the structure. If you do not need to specify a date, by default you can set today’s date. For example, if you are running the simulation on August 7th, 2021, set --max_template_date = 2021-08-07.

---

https://nostrumbiodiscovery.github.io/nbd_central_docs/software/alphafold/alphafold.html

--db_preset=

controls MSA speed/quality tradeoff

__reduced_dbs__ : This preset is optimized for speed and lower hardware requirements. It runs with a reduced version of the BFD database. It requires 8 CPU cores (vCPUs), 8 GB of RAM, and 600 GB of disk space.

__full_dbs__ : This runs with all genetic databases used at CASP14.

If you change db_preset, you must change database path arguments too

See example scripts for different use-cases

---

You can control MSA speed/quality tradeoff by adding --db_preset=reduced_dbs or --db_preset=full_dbs to the run command. We provide the following presets:
reduced_dbs: This preset is optimized for speed and lower hardware requirements. It runs with a reduced version of the BFD database. It requires 8 CPU cores (vCPUs), 8 GB of RAM, and 600 GB of disk space.
full_dbs: This runs with all genetic databases used at CASP14.


{{< figure src=/notes/rivanna-alphafold/img/Alphafold_24.png >}}

{{< figure src=/notes/rivanna-alphafold/img/Alphafold_25.png >}}

