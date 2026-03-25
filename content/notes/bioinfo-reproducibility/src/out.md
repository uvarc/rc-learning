# Reproducibility in Bioinformatics

# Deb Triant & Marcus Bobar
Research Computing, University of Virginia
dtriant@virginia.edu, mb5wt@virginia.edu

![](img/Triant-Bobar_Reproducibility_0.png)

---

Introductions


# Workshop outline

Difficulties in achieving reproducibility

Potential problems with bioinformatics pipelines

Some helpful tools

Snakemake & Nextflow examples

![](img/Triant-Bobar_Reproducibility_1.png)

# Reproducibility in science



* Reproducibility \- redo a scientific experiment & generate similar results
  * Same sample\, software\, data\, code \- same result?
* Replication \- different data\, same methods \- conclusions consistent?
* Reusability \- Will someone be able to use your pipeline in the future?
  * \- Will you be able to use it?


![](img/Triant-Bobar_Reproducibility_2.png)

# Reproducibility Problem

Where did you do the analysis \- laptop\, server\, lab computer\, environment

Are you using the most recent version \(scripts\, datasets\, analyses\)

We just used the default settings\!

![](img/Triant-Bobar_Reproducibility_3.png)

![](img/Triant-Bobar_Reproducibility_4.png)

# Studies in reproducibility

Nature \(2016\) \- Found that 70% of researchers have failed in reproducing another researcher’s results & >50% failed to reproduce their own

PLoS Biology \(2024\) \- Biomedical researchers \- 72% reported “reproducibility crisis”

Genome Biol \(2024\) \- Reproducibility in bioinformatics era

![](img/Triant-Bobar_Reproducibility_5.png)

![](img/Triant-Bobar_Reproducibility_6.png)

# Challenges of Bioinformatics



* So many tools\, often with:
  * Multiple versions & releases
  * Complex dependencies & hidden parameters\, starting seeds
  * Running tools locally vs on HPC
  * Formatting conversions between software
  * Scalability \- how tools handle datasets increasing in size
  * Keeping codes organized\!


![](img/Triant-Bobar_Reproducibility_7.png)

# Aspects of reproducibility

Version control

Environment management

Data storage

Containers

Tool/software maintenance

![](img/Triant-Bobar_Reproducibility_8.png)

# Saving document versions

![](img/Triant-Bobar_Reproducibility_9.gif)

![](img/Triant-Bobar_Reproducibility_10.png)

# Version Control

![](img/Triant-Bobar_Reproducibility_11.png)

GitHub: https://github\.com

Track and manage changes to your code & files

Store and label changes at every step

Small or large projects

Collaborate on projects and minimize conflicting edits

Works on multiple platforms \(MacOS\, Windows\, Linux\)

![](img/Triant-Bobar_Reproducibility_12.png)

---

Website for github, cutadapt repository

# Environment Management



* Conda/Mamba environments
  * Isolated spaces for each project with specific tool versions
  * Manage Python versions and dependencies
  * Install packages and software directly into environment
  * Stable and reproducible place to run code and applications
  * Not limited to Python\, can run bash\, Rscript
  * YAML configuration file to create or export and transfer an environment


![](img/Triant-Bobar_Reproducibility_13.png)

# Storing results



* Public repositories for sequence data \- required for most journals
  * NCBI: https://www\.ncbi\.nlm\.nih\.gov
  * Ensembl: https://www\.ensembl\.org/index\.html
  * Always document and archive changes\, especially if unpublished:
  * \- genome assembly versions
  * \- sequence data: SNPs\, isoforms


![](img/Triant-Bobar_Reproducibility_14.png)

---

Websites: NCBI, Ensembl, Santa Cruz

# Containers

Portable environments that run across different computing environments

Contain packages\, software and dependencies that remain isolated from host infrastructure

Standalone unit of software and can produce same results on different machine or server

<span style="color:#002060"> __Ruoshi__ </span>  <span style="color:#002060"> __ Sun \- Research Computing Workshop Series__ </span>

1\. Using Containers on HPC \- Monday\, March 30\, 2026 \- 9:00AM

2\. Building Containers on HPC \- Monday April 6\, 2026 \- 9:00AM

![](img/Triant-Bobar_Reproducibility_15.png)

# Bioinformatic Pipelines



* Typical bioinformatics workflows involve many steps:
* FASTQ → QC → Alignment → Sorting → Variant Calling → Annotation
  * \- FASTQ files need quality check and trimming
  * Cutadapt
  * BWA
  * Samtools
  * Freebayes
  * VCFtools
* Create pipeline to string software together for “final” output


![](img/Triant-Bobar_Reproducibility_16.png)

# Bioinformatic Pipeline challenges

Complex dependencies between steps

Formatting inconsistencies

Hard to reproduce results \- scalability\, parameters\, version changes

Difficult to parallelize efficiently

Manual scripts often fail on HPC

![](img/Triant-Bobar_Reproducibility_17.png)

# Bioinformatic Pipelines on HPC

Which modules were loaded?

Where are scripts being run

Tracking paths \- hard\-coded in scripts?

Out/error files \- software vs slurm conflicts

<span style="color:#002060"> __Goal:__ </span>  <span style="color:#002060"> </span> Automate and track these workflows

![](img/Triant-Bobar_Reproducibility_18.png)

![](img/Triant-Bobar_Reproducibility_19.png)

# Snakemake

https://snakemake\.github\.io/

__Snakemake__  is a workflow management system designed for scientific pipelines

Created by Johannes Köster\, first released in 2012

Based on UNIX make \-  originally created in 1976 but still standard use

Python based \- “ _snake\-make_ ”

Free and open source\, available on Mac\, Windows\, Unix

https://snakemake\.readthedocs\.io/en/stable/

https://github\.com/snakemake

---

Make is a command-line interface software tool that performs actions ordered by configured dependencies as defined in a configuration file called a makefile. It is commonly used for build automation to build executable code from source code. 

# Snakemake format

Similar to writing shell scripts but snake files contains sets of rules

Format is based on Python structure

Snakemake reads from snakefile that defines the rules

Snakefile rules have a target output

Snakemake uses pattern matching to follow the inputs\, outputs and commands contained in rules to reach final target output

![](img/Triant-Bobar_Reproducibility_20.png)

# Snakemake Core Idea

Instead of defining  _steps_ \, you define  __rules that produce files__ \.

rule align:

input:

"reads\.fastq"

output:

"aligned\.bam"

shell:

"bwa mem ref\.fa \{input\} > \{output\}"

Snakemake builds a  __directed acyclic graph \(DAG\)__  automatically\.

Fastq → Cutadapt → BWA → Sorted BAM → Freebayes → VCF

![](img/Triant-Bobar_Reproducibility_21.png)

# Recommended Pipeline Directory Structure

Benefits:

separates  __workflow logic from data__

easier debugging

easier collaboration

Common practice:

config/ → parameters and sample tables

envs/ → reproducible environments

rules/ → modular workflow steps

results/ → generated outputs

Example:

bioinformatics\_pipeline/├── Snakefile├── config/│   └── config\.yml├── envs/│   └── bwa\.yml├── rules/│   ├── alignment\.smk│   ├── qc\.smk│   └── variant\_calling\.smk├── scripts/│   └── custom\_processing\.py├── data/│   └── raw/├── results/│   ├── bam/│   ├── qc/│   └── variants/└── logs/

A clean directory structure makes pipelines easier to maintain and reproduce\.

---

.yml file can indicate how to make conda environment and what packages and dependencies you need

# Snakefile breakdown

Fastq files that need trimming \- input:  sample\.fastq

Cutadapt \- output: sample\-trimmed\.fastq

BWA \- align trimmed fastq to assembly output: sample\-aligned\.sam

Samtools sorting\, indexing \- output: sample\-sorted\.bam

Freebayes variant calling \- output: sample\-variants\.vcf

# Example snakefile

__rule__  all:    input:        "variants/sample1\.vcf”

__rule__  trim:

input:

”reads/sample1\.fastq”

output:

”trimmed\_reads/sample1\-trimmed\.fastq”

shell:

cutadapt \-A TCCGGGTS \-o \{output\} \{input\}

__rule__  align:    input:        "trimmed\_reads/sample1\-trimmed\.fastq"    output:        "bam/sample1\.bam"    threads: 1    shell:        "bwa mem \-t \{threads\} ref\.fa \{input\} | samtools view \-Sb \- > \{output\}”

__rule__  call\_variants:    input:        "bam/sample1\.bam"    output:        "variants/sample1\.vcf"    shell:        "freebayes \-f ref\.fa \{input\} > \{output\}”

<span style="color:#0070c0">Snakemake</span>  <span style="color:#0070c0"> takes first rule as the target </span>

<span style="color:#0070c0">then constructs graph of dependencies</span>

<span style="color:#0070c0">\{wildcards\} serve as placeholders within rules to operate</span>

<span style="color:#0070c0">on multiple files via pattern matching</span>

---

Snakemake builds the entire pipeline graph automatically.


# Snakemake exercises on HPC

Class data:

<span style="color:#1a1a1a">/project/</span>  <span style="color:#1a1a1a">hpc\_training</span>  <span style="color:#1a1a1a">/reproducibility/</span>  <span style="color:#1a1a1a">snakemake</span>

$ cp  <span style="color:#1a1a1a">/project/</span>  <span style="color:#1a1a1a">hpc\_training</span>  <span style="color:#1a1a1a">/reproducibility/</span>  <span style="color:#1a1a1a">snakemake</span>  <span style="color:#1a1a1a"> \.</span>

<span style="color:#1a1a1a">   </span>  <span style="color:#1a1a1a">\- GCF\_000005845\.2\_ASM584v2\_genomic\.fna \- genome assembly</span>

<span style="color:#1a1a1a">    \- SRR2584863\_1\.fastq \- </span>  <span style="color:#1a1a1a">fastq</span>  <span style="color:#1a1a1a"> sequence file\, paired\-1</span>

<span style="color:#1a1a1a">    \- SRR2584863\_2\.fastq \- </span>  <span style="color:#1a1a1a">fastq</span>  <span style="color:#1a1a1a"> sequence file\, paired\-2</span>

<span style="color:#1a1a1a">    \- \*\.</span>  <span style="color:#1a1a1a">smk</span>  <span style="color:#1a1a1a">  \- </span>  <span style="color:#1a1a1a">snakemake</span>  <span style="color:#1a1a1a"> files</span>

<span style="color:#1a1a1a">    \- </span>  <span style="color:#1a1a1a">config\_variant\.yml</span>  <span style="color:#1a1a1a">  \- configuration file</span>

<span style="color:#1a1a1a">    \- submit\_snakemake\.sh \- sample </span>  <span style="color:#1a1a1a">slurm</span>  <span style="color:#1a1a1a"> file</span>

![](img/Triant-Bobar_Reproducibility_22.png)

---

Yet another markup language- YAML Ain't Markup Language 

# Running jobs on interactive node

Run interactively \- good for testing

$ ijob \-c 1 \-A hpc\_training \-p interactive –v \-t 2:00:00

$ cp  <span style="color:#1a1a1a">/project/</span>  <span style="color:#1a1a1a">hpc\_training</span>  <span style="color:#1a1a1a">/reproducibility/</span>  <span style="color:#1a1a1a">snakemake</span>  <span style="color:#1a1a1a"> \.</span>

![](img/Triant-Bobar_Reproducibility_23.png)

---

Default execution here is local so everything is running in my ijob session on a compute node. If we wanted to have these processes run non-interactively we would want to make sure we are using the executor flag in our snakemake call: "--executor slurm"

Work in scratch

# Modules

$ module spider \<package>

\- specifics and version of package available

$ module spider snakemake

$ module load snakemake/9\.8\.1

$ module list

$ snakemake \-help

![](img/Triant-Bobar_Reproducibility_24.png)

# Other modules needed for today

$ module load bwa/0\.7\.17

$ module load  cutadapt/4\.9

$ module load snakemake/9\.8\.1

$ module load freebayes/1\.3\.10

$ module load  samtools/1\.21

![](img/Triant-Bobar_Reproducibility_25.png)

# Running snakemake - genome alignment

Snakefile \- file\.smk\, contains rules for snakemake

$ snakemake \-c 1 \-s align\.smk

\-\-dry\-run \-np good to test first without producing output

\-n only show steps\, don't run\, \-p print shell commands

\-c number of cores

\-s needed if using a named snakefile \(if just called "snakefile"\,  don't need the –s flag\)

$ snakemake \-\-dag| dot \-Tpng > dag\_align\.png

![](img/Triant-Bobar_Reproducibility_26.png)

# Running snakemake - variant detection

Snakefile \- file\.smk\, contains rules for snakemake

$ snakemake \-c 1 \-s variant\-call\.smk

\-\-dry\-run

\-c number of cores

\-s needed if using a named snakefile \(if just called "snakefile"\, don't need\)

$ snakemake \-\-dag \-s variant\-call\.smk | dot \-Tpng \\ > dag\_variant\.png

![](img/Triant-Bobar_Reproducibility_27.png)

# Snakemake Examples on HPC

Not recommended to hard\-code files within snake file

Can organize sample names\, file paths\, and software parameters in a YAML configuration file

YAML \- serialization language that transforms data into a format that can be shared between systems

With snakemake\, configuration file is a reference for the workflow

![](img/Triant-Bobar_Reproducibility_28.png)

---

Yet another markup language- YAML Ain't Markup Language 
Easy to keep things organized within a single file
While showing, good to have separate config files rather than one huge one and commenting sections out

# Running snakemake with config file

Snakefile \- file\.smk\, contains rules for snakemake

$  <span style="color:#000000">snakemake</span>  <span style="color:#000000"> \-c 1 \-s variant\-</span>  <span style="color:#000000">yml\.smk</span>  <span style="color:#000000"> \-\-</span>  <span style="color:#000000">configfile</span>  <span style="color:#000000"> </span>  <span style="color:#000000">config\_variant\.yml</span>

\-\-configfile – directing snakemake to a config file

\-c number of cores

\-s needed if using a named snakefile

![](img/Triant-Bobar_Reproducibility_29.png)

# Reproducible environments

Snakemake supports reproducible environments\.

Example with Conda:

rule fastqc:    input: "reads\.fastq"    output: "qc\.html"    conda:        ”~/\.conda/envs/fastqc\_env” \#path to conda environment    shell:        "fastqc \{input\}"

Benefits: Easy dependency management\, portable workflows

---

.yml file can indicate how to make conda environment and what packages and dependencies you need

# Using Environments

├── Snakefile├── config/│   └── config\.yml├── envs/│   └── bwa\.yml├── rules/│   ├── alignment\.smk│   ├── qc\.smk│   └── variant\_calling\.smk├── scripts/│   └── custom\_processing\.py├── data/│   └── raw/├── results/│   ├── bam/│   ├── qc/│   └── variants/└── logs/

Can also create a environment\.yml file\, list conda envs and what to install

__name__ : bwa\.yml

__channels:__

\- conda\-forge

\- bioconda

__dependencies__ :

\-bwa= <span style="color:#000000">0\.7\.17</span>

![](img/Triant-Bobar_Reproducibility_30.png)

---

.yml file can indicate how to make conda environment and what packages and dependencies you need

# Snakemake with conda environment

$ module load miniforge

$ conda create

$ conda activate

$ snakemake command

$ screen/tmux

\- keeps session running when disconnected

\- make sure to connect to same login node\,

\- confirm login node with:  hostname

Can create different conda environment for different rules

# Smakemake and containers

Snakemake also supports containers:

rule align:    container:        "docker://biocontainers/bwa"

Advantages:

identical software environments

portable across HPC systems

easier collaboration

<span style="color:#002060"> __Ruoshi__ </span>  <span style="color:#002060"> __ Sun \- Research Computing Workshop Series__ </span>

1\. Using Containers on HPC \- Monday\, March 30\, 2026 \- 9:00AM

2\. Building Containers on HPC \- Monday April 6\, 2026 \- 9:00AM

# Best Practices for HPC

Recommendations:

Use threads and resources properlyAvoid huge single jobsBreak workflows into modular rulesUse conda or containersUse \-\-dry\-run before submitting large workflowsStore configuration in YAML files

![](img/Triant-Bobar_Reproducibility_31.png)

# Common HPC Pitfalls with workflow managers

Examples:

requesting too many cores per rule

forgetting to specify memory

submitting thousands of tiny jobs

running Snakemake or Nextflow themselves on a login node

![](img/Triant-Bobar_Reproducibility_32.png)

# Key Takeaways with workflow managers

Snakemake & Nextflow provide:

reproducible pipelines

automatic dependency tracking

scalable HPC execution

environment management

workflow portability

![](img/Triant-Bobar_Reproducibility_33.png)

# Nextflow

Snakemake & Nextflow provide:

reproducible pipelines

automatic dependency tracking

scalable HPC execution

environment management

workflow portability

![](img/Triant-Bobar_Reproducibility_34.png)

# What is Nextflow?

<span style="color:#000000">Nextflow</span>  <span style="color:#000000"> is a workflow management system that helps automate and organize multi\-step computational pipelines\.</span>

<span style="color:#000000">At a high level\, it connects software steps together\, manages how data moves between them\, and handles execution across local machines\, HPC schedulers like SLURM\, or cloud platforms\.</span>

![](img/Triant-Bobar_Reproducibility_35.png)

# Nextflow Pipelines



* Key concepts:
  * Processes\, workflows\, and parameters
* In general\, we are going to:
  * Create processes to execute desired commands
  * Specify parameters to represent workflow settings
  * Define a workflow to execute processes in a specific order
* Key files:
  * main\.nf and nextflow\.config


![](img/Triant-Bobar_Reproducibility_36.png)

---

Parameters are user-adjustable values that control how a workflow runs. They can specify input files, output locations, software options, reference files, or general pipeline behavior.

# Toy example: print the text "Hello World!"

First\, create a process called HELLO with our shell command:

process HELLO \{

script:

"""

echo "Hello World\!"

"""

\}

Then we execute this process in our workflow:

workflow \{

HELLO\(\)

\}

![](img/Triant-Bobar_Reproducibility_37.png)

---

Let's start with a very simple toy example for echo'ing the text "Hello World!" And then we'll build to our bioinformatics example.

# Create a new file called main.nf

process HELLO \{

script:

"""

echo "Hello World\!"

"""

\}

workflow \{

HELLO\(\)

\}

![](img/Triant-Bobar_Reproducibility_38.png)

---

We can create a new file called main.nf with these lines.

Show and execute main.nf in terminal. Show where the file goes. Went to .command.out file in 'work' directory
 for the specific process

# Let's make some changes

__process__  hello \{  __output__ : path 'hello\.txt' script: """ echo 'Hello world\!' > hello\.txt """\}

![](img/Triant-Bobar_Reproducibility_39.png)

---

We want to send the text to a file called 'hello.txt.' Now we can update our shell command to send the text to a file, and we can add an output in our process to define our file name and since out output is a file, we'll specify the type of output as a path.

Run main.nf in terminal and show it still went to 'work' directory

This was better, but we still have to dig around for the file, so let's add one more thing to our process.

# Add a publishDir for output file destination

__process__  hello \{ publishDir "results/" \, mode: "copy"

__ output__ : path 'hello\.txt' script: """ echo 'Hello world\!' > hello\.txt """\}

![](img/Triant-Bobar_Reproducibility_40.png)

---

Now let's try sending our output to a directory called 'results' - we can add a publishDir to our process and specify the mode "copy" is safest, but you can do other things like move or even create links to the file.
Re-run the main.nf in the terminal and show where the file goes to results but since we did copy, it still does go to work. Point out that we need to be mindful of any extra data we're creating so we don't unnecessarily have duplicates for everything.

# Let's look at our snakemake "trim" rule from earlier

__rule__  trim:

input:

”reads/sample1\.fastq”

output:

”trimmed\_reads/sample1\-trimmed\.fastq”

shell:

cutadapt \-A TCCGGGTS \-o \{output\} \{input\}

![](img/Triant-Bobar_Reproducibility_41.png)

---

Here we specified our inputs/outputs and our shell command.

# What do we need to update in Nextflow?

__process__  HELLO \{    publishDir "results/" \, mode: "copy"

__    __ output:    path 'hello\.txt'    script:    """    echo 'Hello world\!' > hello\.txt    """\}

![](img/Triant-Bobar_Reproducibility_42.png)

---

So, looking at our HELLO process, what do we need to add? We already have a publishDir, an output, and script, so let's update those for cutadapt.

# Update for running cutadapt

__process__  CUTADAPT \{    publishDir "results/" \, mode: "copy"

output:    path 'trimmed\.fastq'    script:    """    cutadapt \-a AACCGGTT \-o trimmed\.fastq ~/sample1\.fastq    """\}

workflow \{

CUTADAPT\(\)

\}

![](img/Triant-Bobar_Reproducibility_43.png)

---

We can keep 'results' as our publishDir for this example, but we'll need to change our output to trimmed.fastq and we'll change the command for cutadapt with our adapter and our input and output file names. Because Nextflow executes each task in its own work directory, we need to provide the full path. Our workflow just becomes running the CUTADAPT process.
Does this work? Yes, it does. However, but we are hard-coding everything and this not really flexible and does not really allow us to scale.

# More common approach for input files

__process__  CUTADAPT \{    publishDir "results/" \, mode: "copy"

<span style="color:#000000">input:</span>  <span style="color:#000000">    path </span>  <span style="color:#000000">reads\_var</span>

output:    path 'trimmed\.fastq'    script:    """    cutadapt \-a AACCGGTT \-o trimmed\.fastq $reads\_var    """\}

workflow \{

CUTADAPT\(Channel\.fromPath\('~/sample1\.fastq'\, checkIfExists: true\)\)

\}

![](img/Triant-Bobar_Reproducibility_44.png)

---

A better approach is to pass the file into the process with Channel.fromPath() and use input: path reads. The "input:" declares an input variable, not a literal source file location. And we use this variable "reads" our shell command and here $reads means: the local process input variable and use the actual input file that was provided to Nextflow for this task via our workflow. We can also use the reads variable to other things like dynamically name files or any


# Dynamically scaling to many samples

__process CUTADAPT \{__  __ __  __publishDir__  __ "results/"\, mode: "copy"__  __ input:__  __ path __  __reads\_var__  __ output:__  __ path "$\{__  __reads\_var\.simpleName__  __\}\___  __trimmed\.fastq__  __"__  __ script:__  __ """__  __ __  __cutadapt__  __ \-a AACCGGTT \-o $\{__  __reads\_var\.simpleName__  __\}\___  __trimmed\.fastq__  __ $__  __reads\_var__  __ """__  __\}__  __workflow \{__  __ CUTADAPT\(__  __Channel\.fromPath__  __\('\*\.__  __fastq__  __'\, __  __checkIfExists__  __: true\)\)__  __\}__

![](img/Triant-Bobar_Reproducibility_45.png)

---

Now we can start to use the flexibility nextflow provides to name our output files dynamically based on sample name and we also can start to scale up by using the wildcard to grab all the fastq files in our example 'reads' directory. Here nextflow is going to create a new separate process for each of our samples.


# Parameter options for input files

Add a parameter for '\-\-reads' in your 'nextflow run' command

Add a params\.reads at the top of your main\.nf file

Add a params\.reads to a nextflow\.config file

Works for one file \('reads/sample1\.fastq'\) or many \('reads/\*\.fastq'\)

![](img/Triant-Bobar_Reproducibility_46.png)

---

As with many things with Nextflow, we have multple different ways we can accomplish this. Will talk about nextflow.config shortly.

# Less hard-coding = more reproducibility

From:workflow \{ CUTADAPT\(Channel\.fromPath\(~/sample1\.fastq'\, checkIfExists: true\)\)\}

To:

workflow \{ CUTADAPT\(Channel\.fromPath\(params\.reads\, checkIfExists: true\)\)\}

![](img/Triant-Bobar_Reproducibility_47.png)

---

And if we use one of those parameter methods, instead of our workflow having a hard-coded path for our inputs, we can dynamically provide our input file names and clean things up in our workflow even further.

# Loading software – main.nf

Use a 'beforeScript' in the CUTADAPT process in main\.nf

beforeScript runs specified shell command\(s\) before running the script command

Load the cutadapt module: beforeScript 'module load cutadapt'

Can also do other things like export variables or create directories

beforeScript """        module purge        module load cutadapt        mkdir results        export PATH="$PATH:/opt/tools"'    """

![](img/Triant-Bobar_Reproducibility_48.png)

---

We can definitely load the software in our process, but we just cleaned that thing up, so let's put it somewhere better to keep our main.nf focused on workflow logic. To do this, let's go ahead and start to build a nextflow.config file.

# Loading software – nextflow.config

Again\, we use a 'beforeScript' specific to the CUTADAPT process

Process \{withName: CUTADAPT \{    beforeScript = '''    module purge    module load cutadapt

'''

![](img/Triant-Bobar_Reproducibility_49.png)

---

Now when we specifically run our CUTADAPT process, these commands will run before our script command and set up our process environment.  Ok, so now we have our software dialed in for cutadapt. But we need to think about where we are running these processes. By default, nextflow is running shell commands locally, so that means if we're just at the command line, we'd be running the processes on the login nodes, which is a no-no.

# Adding SLURM options – nextflow.config

Process \{    withName: CUTADAPT \{        beforeScript = '''        module purge        module load cutadapt

'''

executor = 'slurm'    queue = 'standard'    cpus = 2    mem = '16 GB'    time = '1h'    clusterOptions = '\-\-account=hpc\_build'

\}

![](img/Triant-Bobar_Reproducibility_50.png)

---

So, we need to let Nextflow know that we want to use SLURM to execute our processes and with do this by specifying SLURM as our executor. We can also use this to specify various other options – nextflow doesn't have explicit options for all possible slurm commands, so we can supplement with any additional options we need with 'clusterOptions.' Again, there's multiple ways to configure everything – you can also do global slurm options, but often different parts of the workflow are going to need different resources. And we could potentially specify these slurm options in the CUTADAPT process in our main.nf, but we're trying to keep that tidy and focused on the workflow logic.


# Now we have:

Workflow logic in main\.nf

Software and slurm options in nextflow\.config

![](img/Triant-Bobar_Reproducibility_51.png)

---

As you can imagine, there's also multiple ways to set 

# Extend to CUTADAPT → BWA_ALIGN → FREEBAYES



* Same rules apply – largely rinse and repeat for additional processes
* Create processes for each step: inputs/outputs\, commands\, etc\.
* Software and slurm options in nextflow\.config
* Main difference is our workflow \- more processes and channels
  * Send channel into process
  * Process produces output
  * Output becomes new channel for next process\.


![](img/Triant-Bobar_Reproducibility_52.png)

---

With Nextflow, channels carry data and processes do work on that data. You link them together by sending a channel into a process, and if that process produces output, its output can become a new channel for the next process.

# Workflow for CUTADAPT → BWA_ALIGN → FREEBAYES

workflow \{

reads\_ch = Channel\.fromPath\("$\{params\.reads\_dir\}/\*\.fastq"\, checkIfExists:  true\)

trimmed\_ch = CUTADAPT\(reads\_ch\)

aligned\_ch = BWA\_ALIGN\(trimmed\_ch\)

FREEBAYES\(aligned\_ch\)

\}

![](img/Triant-Bobar_Reproducibility_53.png)

---

Here's how we could link the trim, align and variant calling together. So now we'll put it all together and run the entire workflow from end to end on the system.

# Additional links

[https](https://nf-co.re/rnaseq/3.23.0/)[://nf\-co\.re/rnaseq/3\.23\.0/](https://nf-co.re/rnaseq/3.23.0/)

[https://training\.nextflow\.io](https://training.nextflow.io)

https://github\.com/nextflow\-io/nextflow

![](img/Triant-Bobar_Reproducibility_54.png)

# Workflows for computational data analysis



* https://github\.com/common\-workflow\-language/common\-workflow\-language/wiki/Existing\-Workflow\-systems
* https://github\.com/pditommaso/awesome\-pipeline
* Galaxy platform \- bioinformatic software\, pipeline and workflows:
  * https://usegalaxy\.org


![](img/Triant-Bobar_Reproducibility_55.png)

