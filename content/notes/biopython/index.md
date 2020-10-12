---
title: Biopython
highlight_style: "github"
date: "2020-10-01T00:00:00"
toc: true  
type: article
draft: false
---

# Introduction

From the [official Biopython project website](https://biopython.org):

> Biopython is a set of freely available tools for biological computation written in Python by an international team of developers.<br>It is a distributed collaborative effort to develop Python libraries and applications which address the needs of current and future work in bioinformatics. The source code is made available under the Biopython License, which is extremely liberal and compatible with almost every license in the world.

This workshop assumes a working knowledge of the Python programming language and basic understanding of the concepts of online DNA and Protein sequence repositories.

Introductions to Python can be found [here](/courses/programming_python_scientists_engineers/python-interpreter/) and [here](http://localhost:1313/courses/python_introduction/).

---

# Getting Started

**Python code examples**

The Python scripts and data files for this workshop can be [downloaded from here](data/biopython-workshop.zip). On your computer, unzip the downloaded folder and use it as working directory for this workshop.

**Python programming environment**

The Anaconda environment from [Anaconda Inc.](https://anaconda.com/) is widely used because it bundles a Python interpreter, most of the popular packages, and development environments. It is cross platform and freely available. There are two somewhat incompatible versions of Python; version 2.7 is deprecated but still fairly widely used. Version 3 is the supported version. 

**Note: The latest Biopython package version (1.77+) requires Python 3.**

1. Visit the [Anaconda download website](https://www.anaconda.com/products/individual#Downloads) and download the installer for Python 3 for your operating system (Windows, Mac OSX, or Linux). We recommend to use the graphical installer for ease of use.

2. Launch the downloaded installer, follow the onscreen prompts and install the Anaconda distribution on your local hard drive.

The [Anaconda Documentation](https://docs.anaconda.com/anaconda/user-guide/getting-started/) provides an introduction to the Ananconda environment and bundled applications. For the purpose of this workshop we focus on the `Anaconda Navigator` and `Spyder`. 

## Navigator

Once you have installed Anaconda, start the Navigator application: 
* [Instructions for Windows](https://docs.anaconda.com/anaconda/user-guide/getting-started/#open-nav-win)
* [Instructions for Mac](https://docs.anaconda.com/anaconda/user-guide/getting-started/#open-nav-mac)
* [Instructions for Linux](https://docs.anaconda.com/anaconda/user-guide/getting-started/#open-nav-lin)

You should see a workspace similar to the screenshot, with several options for working environments, some of which are not installed. We will use `Spyder` which should already be installed. If not, click the button to install the package.

![AnacondaNavigator](/notes/biopython/anaconda-navigator.png)

## Spyder

Now we will switch to Spyder. Spyder is an Integrated Development Environment, or IDE, aimed at Python. It is well suited for developing longer, more modular programs. 

1. To start it, return to the `Anaconda Navigator` and click on the `Spyder` tile. It may take a while to open (watch the lower left of the Navigator). 
2. Once it starts, you will see a layout with an editor pane on the left, an explorer pane at the top right, and an iPython console on the lower right. This arrangement can be customized but we will use the default for our examples. Type code into the editor. The explorer window can show files, variable values, and other useful information. The iPython console is a frontend to the Python interpreter itself. It is comparable to a cell in JupyterLab.

![AnacondaNavigator](/notes/biopython/anaconda-spyder.png)

## Installation of the Biopython package

It is recommended to install the `biopython` package from PyPI using the `pip install` command. Detailed instructions are available [here](https://biopython.org/wiki/Download).

**On your own computer:**
Start the `Anaconda Prompt` command line tool following the instructions for your operating system.
* Start Anaconda Prompt on [Windows](https://docs.anaconda.com/anaconda/user-guide/getting-started/#open-prompt-win)
* Start Anaconda Prompt on [Mac](https://docs.anaconda.com/anaconda/user-guide/getting-started/#open-prompt-mac), or open a terminal window.
* [Linux:](https://docs.anaconda.com/anaconda/user-guide/getting-started/#open-prompt-lin) Just open a terminal window.

At the prompt, type the following command and press enter/return:
```bash
pip install biopython
```
This command will install the latest biopython package version in your current Anaconda Python environment.

**On Rivanna (UVA's HPC platform):**

[Rivanna](https://www.rc.virginia.edu/userinfo/rivanna/overview/) offers severalAnaconda distributions with different Python versions. Before you use Python you need to load one of the `anaconda` software modules and then run the `pip install` command. 

```bash
module load anaconda
pip install --user biopython
```
**Note:** You have to use the `--user` flag which instructs the interpreter to install the package in your home directory. Alternativley, create your own custom [Conda environment](https://docs.conda.io/projects/conda/en/latest/user-guide/tasks/manage-environments.html) first an run the `pip install biopython` command in that environment (without `--user` flag) 

## Testing the Biopython Installation

Start the Spyder IDE (see [here](#spyder)).  In the `IPython console` pane, type the following command and press `enter/return`:

```
import Bio
print (Bio.__version__)
```

If the package is installed correctly, the output will show the biopython version number.

---

# Bio Subpackages and Classes

The `Bio` package provides a large number of subpackages providing specific functionality. The Biopyhton website provides a [full list of all subpackages](https://biopython.org/docs/1.78/api/Bio.html#subpackages). 

The following table shows an excerpt of that list relevant for this workshop.

| Subpackages/Classes | Purpose |
| - | - | 
| [Bio.Entrez](https://biopython.org/docs/1.75/api/Bio.Entrez.html) | Functions to retrieve Entrez records and associated data |
| [Bio.ExPASy](https://biopython.org/docs/1.78/api/Bio.ExPASy.html) | Tools to access data hosted on the ExPASy protein databases |
| [Bio.SwissProt](https://biopython.org/docs/1.78/api/Bio.SwissProt.html) | Tools to work with the sprotXX.dat file from SwissProt |
| [Bio.Seq](https://biopython.org/docs/1.78/api/Bio.Seq.html?highlight=seq#module-Bio.Seq) | Sequence datastructure (immutable=read-only) |
| [Bio.MutableSeq](https://biopython.org/docs/1.78/api/Bio.Seq.html?highlight=mutableseq#Bio.Seq.MutableSeq) | Sequence datastructure (mutable=modifiable |
| [Bio.SeqRecord](https://biopython.org/docs/1.78/api/Bio.SeqRecord.html?highlight=seqrecord#module-Bio.SeqRecord) | Datastucture for Seq object plus enriched information |
| [Bio.SeqIO](https://biopython.org/docs/1.78/api/Bio.SeqIO.html?highlight=seqio#module-Bio.SeqIO) | Read/write sequences (various file formats )| 
| [Bio.AlignIO](https://biopython.org/docs/1.78/api/Bio.AlignIO.html) | A new multiple sequence Alignment Input/Output interface for BioPython 1.46 and later |
| [Bio.Align.MultipleSeqAlignment](https://biopython.org/docs/1.78/api/Bio.Align.html?highlight=mult#Bio.Align.MultipleSeqAlignment) | Tools for Code for working with sequence alignments |

---

# Online Datasets and Databases

The `Bio` module provides several classes to process output and datasets provided by the following web services and tools:
* FASTA
* Blast output – both from standalone and WWW Blast
* Clustalw
* GenBank
* PubMed and Medline
* ExPASy files, like Enzyme and Prosite
* SCOP, including ‘dom’ and ‘lin’ files
* UniGene
* SwissProt

In this workshop we will explore options to download nucleotide and protein sequences from  [Entrez](https://www.ncbi.nlm.nih.gov/Web/Search/entrezfs.html) and [ExPASy](https://www.expasy.org).

## Accessing NCBI's Entrez Databases

The `Bio.Entrez` submodule provides access to the Entrez databases. When you use this module you need to know the String descriptor of the database you want to query (aka its name).  A list of valid database names is provided in  [column three of this table](https://www.ncbi.nlm.nih.gov/books/NBK25497/table/chapter2.T._entrez_unique_identifiers_ui/?report=objectonly).

**Note:** Please review the [General Introduction to the E-utilities](https://www.ncbi.nlm.nih.gov/books/NBK25497/) for accessing the Entrez Application Programming Interface Programm. The E-utilities limit the frequency of API calls and your IP address may be blocked if you contineously exceed the limit.

**Basic Steps:**

1. Provide an email address. This is requird! `Entrez.email = "your@somewhere"`.
2. Use an [E-utility](https://www.ncbi.nlm.nih.gov/books/NBK25497/) to get a __handle__ for the data of interest, e.g. `handle = Entrez.esearch(...)`.
3. Use __handle__ to read or parse data with `handle.read()` or `handle.parse()`.
4. Close the __handle__ with `handle.close()`.

Also read ["What the heck is a handle?"](http://biopython.org/DIST/docs/tutorial/Tutorial.html#sec416)

**Find Database Records:**

Let's find the **protein** records associated with the **human Pax6 gene** and download the associated sequences in [FASTA](https://en.wikipedia.org/wiki/FASTA_format) format.

To search the database we use the `Entrez.esearch` function.  We need to specify the database via the `db`, argument and specify a search `term` (provided as a list of Strings). 
```python
from Bio import Entrez

Entrez.email = "YOU@SOMEWHERE.com" # your email address is required
handle = Entrez.esearch(db="protein", term = ["Homo sapiens[Orgn] AND pax6[Gene]"], usehistory="y")
record = Entrez.read(handle)
handle.close()
# iterate over items
for k,v in record.items():
    print (k,v)
```

The search results is returned as a [dictionary](https://docs.python.org/3/tutorial/datastructures.html#dictionaries) and we can retrieve the list of unique IDs that match our query via `record["IdList"]`. 

**Note:** The `IdList` returned by `esearch` is limited to the top 20 hits by default. There are two workarounds:
1. Use the `retmax=<number>` keyword argument to increase the maximum number of retrieved records. The problem is you need to know what a reasonable number is. 
2. Or better, use the `usehistory='y'` keyword argument. This will save the search results on the remote server and provide `WebEnv` and `QueryKey` entries that can be used with the `eftech` function (see next section) to retrieve all search records (beyond the top 20).

By default the returned IDs reflect the __GI__ numbers. The __accession.version__ numbers can be retrieved instead by passing `idtype='acc'` as an optional keyword argument to the `esearch` function. See the [detailed documentation of the esearch function](https://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.ESearch) here.

**Download and save sequences as FASTA file:**

With the ID list in hand, we can now download the sequence records using just a few lines of code and save them in a single multi-sequence FASTA file. The `efetch` function is used when you want to retrieve a full record from Entrez. 
```python
# fetch records using id list
handle = Entrez.efetch(db="protein", rettype="fasta", retmode="text", id=record["IdList"])
result = handle.read()  # return tryp is simple string
handle.close()
# remove empty lines
fastaseq = result.replace("\n\n","\n")
with open('HsPax6-protein.fasta', 'w') as f:
   f.write(fastaseq)
```

Alternatively, we can pull individual sequences one at a time and save each sequence into a separate file. To do this we implement a [for loop](https://docs.python.org/3/tutorial/controlflow.html?highlight=loops#for-statements) that iterates over this list and use the `Entrez.efetch` function to retrieve the FASTA sequence record associated with each id.  We wrap this for loop in a [open](https://docs.python.org/3/library/functions.html?highlight=open#open) file operation block to save the retrieved FASTA records into a single .fasta text file.

Let's retrieve the **nucleotide** sequences of our previous top 5 ID hits as **Genbank** files. We specify the database with the `db="nucleotide""` and format with the `rettype="gb"` keyword arguments.
```python
from Bio import Entrez

Entrez.email = "YOU@SOMEWHERE.com" # your email address is required
handle = Entrez.esearch(db="nucleotide", term = ["Homo sapiens[Orgn] AND pax6[Gene]"], retmax=5, usehistory="y")
record = Entrez.read(handle)
handle.close()
for k,v in record.items():
    print (k,v)

# iterate over ids in list
for seq_id in record["IdList"]:
    # get entry from Entrez
    print (seq_id)
    handle = Entrez.efetch(db="nucleotide", id=seq_id, rettype="gb", retmode="text")
    result = handle.read()
    handle.close()
    # save
    filename = f"HsPax6-{seq_id}-nucleotide.gb"
    print ("Saving to file:", filename)
    with open(filename, 'w') as fastafile:
        # append fasta entry
        fastafile.write(result.rstrip()+"\n")
```

Note that the `record['IdList']` may not represent all the records. Rememeber that the  `record['WebEnv']` and `record['QueryKey']` entries provide access to the search history on the remote server.  So we can use these instead of the `record['IdList']` to get all records.
```
# Alternative: fetch all records using search history
handle = Entrez.efetch(db="protein", rettype="fasta", retmode="text", webenv=record["WebEnv"], query_key=record["QueryKey"])
```

**Exercise:**
Find and download the nucelotide sequences for the mouse P53 tumor suppressor. **Hint:** look up the database descriptor in [this table](https://www.ncbi.nlm.nih.gov/books/NBK25497/table/chapter2.T._entrez_unique_identifiers_ui/?report=objectonly).

<details>
<summary>Solution:</summary>
</details>

## Retrieve Protein Records from the ExPASy Database

**Swiss-Prot**
```
from Bio import ExPASy
from Bio import SwissProt

# get single protein record
accession_no = "O23729"
handle = ExPASy.get_sprot_raw(accession_no)
record = SwissProt.read(handle)
``` 

**Prosite**
```
from Bio import ExPASy
from Bio import Prosite

handle = ExPASy.get_prosite_raw("PS00001")
record = Prosite.read(handle)
```

**Prosite Documentation**
```
from Bio import ExPASy
from Bio.ExPASy import Prodoc
handle = ExPASy.get_prosite_raw("PDOC00001")
record = Prodoc.read(handle)
```

**Exercise:** 

Retrieve the SwissProt records for proteins with the following IDs: "O23729", "O23730", "O23731". Try to use list comprehension to create a list containing the records for all retrieved proteins.
<details>
<summary>Solution</summary>

```
accession_nos = ["O23729", "O23730", "O23731"]
handles  = [ExPASy.get_sprot_raw(a) for a in accession_nos]
records = [SwissProt.read(handle) for h in handles]
```
</details>
<br>

http://biopython.org/DIST/docs/tutorial/Tutorial.html#sec%3Aexpasy_swissprot


**ScanProsite**

We can query the Prosite database with a sequence string to find proteins with corresponding matches.
 
```
from Bio.ExPASy import ScanProsite

sequence = "MEHKEVVLLLLLFLKSGQGEPLDDYVNTQGASLFSVTKKQLGAGSIEECAAKCEEDEEFTCRAFQYHSKEQQCVIMAENRKSSIIIRMRDVVLFEKKVYLSECKTGNGKNYRGTMSKTKN"
handle = ScanProsite.scan(seq=sequence)
result = ScanProsite.read(handle)
```
By executing `handle.read()`, you can obtain the search results in raw XML format. Here we use `Bio.ExPASy.ScanProsite.read` to parse the raw XML into a `Bio.ExPASy.ScanProsite.Record` object which represents a specialized list.

We can now access the found matches like this:
```
print ("Number of matches:", result.n_match)
for (r in result):
    print (r)
```

Output:
```
Number of matches: 6
{'signature_ac': u'PS50948', 'level': u'0', 'stop': 98, 'sequence_ac': u'USERSEQ1', 'start': 16, 'score': u'8.873'}
{'start': 37, 'stop': 39, 'sequence_ac': u'USERSEQ1', 'signature_ac': u'PS00005'}
{'start': 45, 'stop': 48, 'sequence_ac': u'USERSEQ1', 'signature_ac': u'PS00006'}
{'start': 60, 'stop': 62, 'sequence_ac': u'USERSEQ1', 'signature_ac': u'PS00005'}
{'start': 80, 'stop': 83, 'sequence_ac': u'USERSEQ1', 'signature_ac': u'PS00004'}
{'start': 106, 'stop': 111, 'sequence_ac': u'USERSEQ1', 'signature_ac': u'PS00008'}
```

You see that each `result` item `r` represents a dictionary describing a specific match.

---

# Working with Sequence Files

**Bioinformatics File Formats**

## Sequence Objects

The `Seq` object is similar to a string object augmented with methods for nucleotide sequence operations including 
* `find()`, `count()`
* `complement()`, `reverse_complement()`
* `transcribe()`, `back_transcribe()``
* `translate()`

```python
my_dna = Seq("ATGAGTACACTATAGA")
print (my_dna)
# find position of first subsequence
print (my_dna.find("TAC"))
print (my_dna.find("AC"))
print (my_dna.find("CTC"))

# count
print ("A count:", my_dna.count("A"))
print ("AC count:", my_dna.count("AC"))
print ("AA count:", Seq("AAAA").count("AA")) # non-overlapping
```

**Output:**
```
ATGAGTACACTATAGA
5
6
-1
A count: 7
AC count: 2
AA count: 2
```
Note the return of `-1` if no sequence match was found.

```python
# complement and reverse complement
compl = my_dna.complement()
rev_compl = my_dna.reverse_complement()
print ("original: \t", my_dna)
print ("complement:\t", compl)
print ("rev complement:\t", rev_compl)

# transcription
my_rna = my_dna.transcribe()
print ("RNA:", my_rna)

# translation
my_peptide = my_dna.translate()
print ("Peptide:", my_peptide)
```

**Output:**
```
original:        ATGAGTACACTATAGA
complement:      TACTCATGTGATATCT
rev complement:  TCTATAGTGTACTCAT
RNA: AUGAGUACACUAUAGA
Peptide: MSTL*
```

Like Strings, Seq objects are immutable; this means that the sequence is read-only and cannot be modified in place. However, you can convert a `Seq` object into a `MutableSeq` object that allows you to manipulate he sequence after object initialization.

```python
my_dna[2] = 'A' # error, immutable Seq object
mutable_dna = my_dna.tomutable()
mutable_dna[2] = 'A'
print (my_dna)
print (mutable_dna)
```

**Output:**
```
ATGAGTACACTATAGA
ATAAGTACACTATAGA
```

Note that the sequence is zero-indexed: the first nucleotide has index 0, the second has index 1, and so forth. So in this example we're changing the third nucleotide (index 2, G->A).  

**Exercise:**

Create a `Seq` object with a DNA nucleotide sequence of your choice. Find the first putative start codon (ATG), replace all "C"s with a "G", and transcribe and translate the original as well as the modified sequence.

<details>
<summary>Solution:</summary>
</details> 

## Handling Sequence Records

The [SeqRecord](https://biopython.org/wiki/SeqRecord) class provides the following fields:
* `.seq`: a sequence (as a [Seq](#seq) object)
* `.id`: the identifier, e.g. an accession number (String)
* `.name`: can be just the accession number or the locus name (String)
* `.description`: self-explanatory (String)
* `.annotations`: dictionary of additional often unstructured info (optional)
* `.letter_annotations`: often used for quality scores or secondary structure info
* `.features`: list of [SeqFeature]() objects; more structured than annotations, e.g. gene position in a genome, or domain position in a protein
* `.dbxref`: list of database cross-references


So it is used to wrap around a [Seq](#seq) object with richer information. We can manually create a `SeqRecord` object like this:
```python
from Bio.Seq import Seq
from Bio.SeqRecord import SeqRecord

record = SeqRecord(
    Seq("MKQHKAMIVALIVICITAVVAALVTRKDLCEVHIRTGQTEVAVF"),
    id="YP_025292.1",
    name="HokC",
    description="toxic membrane protein, small",
)
print(record)
```

**Output:**
```
ID: YP_025292.1
Name: HokC
Description: toxic membrane protein, small
Number of features: 0
Seq('MKQHKAMIVALIVICITAVVAALVTRKDLCEVHIRTGQTEVAVF')
```

## Sequence File Operations

The [Bio.SeqIO](https://biopython.org/wiki/SeqIO) class provides simple tools to read and write a variety of sequence file formats (including multiple sequence alignments). It operates exclusively on [SeqRecord](#seqrecord) objects. 

**Read Fasta File**
```python
from Bio import SeqIO

file = open('HsPax6-protein.fasta') 
fastarecords = SeqIO.parse(file, "fasta")
# create a list of SeqRecords
fastalist = [entry for entry in fastarecords]
# iterate over fasta entries
for entry in fastalist:
    print (f"ID={entry.id}")
    print (f"Name={entry.name}")
    print (f"Description={entry.description}")
    print (f"Seq length={len(entry.seq)}")
    print (f"Features={entry.features}")  # empty for Fasta format
    print (f"Sequence={entry.seq}\n")
```

**Exercice:**

<details>
<summary>Solution</summary>

```python
# filter list of records
sublist = [e for e in fastalist if len(e.seq) < 300]
print (f"Total number of sequences: {len(fastalist)}")
print (f"Number of sequences (<300 aa): {len(sublist)}")
```
</details>
<br>

**Convert Genbank to Fasta File**

```python
from Bio import SeqIO

gb_file = 'Hs-pax6-1844139629-nucleotide.gb'
with open(gb_file) as f:
    gb_generator = SeqIO.parse(f, format='gb')
    for entry in gb_generator:
        with open(f'Hs-pax6-{entry.id}-nucleotide.fasta', 'w') as fileout:
            SeqIO.write(entry, fileout, 'fasta')
```

The later versions of Biopython also include a `Bio.SeqIO.convert()` function.
```
# convert GenBank to Fasta
count = Bio.SeqIO.convert("my_file.gb", "genbank", "my_file.fasta", "fasta")
```

## AlignIO: Reading Sequence Alignment Files

The [Bio.AlignIO](https://biopython.org/wiki/AlignIO) class provides functions to handle paired or multiple sequence alignment files. It does not perform the alignemnt but provides tools to read/write alignment files and manipulate alignment objects. `Bio.AlignIO` uses the same set of functions for input and output as in `Bio.SeqIO`, and the same names for the file formats supported.

The key functions are:
* `Bio.AlignIO.read()`: For a file that contains one and only one alignment. The return type is a `Bio.Align.MultipleSeqAlignment` object.
* `Bio.AlignIO.parse()`: A more general function when the file may contain multiple separate alignments. The return type is a generator that can be converted into a list of `Bio.Align.MultipleSeqAlignment` objects. 

**Example:** 

Let's create a Fasta file with Pax6 orthologs from human, mouse, xenopus, pufferfish, zebrafish (2), and fruitfly.

```python
from Bio import Entrez

# get human, mouse, xenopus, pufferfish, zebrafish 1, zebrafish 2, Drosophila 1, Drosophila 2
ids = ['1587062735','AAH36957.1','NP_001006763.1','XP_029701655.1','NP_571379.1','NP_571716.1','NP_524628','NP_524638']
handle = Entrez.efetch(db="protein", rettype="fasta", retmode="text", id=ids)
result = handle.read()
handle.close()
fastaseq = result.replace("\n\n","\n")
with open('Pax6-multispec-protein.fasta', 'w') as f:
   f.write(fastaseq)
```

This will create the `Pax6-multispec-protein.fasta` Fasta file with 8 sequences.  The alignment was performed using [Clustal Omega](https://www.ebi.ac.uk/Tools/msa/clustalo/) and you can [download the Pax6-multispec-protein.aln](data/Pax6-multispec-protein.aln) alignment file and move it to your Python script folder that you use for this workshop.


**Alternatively, create the alignment yourself:**
1. Visit the [Clustal Omega website](https://www.ebi.ac.uk/Tools/msa/clustalo/) and upload the `Pax6-multispec-protein.fasta` file as input. 
2. Under **Step 1**, click the **Choose File** button and upload the `Pax6-multispec-protein.fasta` file as input.
3. Under Step 3, click **Submit**.
4. When the alignment is done, click the **Alignments**, select the entire alignment output in the window and paste it into a text editor. **Do not Microsoft Word for this, but programs like `Text Edit`, `Notepad++`, `Emacs` or `vim`.**
5. Save the alignment in the text editor as `Pax6-multispec-protein.aln` in your Python script folder that you use for this workshop.

**Parse the alignment file**
```
from Bio import AlignIO

inputfile = open("Pax6-multispec-protein.aln", "r")
# assuming single alignment in file; use AlignIO.parse for multiple alignments 
alignment = AlignIO.read(inputfile, "clustal")
inputfile.close()
print ("Alignment length:", alignment.get_alignment_length())
print (alignment,"\n")
```

**Output:**
```bash
Alignment with 8 rows and 867 columns
MFTLQPTPTAIGTVVPPWSAGTLIERLPSLEDMAHKGHSGVNQL...PWV NP_524628.2
---MMLTTEHIMHGHPH-----SSVGQSTLFGCSTAGHSGINQL...--- NP_524638.3
---------------------------------MQNSHSGVNQL...--- NP_001006763.1
--------------------------------------------...--- NP_001355831.1
---------------------------------MQNSHSGVNQL...--- AAH36957.1
--------------------------------MMQNSHSGVNQL...--- XP_029701655.1
----MPQKEY-Y----N-----RATWESGVASMMQNSHSGVNQL...--- NP_571379.1
----MPQKEY-H----N-----QPTWESGVASMMQNSHSGVNQL...--- NP_571716.1
```    

**Update identifier**
```python
species = ['H.sapiens', 'M.musculus', 'X.tropicalis', 'T.rubripes', 'D.rerio', 'D.rerio', 'D.melanogaster', 'D.melanogaster']
for idx,line in enumerate(alignment):
    line.id = f"{species[idx]}:{line.id}"
print (alignment)
```

**Output:**
```bash
Alignment with 8 rows and 867 columns
MFTLQPTPTAIGTVVPPWSAGTLIERLPSLEDMAHKGHSGVNQL...PWV H. sapiens: NP_524628.2
---MMLTTEHIMHGHPH-----SSVGQSTLFGCSTAGHSGINQL...--- M. musculus: NP_524638.3
---------------------------------MQNSHSGVNQL...--- X. tropicalis: NP_001006763.1
--------------------------------------------...--- T. rubripes: NP_001355831.1
---------------------------------MQNSHSGVNQL...--- D. rerio: AAH36957.1
--------------------------------MMQNSHSGVNQL...--- D. rerio: XP_029701655.1
----MPQKEY-Y----N-----RATWESGVASMMQNSHSGVNQL...--- D. melanogaster: NP_571379.1
----MPQKEY-H----N-----QPTWESGVASMMQNSHSGVNQL...--- D. melanogaster: NP_571716.1
```

**Slicing and joining**
```python
# slice: first axis defines line, second axis defines column index (zero-indexed)
# get lines 1-6, first 50 columns
subset = alignment[:6,:50]
print (subset)
```

**Output:**
```bash
Alignment with 6 rows and 50 columns
MFTLQPTPTAIGTVVPPWSAGTLIERLPSLEDMAHKGHSGVNQLGGVFVG H. sapiens: NP_524628.2
---MMLTTEHIMHGHPH-----SSVGQSTLFGCSTAGHSGINQLGGVYVN M. musculus: NP_524638.3
---------------------------------MQNSHSGVNQLGGVFVN X. tropicalis: NP_001006763.1
-------------------------------------------------- T. rubripes: NP_001355831.1
---------------------------------MQNSHSGVNQLGGVFVN D. rerio: AAH36957.1
--------------------------------MMQNSHSGVNQLGGVFVN D. rerio: XP_029701655.1
```

Let's join two alignment blocks:
```python
edited = alignment[:,:50] + alignment[:,500:]
print (edited)
```

**Output:**
```bash
Alignment with 8 rows and 417 columns
MFTLQPTPTAIGTVVPPWSAGTLIERLPSLEDMAHKGHSGVNQL...PWV H. sapiens: NP_524628.2
---MMLTTEHIMHGHPH-----SSVGQSTLFGCSTAGHSGINQL...--- M. musculus: NP_524638.3
---------------------------------MQNSHSGVNQL...--- X. tropicalis: NP_001006763.1
--------------------------------------------...--- T. rubripes: NP_001355831.1
---------------------------------MQNSHSGVNQL...--- D. rerio: AAH36957.1
--------------------------------MMQNSHSGVNQL...--- D. rerio: XP_029701655.1
----MPQKEY-Y----N-----RATWESGVASMMQNSHSGVNQL...--- D. melanogaster: NP_571379.1
----MPQKEY-H----N-----QPTWESGVASMMQNSHSGVNQL...--- D. melanogaster: NP_571716.1
```

**Exporting to other alignment file formats**
```python
# save as Stockholm
with open("Pax6-multispec-protein.sth", "w") as outputfile:
    AlignIO.write(alignment, outputfile, "stockholm")

# get alignment as formatted string
print ("Formatted AlignmentL:")
print (format(alignment, "clustal"))
```

**Output:**

```bash
Formatted AlignmentL:
CLUSTAL X (1.81) multiple sequence alignment

H.sapiens:NP_524628.2               MFTLQPTPTAIGTVVPPWSAGTLIERLPSLEDMAHKGHSGVNQLGGVFVG
M.musculus:NP_524638.3              ---MMLTTEHIMHGHPH-----SSVGQSTLFGCSTAGHSGINQLGGVYVN
X.tropicalis:NP_001006763.1         ---------------------------------MQNSHSGVNQLGGVFVN
T.rubripes:NP_001355831.1           --------------------------------------------------
D.rerio:AAH36957.1                  ---------------------------------MQNSHSGVNQLGGVFVN
D.rerio:XP_029701655.1              --------------------------------MMQNSHSGVNQLGGVFVN
D.melanogaster:NP_571379.1          ----MPQKEY-Y----N-----RATWESGVASMMQNSHSGVNQLGGVFVN
D.melanogaster:NP_571716.1          ----MPQKEY-H----N-----QPTWESGVASMMQNSHSGVNQLGGVFVN
```

**Exercise:**
Find the first alignment block that shows no gps across all 8 aligned sequences. 
1. Print the block.
2. Save the block as a new clustal formatted text file.
3. From that block, extract the D. rerio (zebrafish) sequences and print the two sequences 

<details>
<summary>Solution:</summary>
</details>

# Resources

* [Biopython Tutorial and Cookbook](http://biopython.org/DIST/docs/tutorial/Tutorial.html)
* [UVA Research Computing](https://www.rc.virginia.edu)