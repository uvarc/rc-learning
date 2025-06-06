---
title: Input - FASTA File
date: 2025-05-20-00:23:54Z
type: docs 
weight: 1150
menu: 
    rivanna-alphafold:
---

To run AlphaFold, you will need a FASTA file containing your protein sequence.

First, generate a FASTA file in your preferred manner. For this tutorial, we are using a sequence from UniProt:
* [https://rest.uniprot.org/uniprotkb/P21554.fasta](https://rest.uniprot.org/uniprotkb/P21554.fasta)
This is the UniProt ID **P21554**, which corresponds to the human Cannabinoid Receptor 1 (CNR1), a GPCR protein.

Next, get your FASTA file onto Rivanna in your working directory. You can do this using OOD, Globus, or `scp` (command line tool), or you can make the file directly on Rivanna with a text editor like `nano` or `vim`. 

{{< figure src=/notes/rivanna-alphafold/img/Alphafold_22.png caption="Text representation of a FASTA file for CNR1">}}

{{< figure src=/notes/rivanna-alphafold/img/Alphafold_23.png caption="Schematic diagram of the expected structure for CNR1">}}

