---
title: Input - FASTA File
date: 2025-05-20-00:23:54Z
type: docs 
weight: 1100
menu: 
    rivanna-alphafold:
        parent: Running AlphaFold On Rivanna
---

To run AlphaFold, you will need a FASTA file containing your protein sequence.

You can generate a FASTA file from your own sequence, or download one from a protein database such as [UniProt](https://www.uniprot.org/).

As an example, you can download a sequence from UniProt, such as [P21554 (CNR1)](https://rest.uniprot.org/uniprotkb/P21554.fasta), which corresponds to the human Cannabinoid Receptor 1 (CNR1), a GPCR protein.

Next, get your FASTA file onto Rivanna in your working directory. You can do this using OOD, Globus, or `scp` (command line tool), or you can make the file directly on Rivanna with a text editor like `nano` or `vim`. 

{{< figure src=/notes/rivanna-alphafold/img/Alphafold_22.png width=40% height=40% caption="Schematic diagram of the expected structure for CNR1">}}

{{< figure src=/notes/rivanna-alphafold/img/Alphafold_23.png width=90% height=90% caption="Text representation of a FASTA file for CNR1">}}