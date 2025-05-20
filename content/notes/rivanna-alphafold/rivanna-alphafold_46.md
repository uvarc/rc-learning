---
title: Limitations
date: 2025-05-20-00:23:54Z
type: docs 
weight: 2350
menu: 
    rivanna-alphafold:
---


Limitations of Alphafold DB
The AlphaFold DB uses a monomeric model similar to the CASP14 version. As a result, many of the same limitations are expected:[75] 
The DB model only predicts monomers, missing some important context in the form of protein complexes. The AlphaFold Multimer model is published separately as open-source, but pre-run models are not available.
The model is unreliable for intrinsically disordered proteins, although it does convey the information via a low confidence score.
The model is not validated for mutational analysis.
The model relies to some extent upon co-evolutionary information across similar proteins, and thus may not perform well on synthetic proteins or proteins with very low homology to anything in the database.[76]
The model can only output one conformation of proteins with multiple conformations, with no control of which.
The model only predicts protein structure without cofactors and co- and post-translational modifications. This can be a significant shortcoming for a number of biologically-relevant systems:[65] between 50% and 70% of the structures of the human proteome are incomplete without covalently-attached glycans.[77] On the other hand, since the model is trained from PDB models often with these modifications attached, the predicted structure is "frequently consistent with the expected structure in the presence of ions or cofactors".[75]
The database does not include proteins with fewer than 16 or more than 2700 amino acid residues,[69] but for humans they are available in the whole batch file.
On its potential as a tool for drug discovery, Stephen Curry notes that while the resolution of AlphaFold 2's structures may be very good, the accuracy with which binding sites are modelled needs to be even higher: typically molecular docking studies require the atomic positions to be accurate within a 0.3 Å margin, but the predicted protein structure only have at best an RMSD of 0.9 Å for all atoms.


* unreliable for [intrinsically disordered proteins](https://en.wikipedia.org/wiki/Intrinsically_disordered_protein), but does convey that information via a low confidence score
* not validated for mutational analysis
  * relies to some extent upon co-evolutionary information across similar proteins, and thus may not perform well on synthetic proteins or proteins with very low homology to anything in the database
* can only output one conformation of proteins with multiple conformations, with no control of which
* only predicts protein structure without cofactors and co- and post-translational modifications.
  * 50% - 70% of the structures in the human proteome are incomplete without covalently-attached glycans
  * however, since the model is trained from PDB models often with these modifications attached, the predicted structure is "frequently consistent with the expected structure in the presence of ions or cofactors"
* drug discovery: typical molecular docking studies require atomic positions to be accurate within a 0.3 Å margin, but the predicted protein structures only have at best an RMSD of 0.9 Å for all atoms
* AlphaFold-multimer has a limit of nine chains or 1536 residues in its training and testing data
* Not multi-GPU enabled (limited acceleration possibilities)
---

From Wikipedia

Evolved proteins are not subject to random changes, only changes which allow the organism to survive. Mutations which significantly destabilize or change the structure of the protein are unlikely to be advantageous and outcompete the existing protein. CASP14 seems to be the prime source of validation for the model. This consists entirely of evolved proteins, so does not provide any evidence of performance against non-evolved (i.e. engineered) proteins. This strongly limits the usage of AlphaFold for protein engineering and design.


https://www.lesswrong.com/posts/6vybojuDEqaHeg8aN/a-confused-chemist-s-review-of-alphafold-2
https://www.blopig.com/blog/2021/07/alphafold-2-is-here-whats-behind-the-structure-prediction-miracle/



{{< figure src=/notes/rivanna-alphafold/img/Alphafold_58.png >}}

