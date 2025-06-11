---
title: AlphaFold
date: 2025-05-20-00:23:54Z
type: docs 
weight: 400
menu: 
    rivanna-alphafold:
        name: AlphaFold
---

**AlphaFold** is a protein structure prediction system developed by DeepMind, an artificial intelligence company. It uses deep learning to predict the 3D structure of proteins directly from their amino acid sequence.

It is built on a neural network architecture with multiple different modules trained simultaneously using end-to-end learning.

AlphaFold's operational stages: 
- AlphaFold searches genetic databases to find sequences that are homologous to the input protein. 
- It passes these homologous sequences into a pre-trained neural network to predict the 3D positions of atoms in the protein.
- The neural network output is refined using molecular dynamics simulations to produce PDB files. A PDB file is the standard format for representing 3D protein structures. 

AlphaFold features: 
- It has both monomer and multimer versions. 
- It is based largely on coevolutionary information.

*"AlphaFold directly predicts 3D coordinates of all heavy atoms for a given protein using the primary amino acid sequence and aligned sequences of homologues as input”* 
> Jumper, J., Evans, R., Pritzel, A. et al. *Highly accurate protein structure prediction with AlphaFold*.