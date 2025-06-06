---
title: Limitations of AlphaFold DB
date: 2025-05-20-00:23:54Z
type: docs 
weight: 2350
menu: 
    rivanna-alphafold:
    parent: AlphaFold Protein Structure Database
---
The AlphaFold DB uses a monomeric model similar to the CASP14 version. As a result, many of the same limitations are expected:

**Structural Limitations**

The DB model only predicts monomers, missing some important context in the form of protein complexes. The AlphaFold Multimer model is published separately as open-source, but pre-run models are not available.​

**Prediction Accuracy**

From Wikipedia:

>"Evolved proteins are not subject to random changes, only changes which allow the organism to survive. Mutations which significantly destabilize or change the structure of the protein are unlikely to be advantageous and outcompete the ?existing protein. CASP14 seems to be the prime source of validation for the model. This consists entirely of evolved proteins, so does not provide any evidence of performance against non-evolved (i.e. engineered) proteins. This strongly limits the usage of AlphaFold for protein engineering and design."

The model is unreliable for intrinsically disordered proteins, although it does convey the information via a low confidence score.​

The model is not validated for mutational analysis.​

The model relies to some extent upon co-evolutionary information across similar proteins, and thus may not perform well on synthetic proteins or proteins with very low homology to anything in the database.

**Practical Limitations**

The model only predicts one conformation for proteins with multiple structural states.  
It does not account for cofactors or post-translational modifications (PTMs), which can limit biological relevance.  
For example, 50–70% of human proteins** may be incomplete without attached glycans.  
However, because the model was trained on PDB entries that often contain such modifications, predicted structures are frequently consistent with the presence of ions or cofactors.  

The DB excludes proteins smaller than 16 or longer than 2700 amino acids, though full human proteome predictions are available via batch download.  

For drug discovery, AlphaFold’s resolution (~0.9 Å RMSD) is often insufficient for molecular docking, which typically requires positional accuracy within 0.3 Å.

Sources and more information:
- [AlphaFold Protein Structure Database](https://www.alphafold.ebi.ac.uk)
- [A Confused Chemist’s Review of AlphaFold 2 (LessWrong)](https://www.lesswrong.com/posts/6vybojuDEqaHeg8aN/a-confused-chemist-s-review-of-alphafold-2)
- [AlphaFold 2 Review – What’s Behind the Structure Prediction Miracle? (Blopig)](https://www.blopig.com/blog/2021/07/alphafold-2-is-here-whats-behind-the-structure-prediction-miracle/)
- [AlphaFold-Multimer](https://www.deepmind.com/publications/protein-complex-prediction-with-alphafold-multimer)



