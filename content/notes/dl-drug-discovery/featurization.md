---
title: Featurization
date: 2025-03-11T13:24:11Z
type: docs 
weight: 500
menu: 
    dl-drug-discovery:
        parent: Deep Learning in the Optimization Stage
        
---

Information about how a molecule is organized can be broken down into different levels:

- **Atom level information** includes properties like atomic number, number of bonds, and atomic charge.
- **Molecule level features** describe characteristics such as the type of bond, whether the molecule is conjugated, and whether atoms are part of a ring structure.
- **Other properties** may be included depending on the featurization tool. For example, Chemprop incorporates global features that help distinguish between different molecular subsets or reflect modifiable properties.


In other words, in Python you can describe this kind of structure in your scripts using classes like `BondFeaturizer` or `AtomFeaturizer`.

You featurize the molecule, then proceed to split the data into train, validation, and test sets, followed by molecular loading and additional downstream steps.

Below is an example of how you might define such a featurizer:

```python
class AtomFeaturizer(Featurizer):
    def __init__(self, allowable_sets):
        super().__init__(allowable_sets)
    
    def symbol(self, atom):
        return atom.GetSymbol()
    
    def n_valence(self, atom):
        return atom.GetTotalValence()
    
    def n_hydrogens(self, atom):
        return atom.GetTotalNumHs()

    def hybridization(self, atom):
        return atom.GetHybridization().name.lower()

```

And below is an example of instantiating it with specific allowable sets:

```python
atom_featurizer = AtomFeaturizer(
    allowable_sets={
        "symbol": {"B","Br","C","Ca","Cl","F","H","I","N","Na","O","P","S"},
        "n_valence": {0,1,2,3,4,5,6},
        "n_hydrogens": {0,1,2,3,4},
        "hybridization": {"s","sp","sp2","sp3"},
    }
)
```