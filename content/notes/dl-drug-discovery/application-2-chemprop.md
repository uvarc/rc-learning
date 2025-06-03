---
title: Chemprop Application, Part II
date: 2025-03-11-13:24:11Z
type: docs 
weight: 650
menu: 
    dl-drug-discovery: 
      parent: Chemprop Application Case Study
---

#### Halicin – Discovery of a Novel Antibiotic

After validating the Chemprop model on known compound libraries, the researchers applied it to search for new potential antibiotics. One of the most promising discoveries was a compound called **Halicin**.

Halicin was selected by combining the model’s predictions with additional filtering based on structural similarity and toxicity data. The goal was to find molecules that were not only predicted to work, but also **different from the training set** and **likely to be safe**. On the map below, Halicin is marked as a **yellow circle**, highlighting its uniqueness compared to other molecules in the dataset.

{{< figure src="/notes/dl-drug-discovery/img/Deep-Learning-Drug-Discovery_27.png" caption="Halicin (yellow circle) stands out from the training set and Broad library, showing that the model identified a structurally distinct compound." >}}

Halicin had the lowest similarity to the training set, which was one of the main criteria for follow-up testing. When tested in the lab, Halicin showed **strong antibiotic activity**, successfully stopping bacterial growth at low concentrations.











