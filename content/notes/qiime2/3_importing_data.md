---
title: Importing Data
date: 2025-06-13T14:45:04Z
type: docs 
weight: 150
menu: 
    qiime2:
---

Data produced by QIIME 2 exist as **QIIME 2 artifacts**. A QIIME 2 artifact contains data and metadata. The metadata describes things about the data, such as its type, format, and how it was generated (provenance). A QIIME 2 artifact typically has the .qza or .qzv file extension when stored in a zip file.

Since QIIME 2 works with artifacts instead of data files (e.g. FASTA files), you must create a QIIME 2 artifact by importing data. You can import data at any step in an analysis, though typically you will start by importing raw sequence data. QIIME 2 also has tools to export data from an artifact. See the [QIIME 2 Importing Guide](https://docs.qiime2.org/2022.8/tutorials/importing/) for details on how to import data.

Note: if you are having trouble unzipping the file, you can run this line in your command line.
```bash
qiime tools extract
```

For more information, visit [https://dev.qiime2.org/latest/storing-data/](https://dev.qiime2.org/latest/storing-data/
)
