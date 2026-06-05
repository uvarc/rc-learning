---
title: Jupyter Notebooks
date: "2026-04-20T00:00:00"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 70

menu:
  uva-rc-genai:
    parent: Usage
---

# Jupyter Notebook Workflow
UVA RC GenAI can be accessed from a Jupyter notebook using the python API client to integrate LLM capabilites for data analysis, code generation, and automating research workflows. 

## Basic Literature Review
Download {{< file-download file="/notes/uva-rc-genai/codes/lit_review_assistant.ipynb " text="Literature Review Notebook" >}} for a simple example of a basic literature review in a Jupyter notebook. The notebook demonstrates how to test the API connection and uses manual prompt engineering for individual abstracts of research articles.

While the basic notebook demonstrates direct API interaction suitable for 5-10 papers, scaling to comprehensive literature reviews requires automation. Retrieval-Augmented Generation (RAG) moves beyond the 'single-document' approach of the example notebook.

## Advanced Literature Review with RAG

RAG is a technique that automatically retrieves relevant text from a document collection before generating responses. It expands analysis to entire collections, eliminating  manual copy-paste workflows, enabling synthesis across hundreds of papers. RAG also reduces hallucinations by grounding LLM responses in retrieved source text, rather than relying on the model's training data alone.

| Current Notebook (manual) | RAG-Enhanced Version (automated) |
|------------------|----------------------|
| Full abstract pasted into prompt | Retrieved context + question |
| Manually provides relevant text | Retriever automatically finds relevant text |
| 5-10 papers at a time | Hundreds/thousands of papers indexed |

A RAG setup requires additional setup steps for document embedding and vector storage:

* Vector database setup (for storing document embeddings)
* Document indexing (converting PDFs/text to searchable vectors)
* Retrieval configuration (tuning search parameters for relevance)

Start with the Basic Literature Review notebook to understand API authentication and prompt engineering fundamentals. Consider migrating to the RAG approach when analyzing a large number of papers or working across research collections.

