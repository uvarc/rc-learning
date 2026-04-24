---
title: Claude Code
date: "2026-04-20T00:00:00"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 130

menu:
  uva-rc-genai:
    parent: Getting Started
---

## Claude Code on HPC
Claude Code is available to HPC users as a software module. It is installed inside of an Ollama container and can be launched with either locally installed Ollama models or via UVA RC GenAI's API.  To load the software, you can run the following on the command line:
`module load apptainer claudecode`

UVA RC GenAI can be accessed with the following command:

```apptainer run $CONTAINERDIR/claudecode-2.1.80.sif -k $UVARC_GenAI_API```

where `$UVARC_GenAI_API` is your exported API key.

Local Ollama models can be run with the following command:

`apptainer run --nv --bind /path/to/models:/ollama_models \
$CONTAINERDIR/claudecode-2.1.80.sif -m /ollama_models
`

<div style="background-color: #dc3545; border-left: 4px solid
  #2196F3; padding: 12px; margin: 16px 0;">
  <strong>Note:</strong> To run local Ollama models with Claude Code, you must first request a GPU node (e.g., OOD Desktop, ijob, etc.).
</div>
