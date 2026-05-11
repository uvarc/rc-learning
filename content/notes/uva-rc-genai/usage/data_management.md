---
title: Data Management
date: "2026-04-20T00:00:00"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 80

menu:
  uva-rc-genai:
    parent: Usage
---


## Methods for Saving Chats
Methods for saving chats via API were discussed [earlier](/notes/uva-rc-genai/usage/api.md), but you also have the option to export or save chats through the OpenWebUI interface. Clicking the three dots next to your icon in the upper-right section of the interface displays the options to either download or copy the chat session.

{{< figure src="/notes/uva-rc-genai/img/browser_save.png" alt="Screenshot of OpenWebUI option to Download or Copy chat"  >}}

The chat can be downloaded in three formats (`.txt`, `.pdf`, `.json`). Saving as a `.txt` file stores only your prompts and the LLM's responses/reasonings; however, saving as a `.json` includes metadata (e.g., timestamps, model, version, message IDs, etc), which may be useful in certain cases. 

The "Copy" option copies only prompts, responses and reasonings, similar to what's stored in the `.txt` file.


## Storage Options
Downloaded browser chats will be saved to your local workstation but can be easily copied to HPC storage. If you're saving chats on HPC storage it's important to remember the limitations and use cases for each of RCs [storage options](https://www.rc.virginia.edu/userinfo/storage/).

### Complementary HPC Storage
**Home Storage**
* Personal storage space that is not shareable with other users and mounted at `/home/$USER`
* 200GB
* Best used as a working directory when using Rivanna/Afton interactively, not for Slurm jobs

**Scratch Storage**
* High performance parallel filesystem that is suitable for large scale computational work and mounted at `/scratch/$USER`
* Personal use and not shared with other users
* Data should be moved from /scratch for long-term storage. See [here](https://www.rc.virginia.edu/2024/07/reinstatement-of-file-purging-of-personal-/scratch-files-on-afton-and-rivanna/) for details on data purging policy.

### Leased Storage
**Research Project Storage**
* Best suited for data accessible to Rivanna/Afton, shared within a research group, and for researchers who are actively running Slurm jobs and need more capacity than /home provides. 

**Research Standard Storage**
* Best suited for data that needs to be accessible to a workstation or Rivanna/Afton, shared within a research group, and for longer-term storage of Slurm job outputs completed in /scratch. 
* Slurm Jobs are not recommended since file operations are slower compared to /home, /scratch, and /project storage.

## Reproducibility Considerations
Reproducibility controls are important since LLM outputs can vary across different runs. This makes it impossible to verify results, replicate research, or trace back errors. The following are some important considerations regarding output reproducibility.

* **Temperature and Seed** - Set `temperature=0` and a fixed `seed` when consistency matters. Higher temperature introduce randomness.

* **Prompt Versioning** - Track prompts in versioned files since even small wording changes can affect outputs.

* **Output Logging** - Save raw API responses with metadata (temperature, tokens, request ID) rather than just extracted text.

* **Context Window and State** - Document the full context window, including system prompts, prior conversation history, and any retrieved content (RAG); exceeding the window silently drops content.
