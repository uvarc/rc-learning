---
title: API Access
date: "2026-04-20T00:00:00"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 50

menu:
  uva-rc-genai:
    parent: Getting Started
---

API access to UVA RC GenAI is useful for users who want to integrate the LLM into their code.

## Getting your API Key
1. Sign in to [UVA RC GenAI](https://open-webui.rc.virginia.edu/)

2. Click profile (top right) → Settings → Account

3. Select “Show” next to “API Keys”

You will have the option to view, copy or create a new API key.

## Securely Storing Your Key
Use environment variables to safely store your key (e.g., `export UVARC_GenAI_API ="your-key-here"`). Make sure to never commit keys to code repositories, and regenerate keys in UVA RC GenAI browser if compromised.


<div style="background-color: #dc3545; border-left: 4px solid
  #2196F3; padding: 12px; margin: 16px 0;">
  <strong>Note:</strong> You need to be on a compute node to run your code.
</div>

## Code Examples

{{< spoiler text="Curl Example" >}}
{{< code-download file="/notes/uva-rc-genai/codes/uva-rc-genai-curl.sh" lang="bash" >}}
{{< /spoiler >}}

{{< spoiler text="Python with OpenAI Library" >}}
{{< code-download file="/notes/uva-rc-genai/codes/uva-rc-genai-oai.py" lang="python" >}}
{{< /spoiler >}}

{{< spoiler text="Python Requests" >}}
{{< code-download file="/notes/uva-rc-genai/codes/uva-rc-genai-requests.py" lang="python" >}}
{{< /spoiler >}}



[Jupyter Notebook Reference](https://www.rc.virginia.edu/data/LLM_API_Example.zip) (ZIP File)
