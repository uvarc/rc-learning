---
title: API Access
date: "2026-04-20T00:00:00"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 50

menu:
  uva-rc-genai:
    parent: Usage
---

API access lets you send requests to UVA RC GenAI programmatically from your code, instead of typing into a chat window. This enables automation and integration with existing workflows like bulk processing, large-scale data analysis, and connection to other tools. 

## Use Cases

**Browser Access:**
* Asking a single research question or a quick explanation
* Brainstorming ideas for a project or paper
* Drafting or polishing a single email or paragraph
* Experimenting with prompts

**API Access:**
* Processing hundreds or research papers to extract summaries
* Building a pipeline that automatically categorizes and tags new data as it arrives
* Integrating the LLM into a custom internal tool 

Overall, use the browser when you're interacting directly, and use the API when you want the LLM to work inside your existing workflows to handle volume without you having to type each prompt.

## Getting your API Key
1. Sign in to [UVA RC GenAI](https://open-webui.rc.virginia.edu/)

2. Click profile (top right) → Settings → Account

3. Select “Show” next to “API Keys”

{{< figure src="/notes/uva-rc-genai/img/apikey.png" alt="Screenshot of settings tab in browser where API key can be viewed, copied, or created"  >}}

You will have the option to view, copy or create a new API key.

## Securely Storing Your Key
Use environment variables to safely store your key (e.g., `export UVARC_GenAI_API="your-key-here"`). Make sure to never commit keys to code repositories, and regenerate keys in UVA RC GenAI browser if compromised.


<div role="note" style="background-color: #dc3545; border-left: 4px solid #2196F3; padding: 12px; margin: 16px 0;">
  <strong>Note:</strong> You need to be on a compute node to run your code.
</div>

HPC compute nodes can be accessed either via [OpenOn Demand](https://www.rc.virginia.edu/userinfo/hpc/ood/) (JupyterLab, Desktop, etc) or through an [ijob](https://www.rc.virginia.edu/userinfo/hpc/slurm/#submitting-an-interactive-job) from the command line. 

## Code Examples

Each of the following examples prompts "Hello" to UVA RC GenAI via API.

{{< spoiler text="Curl Example" >}}
{{< code-download file="/notes/uva-rc-genai/codes/uva-rc-genai-curl.sh" lang="bash" >}}
{{< /spoiler >}}

{{< spoiler text="Python with OpenAI Library" >}}
{{< code-download file="/notes/uva-rc-genai/codes/uva-rc-genai-oai.py" lang="python" >}}
{{< /spoiler >}}

## Saving Outputs

Outputs can be saved programmatically by capturing the response from your HTTP client (like `requests` or `openai` in Python, `axios` in Node.js, or `curl` in shell) using standard I/O methods to save the results to your storage. 

For example, pipe `curl` output directly to a file with `> output.txt`, or in Python use 
```
with open("output.txt", "w") as f:
    f.write(response_text)
```

## Token Tracking

It's important to monitor token usage to stay within limits and ensure requests don't exceed the context window. Token counts are included in every API response. For example, in the OpenAI library:

```
print(response.usage.prompt_tokens)      # Input tokens
print(response.usage.completion_tokens)  # Output tokens
print(response.usage.total_tokens)       # Combined
```

Similar fields exist in the Anthropic SDK and other tools.
