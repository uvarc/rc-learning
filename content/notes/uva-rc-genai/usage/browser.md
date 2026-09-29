---
title: Browser Access
date: "2026-04-20T00:00:00"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 40

menu:
  uva-rc-genai:
    parent: Usage
---

After signing into [UVA RC GenAI](https://open-webui.rc.virginia.edu/), you should have browser access to the OpenWebUI interface.

{{< figure src="/notes/uva-rc-genai/img/openwebui.png" alt="Screenshot of OpenWebUI interface in browser with UVA RC GenAI"  >}}

Here, you can chat through the conversational interface, adjust integrations (e.g., web search), or even upload and attach content to the chat session.

Files can be loaded into the web interface – supported extensions include: pdf, docx, txt, md, csv, png, jpeg, jpg, pptx, xls, xlsx, json, sh, html, htm, xhtml, js, and py.

{{< warning >}}
  Chats are not saved. Conversation history disappears
  when you close the browser tab, sign out, or if the session expires.
{{< /warning >}}

More on data management will be discussed in [Data Management](/notes/uva-rc-genai/usage/data_management).

## Custom Models and Workspaces

Custom models can be created under the Workspace tab on the left side of the OpenWebUI interface. Custom models can be tuned with specific system prompts and additional capabilities outside of the base offering.

The base model is configured with streaming enabled on default. Custom models with streaming disabled can be useful to de-clutter output prompts from programatic API calls. 

### Steps to Configure Custom Model for programatic API access

1. Click "Workspace" in the sidebar followed by "+ New Model" on the page

2. Configure your custom model as desired and disable streaming:
   	     Advanced Params (click "Show") > Stream Chat Response (click to "Off")

3. Click "Save & Create" to add the custom model to your workspace.

If reproducibility is a concern, parameters such as `seed` and `temperature` can be configured as desired (Advanced Params (click show) > `Seed` and `Temperature` options)

{{< warning >}}
Reproducibility Note: While setting temperature=0 and a fixed seed minimizes variance, these parameters do not guarantee identical outputs across runs. Factors such as GPU floating-point precision, batching behavior, and inference engine optimizations (VLLM, CUDA drivers, etc.) can introduce non-determinism.
{{< /warning >}}
