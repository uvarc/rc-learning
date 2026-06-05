---
title: "Terminology"
date: "2026-04-20T00:00:00Z"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs
weight: 37

menu:
  uva-rc-genai:
    parent: Usage
---

The following are common terms used when working with UVA RC GenAI and LLMs in general.

### Core Units

* **Token** - The basic unit of text LLMs process. Can be a whole word, part of a word, or a single character. Roughly 100 tokens ≈ 75 words. All usage, limits, and costs are measured in tokens.

* **Prompt** - The input text you send to the model. Prompts can be a question, instruction, or conversation history. A prompt is measured in tokens.

* **System Prompt** - A hidden prompt that sets the model's behavior rules for the entire conversation—such as tone, role, or safety constraints. It frames how the model interprets all subsequent user prompts.

### Capacity Limits

* **Context Window** - The maximum number of tokens (input + output combined) a model can consider at one time. 

* **Batched Token Length** - The total tokens across multiple requests sent together as a single batch. Batching can improve efficiency; this is the sum of all tokens in that group.

### Generation Parameters (API access only)

* **Temperature** - Controls output randomness. Lower values (0.1-0.3) produce more focused, deterministic responses; higher values (0.8-1.0) produce more varied and creative outputs.

* **Seed** - An integer that initializes the random number generator. Using the same seed with identical prompts and settings produces reproducible outputs.

### Usage and Operations

* **Metadata** - Non-content information (e.g., timestamp, model version, token counts, request ID, etc).

* **Rate Limiting** - Restrictions on tokens or requests per time period to prevent overload and ensure fair access.

