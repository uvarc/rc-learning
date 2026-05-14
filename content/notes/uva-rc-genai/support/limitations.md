---
title: Current Limitations
date: "2026-04-20T00:00:00"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 90

menu:
  uva-rc-genai:
    parent: Best Practices & Limitations
---

## Server Limitations
* **Context window:** 65536 tokens
* **Batched Token Length:** 131072 tokens
* **Rate Limiting:** 60 requests per minute

You may receive errors if any of these limits are reached. Errors may highlight that a rate limit has been exceeded if more than 60 requests are made within a minute, or you may receive context overflow errors if you surpass the maximum context length. Responses may cut off mid-sentence when the total token budget is used. 

## Workflow Challenges

If you're working with large documents, you may need to break them into smaller pieces to fit within the context window. High-volume workflows like processing thousands of files or running automated batches can quickly hit rate limits, so you'll need to pace your requests.
Long-running conversations or agentic workflows can exhaust the context window as chat history and file constraints accumulate. 

## Best Practices

* **Track token usage** - Monitor prompts and responses to avoid silent truncation.
* **Avoid large file uploads** - Split files into more maintainable chunks (if possible) to prevent filling context.
* **Saving Outputs** - Save full API output or OpenWebUI chat data (`.json` format) for debugging and reproducibility.
* **Restart sessions when needed** - Restart session to clear context or use `/clear` command in Claude Code to clear context window. You can also use the `/compact` command in Claude Code to clear context while maintaining a summary of your chat.

