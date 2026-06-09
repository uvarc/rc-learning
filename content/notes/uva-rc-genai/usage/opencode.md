---
title: OpenCode
date: "2026-04-20T00:00:00"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 75

menu:
  uva-rc-genai:
    parent: Usage
---

## Opencode on HPC
[OpenCode](https://opencode.ai/) is an open source agent that helps you write code in your terminal.

### Installing OpenCode
First, you'll need to install OpenCode into your home account with the following command:

```curl -fsSL https://opencode.ai/install | bash```

once the command is finished running, add the following to `~/.config/opencode/opencode.json`:

```
{
  "$schema": "https://opencode.ai/config.json",
  "model": "uva-rc-genai/Kimi K2.5",
  "provider": {
    "uva-rc-genai": {
      "npm": "@ai-sdk/openai-compatible",
      "name": "UVA RC GenAI",
      "options": {
        "baseURL": "https://open-webui.rc.virginia.edu/api",
        "apiKey": "<your-api-key>"
      },
      "models": {
        "Kimi K2.5": {
          "id": "Kimi K2.5"
        }
      }
    }
  }
}
```

You'll need to replace `<your-api-key>` with your own personal API key. You can then launch by running `opencode` from the command line.

<div role="note" style="background-color: #dc3545; border-left: 4px solid #2196F3; padding: 12px; margin: 16px 0;">
  <strong>Note:</strong> You need to be on a compute node to run Claude Code or OpenCode sessions.
</div>
