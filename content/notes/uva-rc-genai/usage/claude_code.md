---
title: Claude Code
date: "2026-04-20T00:00:00"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 60

menu:
  uva-rc-genai:
    parent: Usage
---

## Claude Code on HPC
Claude Code is an AI-assistant for software engineering tasks that runs directly in the terminal to write, edit and analyze code. You can link it to UVA RC GenAI. 

### Installing Claude Code
First, you'll need to install claude code into your home account on a login node with the following:

```curl -fsSL https://claude.ai/install.sh | bash```

once the above command is finished running, run the following to add ~/.local/bin to your path:

```echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.bashrc && source ~/.bashrc```

### Claude Code with UVA RC GenAI

Set these environment variables:
```
export API_KEY="<your_api_key>"
export ANTHROPIC_BASE_URL="https://open-webui.rc.virginia.edu/api"
export ANTHROPIC_AUTH_TOKEN=$API_KEY
export ANTHROPIC_API_KEY=""
export CLAUDE_CODE_MAX_OUTPUT_TOKENS=4096
```
Then launch with:
`claude --model 'Kimi K2.5'`

