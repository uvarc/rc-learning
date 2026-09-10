---
title: Claude Code
date: "2026-04-20T00:00:00"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 70

menu:
  uva-rc-genai:
    parent: Usage
---

## Claude Code on HPC
[Claude Code](https://code.claude.com/docs/en/overview) is an AI-assistant for software engineering tasks that runs directly in the terminal to write, edit and analyze code. You can link it to UVA RC GenAI. 

### Installing Claude Code
First, you'll need to install claude code into your home account with the following:

```curl -fsSL https://claude.ai/install.sh | bash```

once the command is finished running, run the following to add `~/.local/bin` to your path:

```echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.bashrc && source ~/.bashrc```

### Claude Code with UVA RC GenAI

Edit your `~/.claude/settings.json` file to point towards UVA RC GenAI:

```
{
    "model": "Kimi K2.5",
    "env": {
    "ANTHROPIC_BASE_URL": "https://open-webui.rc.virginia.edu/api",
    "ANTHROPIC_AUTH_TOKEN": "<your-api-key>",
    "CLAUDE_CODE_MAX_OUTPUT_TOKENS": "4096"
  }
}
```

Then launch with:
`claude` on the command line

`claude` will run in whatever directory it's launched in. 

### VS Code's Claude Code Extension

The VS Code Claude Code extension can also be integrated with the above configuration in `~/.claude/settings.json`.
You'll want to ensure that the Claude Code extension is installed inside of your  [Open OnDemand VS Code session](https://learning.rc.virginia.edu/notes/vscode-intro/using-ood/#installing-extensions) prior to launching Claude Code.

{{< figure src="/notes/uva-rc-genai/img/vscode-extension.png" alt="Screenshot of VS Code's Claude Code extension in action. As long as the configuration inside of ~/.claude/settings.json is correct, VS Code should be able to automaticall detect the correct model and API endpoint for UVA RC GenAI. In this example, Claude Code is asked which model is being run, and it responds correctly with RC's hosted model">}}

