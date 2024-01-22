---
title: Navigating the Bash Shell
date: 2023-12-11-14:11:14Z
type: docs 
weight: 550
menu: 
    rivanna-command-line:
---

Modern shells provide useful "hotkey" commands that can save considerable typing. The control- notation means that the `Ctrl` (control) key and the following key should be depressed at the same time.

{{< table >}}

| Function |  Key |
|-------|-----|
|Tab completion: expand typed string to as far as it has a unique name. | tab |
|Search for earlier command | control-r <cmd> |
|Move to the beginning of the line | control-a |
|Move to the end of the line | control-e |
|Clear the screen | `clear` or control-l |
{{< /table >}}

## Arrow Keys

When using bash you may use its built-in history mechanism to save yourself some keystrokes.

{{< table >}}
| Function |  Key |
|-------|-----|
|scroll through the previous commands you have typed | up arrow|
|if already scrolled back, scroll to more recent commands | down arrow |
|edit text on a line | right or left arrow |
{{< /table >}}

## Logging Out

To log out of a shell, type `exit`
```bash
exit
```

This logs you out of your current terminal window. If that is a login window, for example your `ssh` shell, it will log you out of the system entirely.

