---
title: "Here Documents"
date: 2023-11-29-21:03:38Z
type: docs 
weight: 2400
menu: 
    bash-scripting:
---

A **here document** or _here file_ is a block of text that is dynamically generated when the script is run.
```no-highlight
CMD << [-] Delimiter
   line
   line
Delimiter
```
In this case, `CMD` is any bash command that reads from standard input, the _optional_ hyphen `-` strips tabs, and the Delimiter is an arbitrary text string.  Popular delimiters include `EOF`, `EOT`, and `END`. No spaces should follow the delimiter string on the line that closes the here document.

**Example**

{{< code-download file="/notes/bash-scripting/scripts/here.sh" lang="bash" >}}
