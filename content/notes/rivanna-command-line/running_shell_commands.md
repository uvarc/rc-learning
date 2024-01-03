---
title: Running Shell Commands
date: 2023-12-11-14:11:14Z
type: docs 
weight: 400
menu: 
    rivanna-command-line:
---

The syntax of Unix commands is not completely standardized but in general follow the pattern of a two or three-letter abbreviation followed by _command-line options_, which are single-letter options preceded by one hyphen or multiple-letter options with two hyphens. Many commands also take _arguments_.

{{< hl >}}
cmd -o opt1 --anoption opt2 argument
{{</hl >}}

**Example**
Invoke the utility `rm` to delete a file.
```bash
rm myfile
```
In this example, the shell issues a request to the kernel to delete `myfile`.  The kernel then communicates with the software that manages file storage to exectute the operation.

When it is complete the shell then returns the UNIX prompt to the user, indicating that it is waiting for further commands.

