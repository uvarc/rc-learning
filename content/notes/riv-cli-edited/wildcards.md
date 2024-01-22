---
title: Wildcards
date: 2023-12-11-14:11:14Z
type: docs 
weight: 1200
menu: 
    rivanna-command-line:
---

**Wildcards** are special characters that can stand for strings.  Wildcards enable you to work with groups of files without needing to type multiple files with similar names. 

The asterisk `*` can replace zero to unlimited characters except for a leading period.

The question mark `?` replaces exactly one character.  

**Examples**
```bash
ls *.py
cd array_test
ls input?.dat
ls input??.dat
ls input*.dat
rm list?.sh
```

{{< warning >}}
BE CAREFUL when using wildcards with rm! Gone is gone! On some systems there may be backups, or there may not be, and on your personal system you would need to set up backups and learn to retrieve files. It is advisable to first run an `ls` with the pattern you plan to use with `rm`.
{{< /warning >}}



