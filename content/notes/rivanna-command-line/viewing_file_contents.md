---
title: Viewing Files
date: 2023-12-11-14:11:14Z
type: docs 
weight: 930
menu: 
    rivanna-command-line:
---

## Pagers

The most widely used way to view files quickly without editing is to use a _pager_.  A pager prints to the terminal window the amount of text that fits in that window.  The default pager on Linux is `more` (also called `less` because "less is more").

```bash
more filename
```
This displays file the contents on the screen with line scrolling. To scroll you can use ‘arrow’ keys. To advance one line, press the Enter key.  To advance a full page, press the space bar. Press `q` to exit.

```bash
$more ~/rivanna-cli/shakespeare/Lear.txt
```

To page upward within the text, press `b` (back).

### Searching.

You can search in the forward direction with `/`<pattern>, where pattern is a combination of characters you wish to find.

```bash
$more ~/rivanna-cli/shakespeare/Lear.text
 /serpent
```
<pre>
 ...skipping
     Turn all her mother's pains and benefits
     To laughter and contempt, that she may feel
     How sharper than a serpent's tooth it is
     To have a thankless child! Away, away!                Exit.
</pre>

Search stops at the first occurrence. To locate the next one, type `n`.

