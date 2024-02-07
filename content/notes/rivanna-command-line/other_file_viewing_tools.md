---
title: Other File Viewing Tools
date: 2023-12-11-14:11:14Z
type: docs 
weight: 935
menu: 
    rivanna-command-line:
---

## Printing a file to the console

The `cat` (concatenate) command prints the contents of a file to the screen.
```bash
cat myfile
```
It does not fit the output to the terminal windows size; it simply keeps printing to the end.

It can also be used to create or join files (hence its name). We will learn more about its behavior here when we look at [standard streams](/notes/rivanna-command-line/standard_streams).

```bash
#create a file
cat > newfile
Now I can enter
some text
It will be copied exactly into the file
^d
```
The `^d` notation means hold down `Ctrl` and `d` together. It is the end-of-file marker for bash.

```bash
#append a file to another
cat otherfile >> newfile
```
{{< warning >}}
If the double `>>` is omitted, `cat` will overwrite `newfile` with `otherfile`.
{{< /warning >}}

## Displaying Parts of a File

### head and tail

`head` filename

Displays only the starting lines of a file. The default is first ten lines. Use “-n” to specify the number of lines.
```bash
$head ~/rivanna-cli/shakespeare/Lear.text
```
<pre>
This Etext file is presented by Project Gutenberg, in
cooperation with World Library, Inc., from their Library of the
Future and Shakespeare CDROMS.  Project Gutenberg often releases
Etexts that are NOT placed in the Public Domain!!

*This Etext has certain copyright implications you should read!*

&lt&ltTHIS ELECTRONIC VERSION OF THE COMPLETE WORKS OF WILLIAM
SHAKESPEARE IS COPYRIGHT 1990-1993 BY WORLD LIBRARY, INC., AND IS
PROVIDED BY PROJECT GUTENBERG WITH PERMISSION.  ELECTRONIC AND
</pre>

`tail` filename

Displays the last 10 lines.
```bash
$tail 30 ~/rivanna-cli/shakespeare/Lear.text
```
<pre>
     The cup of their deservings.- O, see, see!
  Lear. And my poor fool is hang'd! No, no, no life!
     Why should a dog, a horse, a rat, have life,
     And thou no breath at all? Thou'lt come no more,
     Never, never, never, never, never!
     Pray you undo this button. Thank you, sir.
     Do you see this? Look on her! look! her lips!
     Look there, look there!                            He dies.
  Edg. He faints! My lord, my lord!
  Kent. Break, heart; I prithee break!
  Edg. Look up, my lord.
  Kent. Vex not his ghost. O, let him pass! He hates him
     That would upon the rack of this tough world
     Stretch him out longer.
  Edg. He is gone indeed.
  Kent. The wonder is, he hath endur'd so long.
     He but usurp'd his life.
  Alb. Bear them from hence. Our present business
     Is general woe. [To Kent and Edgar] Friends of my soul, you
        twain
     Rule in this realm, and the gor'd state sustain.
  Kent. I have a journey, sir, shortly to go.
     My master calls me; I must not say no.
  Alb. The weight of this sad time we must obey,
     Speak what we feel, not what we ought to say.
     The oldest have borne most; we that are young
     Shall never see so much, nor live so long.
                                       Exeunt with a dead march.

THE END
</pre>
