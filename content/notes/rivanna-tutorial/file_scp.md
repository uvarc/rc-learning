---
title: Transferring with Command-Line SCP
date: "2022-10-01:00:00Z"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 550

menu:
  rivanna-tutorial:
    parent: Working with Files
---

The syntax of the command-line `scp` is similar with all operating systems. Always execute it from your _local_ machine to a rivanna frontend.

Transfer _from_ local _to_ Rivanna
```bash
scp path/to/file mst3k@rivanna2.hpc.virginia.edu:
```
Be sure to include the colon after the `edu`.  The path will depend on the local operating system.  This syntax copies the file into your home folder on Rivanna.

Transfer _from_ Rivanna _to_ local
```bash
scp mst3k@rivanna2.hpc.virginia.edu:path/to/file localpath
```
The path on Rivanna is relative to your home folder.

## Windows

Go to Settings->Apps->optional features and be sure `OpenSSH Client` is installed.  This [site]( https://support.cci.drexel.edu/cci-virtual-lab-resources/scp-or-ssh-or-sftp-gui-or-cli/scp-windows-10-powershell-cli-command-line-interface/) has a good explanation of using command-line scp in both Powershell and cmd.exe. Of course you should substitute a Rivanna frontend for the Drexel computers.

## Mac OS and Linux

Open a Terminal window and type the `scp` command as shown above.
