---
title: Process Control
date: 2023-12-11T21:14:11-14:00
type: docs 
weight: 1500
menu: 
    rivanna-command-line:
---

A running executable is a _process_ to the Unix operating system. When it is run at a command line, a process can be running in the  _foreground_, which suppresses a prompt, or in the  _background_, which returns a prompt to the shell.  

To start in the background, add an ampersand `&` at the end of the command.
```bash
$./myexec -o myopt myfile&
```

## Managing Processes

The `jobs` command lists your running jobs (processes) with their job index.

The key combination control-z (ctrl-z or ^z) suspends the foreground job. To resume the job in the background, type `bg`.

This can be combined with output from `jobs`
```bash
$bg %1	# place the job number 1 into the background
$fg %4	# place the job number 4 back to the foreground
```

For more general information about processes, use `ps` (process status) The `-u` option limits it to processes owned by user `mst3k`.
```bash
$ps -u mst3k
   PID TTY          TIME CMD
498571 ?        00:00:00 systemd
498575 ?        00:00:00 (sd-pam)
498581 ?        00:00:00 pulseaudio
498582 ?        00:00:00 sshd
498593 pts/3    00:00:00 bash
498665 ?        00:00:00 dbus-daemon
498670 ?        00:00:00 dbus-daemon
498672 ?        00:00:00 dbus-kill-proce
498677 ?        00:00:00 gio
498685 ?        00:00:00 gvfsd
498691 ?        00:00:00 gvfsd-fuse
517189 pts/3    00:00:00 ps
```

The `pid` is the _process id_.  


## Killing Processes

You have accidentally started a production job on a frontend node.  What to do?

You can kill your forground process with `Crtl c`. 
```bash
#oops, I was supposed to run this through Slurm
$./myexe  
^c
```

If you need to kill a background process, you can use `jobs` to locate and foreground it.  You may also have processes that don't appear with `jobs`.  Use `ps` to find the PID, then
```bash
$kill -9 <pid>
```
Do not type the angle brackets, just the number. Many processes will ignore the kill command without the `-9` option so we routinely include it.

To kill by executable name
```bash
$killall -9 <executable name> 
```
The kill command with -9 immediately kills the process without allowing the process to clean up or save data. The killall command can be used to kill all of the processes that match a specific name or pattern.

If you find yourself in a jam and do not know what is wrong and you must start over, 
```bash
$kill -9 -1
```
kills **all** your processes, including your login.

