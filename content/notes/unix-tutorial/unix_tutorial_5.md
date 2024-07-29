---
date: "2023-12-11"
title: "Processes and Jobs"
weight: 60
---

A _process_ is an executing program identified by a unique PID (process identifier). To see information about your processes, with their associated PID and status, type
```bash
% ps -u mst3k
```

A process may be in the foreground, in the background, or be suspended. In general the shell does not return the Unix prompt until the current process has finished executing. Some processes take a long time to run and hold up the terminal. Backgrounding a long process has the effect that the Unix prompt is returned immediately, and other tasks can be carried out while the original process continues executing.

## Running Background Processes
To background a process, type an & at the end of the command line. For example, the command sleep waits a given number of seconds before continuing. Type
```bash
% sleep 10
```
This will wait 10 seconds before returning the command prompt %. Until the command prompt is returned, you can do nothing in that terminal except wait. To run sleep in the background, type
```bash
% sleep 10 &

[1] 6259
```
The `&` runs the process in the background and returns the prompt immediately, allowing you to run other programs while waiting for that one to finish. The first line in the above example is typed in by the user; the next line, indicating _job number_ and PID, is returned by the machine. The user is notified of a job number (numbered from 1) enclosed in square brackets, together with a PID and is notified when a background process is finished.  Backgrounding is useful for jobs which will take a long time to complete.

To the system, a "job" represents a group of processes (often just one) to which _signals_ should be sent. Signals include backgrounding, foregrounding, and cancelling. This meaning of "job" must be distinguished from "jobs" submitted to a queueing system such as Slurm.  To a resource manager (queueing system) a "job" is a set of instructions to be executed.

### Backgrounding a Current Foreground Process

At the prompt, type
```bash
% sleep 100
```
You can suspend the process running in the foreground by holding down the `[CTRL]` key and typing [`z`] (written as `^Z`) Then to put it in the background, type
```bash
% bg
```
Note: do not background programs that require user interaction.

### Listing Suspended and Background Processes
When a process is running, backgrounded or suspended, it will be entered onto a list along with a job number. To examine this list, type
```bash
% jobs
```
An example of a job list could be

1. Suspended sleep 100
2.  Running firefox
3. Running vi

To restart (foreground) a suspended processes, type
```bash
% fg %jobnumber
```
For example, to restart sleep 100, type
```bash
% fg %1
```

Typing `fg` with no job number foregrounds the last suspended process.

## Killing a Process

### `kill` (terminate or signal a process)

It is sometimes necessary to kill a process (for example, when an executing program is in an infinite loop). To kill a job running in the foreground, type ^C ([CTRL] + [c]). For example, run
```bash
% sleep 100 ^C
```

To kill a suspended or background process, type
```bash
% kill %jobnumber
```

For example, run
```bash
% sleep 100 &

% jobs
```

If it is job number 4, type
```bash
% kill %4
```

To check whether this has worked, examine the job list again to see if the process has been removed.

### `ps` (process status)
Alternatively, processes can be killed by finding their process numbers (PIDs) and using `kill PID_number`:
```
% sleep 100 &
% ps

PID   TTY       TIME CMD
20077 pts/5 S   0:05 sleep 100
21563 pts/5 T   0:00 firefox
21873 pts/5 S   0:25 vi
```
To kill off the process `sleep 100`, type
```
% kill 20077
```
and then type ps again to see if it has been removed from the list. If a process refuses to be killed, uses the `-9` option, i.e., type
```
% kill -9 20077
```

Linux systems support a command `killall` which takes the name of the process rather than its PID.
```bash
killall -9 sleep
```
Note: It is not possible to kill off other users' processes!  Unless, of course, it is a system you control and for which you have _root_ privileges.  The _root_ account is the system administrator, and on Linux is all-powerful.

## Summary
| Command | Operation |
|---|---|
| `ls -lag` | list access rights for all files |
| `chmod [options] file` | change access rights for named file |
| `command &` | run command in background |
| `^C` | kill the job running in the foreground |
| `^Z` | suspend the job running in the foreground |
| `bg` | background the suspended job |
| `jobs` | list current jobs |
| `fg %1` | foreground job number `1` |
| `kill %1` | kill job number `1` |
| `ps` | list current processes |
| `kill 26152` | kill process number `26152` |
| `killall name` | kill process `name` |

