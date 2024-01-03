---
title: Logging In
date: 2023-12-11-14:11:14Z
type: docs 
weight: 300
menu: 
    rivanna-command-line:
---

Logging into a remote UNIX based system requires a program generally called a _client_. The options for the client depends on your OS.

## SSH

Command line access through a terminal on your computer is based on the `ssh` or Secure Shell protocol.  It is
  * Encrypted
  * Available on most UNIX systems

Your ssh client communicates with the ssh _server_ program running on the remote system.  Once established, the connection is secure even if the network is insecure.   

**Example**

If your computer is a MacOS or Linux system, you log in with
```bash
ssh -Y mst3k@virginia.edu
```
Throughout this tutorial we will use `mst3k` as our example user ID. You should substitute your own.  The option `-Y` allows access to graphical applications and requires that an _X11 server_ application must be installed on your computer.  This should be the default for Linux, but MacOS users must install [XQuartz](https://xquartz.org) before this command-line option will be useful.
