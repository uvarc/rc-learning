---
title: Transferring Files from the Command Line
date: 2023-12-11-14:11:14Z
type: docs 
weight: 950
menu: 
    rivanna-command-line:
---

Files can be transferred by [graphical clients](https://www.rc.virginia.edu/userinfo/rivanna/logintools/graphical-sftp) such as MobaXterm and Filezilla, or through [Globus](/tutorials/globus-data-transfer).  If you are using a terminal from your local computer, you can also use some command-line tools.

### scp and sftp

The secure shell protocol includes two file-transfer command-line tools; `scp` and `sftp`.  Sftp is scp with a slightly different interface.

```bash
scp LOCAL_FILE mst3k@rivanna.hpc.virginia.edu:REMOTE_PLACE
scp mst3k@rivanna.hpc.virginia.edu:REMOTE_FILE LOCAL_PLACE
```
`REMOTE_PLACE` and `LOCAL_PLACE` refer to the location on the appropriate host where you want the file to be written.  `REMOTE_PLACE` can be omitted and the file will be transferred to your home directory under the same name.  To change the name or specify a directory on the other system, use a different name or path for `REMOTE_PLACE`.

`LOCAL_PLACE` must be present but can be `.` for the directory where the `scp` was run.

The colon `:` is required.

To copy a directory, use `scp -r` similarly to `cp -r`.

**Examples**
```bash
scp myfile.txt mst3k@rivanna.hpc.virginia.edu:
scp myscript.py mst3k@rivanna.hpc.virginia.edu:project1
scp myscript.py mst3k@rivanna.hpc.virginia.edu:script.py
scp myfile.csv mst3k@rivanna.hpc.virginia.edu:/scratch/mst3k
scp mst3k@rivanna.hpc.virginia.edu:/scratch/mst3k/run11/output.txt .
```

Scp resembles cp.  Sftp is an implementation over scp of the interface of a popular, but insecure, protocol widely used in the past called `ftp` (file transfer protocol).

```bash
$sftp mst3k@rivanna.hpc.virginia.edu
sftp> put local_file
sftp> get remote_file
sftp> quit
```
The sftp client permits other commands. `ls` lists files on the remote system.  `lls` lists local files.

### rsync

The rsync command is used to _synchronize_ files and folders.  It has many options and some attention must be paid to whether a trailing slash is needed or not.

```bash
rsync -av ldir/ mst3k@rivanna.hpc.virginia.edu:rdir
rsync my_file mst3k@rivanna.hpc.virginia.edu:/scratch/$USER
```
By default `rsync` does not transfer files that are older than the equivalent on the target. This can increase the transfer speed significantly. Rsync also resumes a transfer that was interrupted.  Scp always starts again from the beginning.

Rsync is very powerful but has many options and can be confusing. For more details see our [documentation](https://www.rc.virginia.edu/userinfo/rivanna/logintools/cl-data-transfer). Several online resources with examples are also available, such as [this](https://www.digitalocean.com/community/tutorials/how-to-use-rsync-to-sync-local-and-remote-directories).

### Setting up Passwordless ssh

If you will frequently use ssh, scp, sftp, or rsync to a remote system, it becomes inconvenient to repeatedly enter your password.  You can generate a _key_ to log in without a password.

1. Go to the `.ssh` directory.  Note that this is a hidden directory.
2. Run the command `ssh-keygen`
3. When asked for a passcode, hit `Enter` to leave it blank.
4. Transfer the file `id_rsa.pub` to the remote system.  You may wish to name it something else, such as `myhost.pub`.
5. Append this file ending in .pub to a file called `authorized_keys`.  Create it if it doesn't exist.

