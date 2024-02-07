
# Handy Commands

* whoami
* show my user ID
* which <executable>
  * indicates the path to the executable specified
* wc<file>
  * word count
    * wc–l <file>is lines only
* date
  * Shows instantaneous date and time
* exit
  * exit current shell (if login shell, this logs you off!)

# Standard Streams

Each executable has associated with it, three I/O streams: __standard input__ , __standard error__ , and __standard output__ .

Normally these streams come from or go to your console (i.e. your shell).

Most Unix commands read from standard input and/or write to standard output.

They are often represented as __stdin__ , __stderr__ , and __stdout__ .

# Stream Redirection

You can redirect standard input with <

mycode<params.txt

Redirect standard output with >

ls–l >filelist

Append with >>

cat file1 >>bigfile

Redirection of standard error depends on the shell

Bash:

make >&make.out

Redirects bothstdoutandstderrtomake.out

# Pipes

* One of the most powerful properties of Unix is that you can __pipe__ the standard output of one command into the standard input of another.
* The pipe symbol|is above the backslash on most US keyboards.
* Example
* grep "@H-148:116" SP_R1.fastq | head
  * grepsearches for the pattern in the file andheadlooks at the first 10 lines of thegrepoutput

# Running Executables

Executables are often called binaries, especially by Unix types and computer programmers.  The terms are synonymous in most cases.

If the executable is in your _search path_ you can simply type its name at the prompt.

gedithello_world.slurm

heregeditis the name of the binary.  Its actual location is/usr/bin/gedit,but/usr/binis in the default search path.

If it is not in your search path you must type the path to the executable (can be absolute or relative)

./hello_world.slurm

Usually current directory is not in your default search path for security reasons.

# Example

In most cases, current working directory (.) is not in your default search path.  To add it, type (for bash)

export PATH=$PATH:.

In this case it is essential to add the first$PATHor you will lose the default path set by the system.

# Process Control

Running from a __command line__ :

Processes can be running in the _foreground_ (no prompt returned to the shell) or _background_ (prompt available).  To start in the background add an ampersand (&) at the end of the command:

./myexec-omyoptmyfile&

control-z(ctrl-zor^z):suspend the job

bgplace into background

fgforward abackgroundedjob

# Killing Processes

control-c(ctrl-cor^c):kill the current running job (must be foregrounded).

Find the process ID withpsthen:

kill -9 <pid>

terminates with extreme prejudice.

killall-9 <executable name>

same as above.

# RIVANNA

# Rivanna in More Detail

