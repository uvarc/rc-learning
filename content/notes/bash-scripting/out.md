![](img/2018-10-18_bash_wkshp%20%28kah3f%40VirginiaEDU%290.jpg)

Introduction

to Bash Scripting

![](img/2018-10-18_bash_wkshp%20%28kah3f%40VirginiaEDU%291.png)

Katherine Holcomb

October 18\, 2017

<span style="color:#FE4A02"> __www\.arcs\.virginia\.edu__ </span>

# Outline

* Scripting Introduction
* Bash Script Structure
* Hands\-on:
                * Executing a Script
                * Variables and Expressions
                * Conditionals
                * Comparisons \(Integer Values and Strings\)
                * Loops
                * Command Line Arguments
                * String Operations
                * Arrays
                * Dealing with Files

# More Tutorials and Resources

* Linux Shell Scripting Tutorial:
  * [http://bash\.cyberciti\.biz/guide/Main\_Page](http://bash.cyberciti.biz/guide/Main_Page)
  * [http://tldp\.org/HOWTO/Bash\-Prog\-Intro\-HOWTO\.html](http://tldp.org/HOWTO/Bash-Prog-Intro-HOWTO.html)
* Advanced Scripting Tutorial
  * [http://tldp\.org/LDP/abs/html/](http://tldp.org/LDP/abs/html/)
* Regex Tutorials
  * [http://www\.regular\-expressions\.info/](http://www.regular-expressions.info/)
  * [http://www\.zytrax\.com/tech/web/regex\.htm](http://www.zytrax.com/tech/web/regex.htm)
* Sed and \(g\)awk tutorial
      * [http://www\.grymoire\.com/Unix/Sed\.html](http://www.grymoire.com/Unix/Sed.html)
      * [http://www\.grymoire\.com/Unix/Awk\.html](http://www.grymoire.com/Unix/Awk.html)

# What is a Script, What Can it be used for?

A Bash script is a plain text file that contains instructions for the computer to execute\.

Scripts are interpreted at runtime and executed line\-by\-line\.  Scripts are not standalone executables but must be run through an interpreter\.

Anything that can be executed or evaluated on the bash command line can be placed into a script\.

Frequently used to automate repetitive tasks:

                * File handling\, data backups
                * Schedule computing jobs\, e\.g\. on UVA’s Rivanna High\- Performance Computing Cluster\.

# How to write a script

1\) Bash shell environment to execute scripts

![](img/2018-10-18_bash_wkshp%20%28kah3f%40VirginiaEDU%292.png)

![](img/2018-10-18_bash_wkshp%20%28kah3f%40VirginiaEDU%293.png)

![](img/2018-10-18_bash_wkshp%20%28kah3f%40VirginiaEDU%294.png)

![](img/2018-10-18_bash_wkshp%20%28kah3f%40VirginiaEDU%295.png)

![](img/2018-10-18_bash_wkshp%20%28kah3f%40VirginiaEDU%296.png)

![](img/2018-10-18_bash_wkshp%20%28kah3f%40VirginiaEDU%297.jpg)

* 2\) Needed: Text Editor to create and edit script files
            * \- vi\, vim\, nano
            * \- gedit through FastX
            * \- avoid Windows Notepad\, Microsoft Word

# Our Workspace Setup

__Basic Unix commands:__

cd \<path>		changes current directory to \<path>

cd ~/			changes to your home directory

pwd			shows full path of current directory

mkdir –p \<path>		create entire directory \<path>

ls			lists non\-hidden files in current directory

ls \*\.sh			lists non\-hidden files that end in \.sh

mv <old\_filename> <new\_filename>	move or rename file

cd ~/

pwd

mkdir scripts

cd scripts

pwd

ls

# Bash Script Structure: A Simple Example

Create script hello\.sh

_\#\!/bin/bash_

<span style="color:#0070C0">\#This is a simple bash script</span>

echo "Hello world\!"  _\#print greeting to screen_

echo "I am done now\!"

The first line tells the system to start the shell and execute the commands\.  It is sometimes called a  _“shebang”_ \. An alternative is:

_\#\!/_  _usr_  _/bin/_  _env_  _ bash_

Scripts can be annotated with comments\, characters after the  <span style="color:#0070C0">\#</span>  character are ignored by the interpreter

The third and fourth line are commands\.

# Executing A Script

Suppose the previous script was in a file  hello\.sh

By default it is just text\.  You must explicitly make it executable:

chmod u\+x hello\.sh

Then you can just invoke it by name at the command line:

\./hello\.sh

_Alternative:_  Run via bash interpreter

bash hello\.sh

# Comments and Line Continuations

All text from the  <span style="color:#0070C0">\#</span>  to the end of the line is ignored

To continue a statement on to the next line\, type  <span style="color:#FF0000">\\</span> \<enter>

<span style="color:#0070C0">\#This is a comment</span>

echo "This line is too long I  <span style="color:#FF0000">\\</span>

want it to be on two lines"

Multiple statements can be placed on the same line when separated by the  <span style="color:#FF0000">;</span>

a=20 <span style="color:#FF0000">;</span>  b=3

# Sourcing

* If you execute the script as a standalone script\, it starts a new shell \-\- that is what the  _shebang_  does \-\- if you do not use that you can type
  * bash myscript\.sh
* Sometimes you just want to bundle up some commands that you repeat regularly and execute them within the  _current_  shell\.  To do this you should  _not include _ a  _shebang_ \, and you must  _source_  the script
* \. \<script> or  source \<script>

# Variables: Placeholders to store values

The shell permits you to define variables\.

__Scalar Variable: __ Holds single value

color _=_ blue

echo  <span style="color:#FF0000">$</span> color

__Array Variable: __ Holds multiple values

colors _=_ \(blue red green\)

echo  <span style="color:#FF0000">$</span> \{colors\[@\]\}  _\# prints all values_

echo  <span style="color:#FF0000">$</span> \{colors\[0\]\}   _\# prints first value_

echo  <span style="color:#FF0000">$</span> \{colors\[1\]\}   _\# prints second value_

A _ssigning_  a value to a variable\.  __Do not put spaces around the __  _=_  __ sign\.__

R _eferencing_  a variable\. The  <span style="color:#FF0000">$</span>  tells the interpreter to look up the value of the variable symbol\.

# Exercise

Create a new script color\.sh

Define a variable and assign it a value

Print the variable’s value to the screen

# Executing a command

Characters between a pair of backticks  <span style="color:#FF0000">\`</span>  are interpreted and executed as commands

The output of an executed command can be stored in variable or used as input for another command

In current shell versions $\(COMMAND\) is equivalent to \`COMMAND\`

__Create new script __  __command\.sh__

_\#\!/bin/bash_

echo  <span style="color:#FF0000">\`</span> date <span style="color:#FF0000">\`</span>   _\# exec date command\, print output_

p= <span style="color:#FF0000"> __\`__ </span> pwd <span style="color:#FF0000"> __\`__ </span>   _\# assign output to variable first_

echo Current directory: $p

# Escaping Special Characters

Bash uses several special characters like $\, “\, ‘\, \`\, \\\, and others that are interpreted with special meaning\.

At your bash prompt\, type

echo Cool\. You won $100\,000 in the lottery\.

_\# Try with escaping the $ character_

Echo Cool\. You won  <span style="color:#FF0000">\\</span> $100\,000 in the lottery\.

The  <span style="color:#FF0000">\\</span>  character instructs the interpreter to treat the next character as a literal and ignore \(escape\) any possible special meaning

# Curly Braces: Keeping a tight wrap on variable names

Create script name\.sh

_\#\!/bin/bash_

firstname=Peter

lastname=Miller

fullname="$firstname $lastname"

_\# simple variable operation_

echo Hello $fullname

_\#Try to echo "Where are the Millers?"_

echo Where are the $lastname s?

echo Where are the $lastnames?

echo Where are the $\{lastname\}s?

Curly braces \{\} are used to separate a variable name from adjacent characters\.

# Strings, Single (') and Double (") quotes

Bash is not very good at math so much of what we do with Bash involves  <span style="color:#FF0000">strings</span> \, e\.g\. a sequence of text and special characters\.

__Pair of Single Quotes ''__

preserves the literal value of each character within the quotes\.

__Pair of Double Quotes ""__

preserves the literal value of all characters within the quotes with the exception of  <span style="color:#FF0000">$ </span> \,  <span style="color:#FF0000">\` </span> \, and  <span style="color:#FF0000">\\</span>  \, which retain their special meaning\.

<span style="color:#FF0000">         $</span> 		Shell Expansion\, e\.g\. variable name follows

<span style="color:#FF0000">          \\</span> 		Escape character

<span style="color:#FF0000">\`COMMAND\`</span> 	Executes COMMAND and returns its output

stream; equiv\. to $\(COMMAND\) in newer versions

![](img/2018-10-18_bash_wkshp%20%28kah3f%40VirginiaEDU%298.png)

# Comparing Single and Double Quotes

__Example: __

Open name\.sh and add the following statements

_\# Compare single and double quotes_

echo 'Where is Mr\. $\{lastname\}?'

echo "Where is Mr\. $\{lastname\}?"

__Output:__

Where is Mr\. $\{lastname\}?

Where is Mr\. Miller?

# Conditionals

* The shell has an if else construct to allow execution of distinct command sets based on a predefined condition
  * if \[\[ \<condition> \]\]
  * then
  * commands
  * elif \[\[ \<condition> \]\]    _\#optional         _      	commands
  * else                      _\#optional_
  * more commands
  * fi  _\#ends the if condition block _

__Note: A space is required after __  __\[\[ __  __and before  __  __\]\]__

# String Comparison Operators

* The conditions take different forms depending on what you want to compare\.
* String comparison operators:
  * = 		equal
  * \!= 		not equal
  * <		lexically before \(may need to be escaped\)
  * > 		lexically after \(may need to be escaped\)
  * \-z		zero length
  * \-n		not zero length
  * == 		can be used but behaves differently in \[\] versus \[\[\]\]

# Example: String Comparisons

__Create new script __  __ifname\.sh__

* _\#\!/bin/bash_
* if \[\[ $name == "Tom" \]\]; then
  * echo $\{name\}\, you are assigned to Room 12
* elif \[\[ $name == "Harry" \]\]; then
* echo $\{name\}\, please go to Room 3
* elif \[\[ \-z $name \]\]; then
* echo You did not tell me your name
* else
* echo "$\{name\}\, I don’t know where \\
* you are supposed to go"
* fi

# Exercise

* Insert this line below the shebang:
* name=Tom
* Rerun the script\.
* Change the name to an empty string:
  * name=""
  * Rerun the script\.

# Numeric Comparisons

Numeric comparisons are possible:

\-eq 	equal

\-ne 	not equal

\-gt 	greater than

\-ge 	greater than or equal

\-lt 	less than

\-le	less than or equal

a=2

if \[\[ $a \-eq 2 \]\] ; then

echo It is two\.

fi

# Testing File Properties

* This is a very common occurrence in bash scripts so we have a set of operators just for this\.
* The most common operators are:
  * \-e \<file>   :  file exists
  * \-f \<file>   :  file exists and is a regular file
  * \-s \<file>   :  file exists and has length > 0
  * \-d <dir>    : exists and is a directory

# Example: Testing File Properties

* if \[\[ \-d $to\_dir \]\]; then
  * if \[\[ \-f $the\_file \]\]; then
    * cp $the\_file $to\_dir
  * fi
* else
  * mkdir $to\_dir
  * echo “Created $to\_dir”
* fi

# Other Conditional Operators

\!    	not ; negates what follows

\-a 	logical and ; for compound conditionals ;

can use && with \[\[ \]\]

\-o 	logical or ; for compound conditionals

can use || with \[\[ \]\]

# Case Statement

* case  _expression_  in
  * _pattern1_  \)
    * statements ;;
  * _pattern2_  \)
    * statements ;;
  * _pattern3_  \)
    * statements ;;
    * \*\)
    * statements ;;  _\# default statement_
* esac  _\# defines end of case block_

# Example

case $filename in

\*\.c\)

echo "C source file"

;;

\*\.py\)

echo "Python script file"

;;

\*\)  _\#optional\, indicates default_

echo "I don’t know what this file is"

;;

esac

# For Loops

The bash for loop is a little different from the equivalent in C/C\+\+/Fortran \(but is similar to Perl or Python\)

for  _variable_  in  _iterator_

do

commands

done  _\# defines end of for loop_

# Examples

__Create new script __  __forloop\.sh__  __:__

* for i in 1 2 3 4 5 ; do
  * echo "Loop 1: I am in step $i"
* done
* for i in \{1\.\.5\} ; do  \#bash 3\.0 and up
  * echo "Loop 2: I am in step $i"
* done
* for i in 0\{1\.\.9\} \{90\.\.100\} ; do
* echo "Loop 3: I am in step $i"
* done

# “Three-Expression” For Loop

for \(\( EXPR1; EXPR2; EXPR3 \)\)

do

statements

done

__Open __  __forloop\.sh__  __ and add:__

name="file"

IMAX=10

for \(\( i=0 ; i<$\{IMAX\} ; i=i\+2 \)\); do

echo "$\{name\}\.$\{i\}"

done

# While Loop

Iterate through loop as long as \<condition> is evaluated as true

* while \[\[ condition \]\]
* do
  * command
  * command
  * command
* done \# defines end of while loop

One of the commands in the while loop  _must _ update the condition so that it eventually becomes false\.

# break

If you need to exit a loop before its termination condition is reached you can use the break statement\.

* while \[\[ condition \]\]
* do
  * if \[\[ disaster \]\]; then
    * break
  * fi
  * command
  * command
* done

# continue

To skip the commands for an iteration use continue

* for i in iterator
* do
  * if \[\[ condition \]\]
  * then
  * continue \# skips to next iteration
  * fi
  * command\_1
  * command\_2
* done

# Example: Combine while and case

* _while_  plus  _case_
* while \[\[ $\# \-gt 0 \]\]; do
* case "$1" in
  * \-v\)  verbose="on";;
  * \-\*\)  echo >&2 "USAGE: $0 \[\-v\] \[file\]"
    * exit 1;;
  * \*\)   break;;   \# default
* esac
* shift
* done

# Bash Arithmetic

* We said earlier that bash is bad at arithmetic\, but some basic operations can be performed\.
* Expressions to be evaluated numerically must be enclosed in  _double parentheses_ \.
  * x=$\(\(4\+20\)\)
* i=$\(\($x\+1\)\)

It works only with  _integers_ \. If you need more advanced math \(even just fractions\!\) you must use bc\.

# Math meets Bash: bc

* If you really\, really\, really must do math in a bash script\, most of the time you must use bc
* The syntax is very peculiar
  * x=$\(echo "3\*8\+$z" | bc\)

[http://ss64\.com/bash/bc\.html](http://ss64.com/bash/bc.html)

# Command-Line Arguments

Many bash scripts need to read arguments from the command line\.  The arguments are indicated by special variables $0\, $1\, $2\, etc\.

Command lines arguments are separated by whitespaces:

\./Commandline\.sh Hello You

$0 is the name of the command/script itself

The subsequent ones are the command line arguments

If you have a variable number of arguments\, use the shift built\-in to iterate over them in your script\.

The special variable $\# is the number of arguments \(not counting the command name\)

$@ expands to array of all command line arguments

# Example

__Create script __  __commandline\.sh__

* _\#\!/bin/bash _
* USAGE="Usage: $0 arg1 arg2 arg3 \.\.\.argN”
* if \[\[ "$\#" \-eq 0 \]\]; then
* <span style="color:#0070C0">\# no command line arguments</span>
  * echo "$USAGE"
  * exit 1  _\# return to command line_
* fi
* echo "All arguments: $@"
* i=1  _\# counter_
* while \[\[ "$\#" \-gt 0 \]\]; do
  * echo "Argument $\{i\}: $1"
  * shift   _\# move _  _args_  _ 1 position to the left_
  * \(\(i\+\+\)\)  _\# increment counter_
* done

# String Operations

* Bash has a number of built\-in string operations\.
* Concatenation
  * Just write them together \(literals should be quoted\)
  * newstring=$\{string\}"\.ext"
* String length
  * $\{\#string\}
* Extract substring
  * Strings are zero\-indexed\, first character is numbered 0
  * $\{string:pos\}  _\# Extract from _  _pos_  _ to the end_
  * $\{string:pos:len\}  _\# Extract _  _len_  _ characters _
  * _		         starting at _  _pos_

# Clipping Strings

* It is very common in bash scripts to clip off part of a string so it can be remodeled\.
  * Delete shortest match from front of string
  * $\{string\#substring\}
  * Delete longest match from front of string
  * $\{string\#\#substring\}
  * Delete shortest match from back of string
  * $\{string%substring\}
  * Delete longest match from back of string
  * $\{string%%substring\}

# Replacing Portions of a String

* Often a specific portion \(substring\) of a string needs to be replaced\.
  * Replace  _first_  match of substring in string with replacement string
  * $\{string/substring/replacement\}
  * Replace  _all_  matches of substring in string with replacement string
  * $\{string//substring/replacement\}

# String Manipulation Example

Create script strings\.sh

_\#\!/bin/bash_

name="Miller"

echo $\{name\}

<span style="color:#0070C0">\# string length</span>

echo "Your name has $\{\#name\} letters\."

<span style="color:#0070C0">\# clipping string</span>

echo "I turned you into a $\{name%"er"\}\."

<span style="color:#0070C0">\# replacing substring</span>

single\_repl=$\{name/"l"/"\*"\}

echo $\{single\_repl\}

all\_repl=$\{name//"l"/"\*"\}

echo $\{all\_repl\}

# Arrays

* Arrays are zero based so the first index is 0\.
* Initialize arrays with a list enclosed in parentheses\.
* Obtaining the value of an item in an array requires use of $\{\}:
  * arr=\(blue red green\)
* echo $\{arr\[@\]\}  <span style="color:#0070C0">\# All the items in the array</span>
* $\{\#arr\[@\]\}  <span style="color:#0070C0">\# Number of items in the array </span>
* $\{arr\[0\]\}   <span style="color:#0070C0">\# Item zero</span>
* <span style="color:#0070C0">     </span> $\{arr\[1\]\}   <span style="color:#0070C0">\# Item one</span>
* $\{\#arr\[0\]\}  <span style="color:#0070C0">\# Length of item zero</span>

# Array Example

_\!\#/bin/bash_

arr=\(blue red green\)

for \(\( i=0 ; i<$\{\#arr\[@\]\} ; i\+\+ \)\)

do

echo $\{arr\[i\]\}

done

_\#\!/bin/bash_

arr=\(blue red green\)

for color in $\{arr\[@\]\}

do

echo $\{color\}

done

# Common Task: File Archiving

Unix/Linux offers the tar command which can be used to bundle and compress many input files into a single archive file\, a \.tar file\.

__Create \.tar  file__

tar \-czvf file\.tar inputfile1 inputfile2

\-c 	create archive

\-f	archive is a file

\-v	verbose output

\-z	compress

__Unpack \.tar file__

tar –xzvf file\.tar

\-x	extract/unpack archive

Open your terminal window

Change current directory to  ~/scripts

Execute tar czvf script\_archive\.tar \*\.sh

Execute ls\, verify that there is a new file archive\.tar

# Automate Archiving Process

__Task:__  Create a script that creates a single tarball archive file for all files with a certain file extension in a given directory

__Basic Requirements: __

Use command line arguments to allow user to specify

directory with files to be archived

extension of the files to be archived\, e\.g\. “\.sh”

destination directory where the archive file is to be saved

__Advanced Requirements:__

add command line argument to allow user to specify new extension used to rename \(homogenize\) extensions of selected files

Autogenerate tarball archive filename that contains creation timestamp

# More Examples

echo \`date\`  _\# exec date command\, print output_

echo 'This is worth $2'

echo "This is worth $2"

var=2

echo "This is worth $\{var\}"

echo "This is worth \\$$\{var\}"

# Herefiles

* A herefile or here document is a block of text that is dynamically generated when the script is run
* CMD << Delimiter
  * line
  * line
  * Delimiter

# Example

  * \#\!/bin/bash
  * \# 'echo' is fine for printing single line messages\,
  * \# but somewhat problematic for message blocks\.
  * \# A 'cat' here document overcomes this limitation\.
  * cat <<End\-of\-message
  * \-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-
  * This is line 1 of the message\.
  * This is line 2 of the message\.
  * This is line 3 of the message\.
  * This is line 4 of the message\.
  * This is the last line of the message\.
  * \-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-
  * End\-of\-message

# Functions

* function name\(\) \{
  * statement
  * statement
  * statement
  * VALUE=integer
  * return $VALUE
* \}
* the keyword function is optional in newer versions of bash\.  The parentheses are  _always_  left empty\.
* Function definitions must  _precede_  any invocations\.

# Function Arguments

* Function arguments are passed in the  _caller_ \.  In the function they are treated like command\-line options\.
  * \#\!/bin/bash
  * function writeout\(\) \{
  * echo $1
  * \}
  * writeout "Hello World"

# Function Variables

Variables set in the function are global to the script\!

\#\!/bin/bash

myvar="hello"

myfunc\(\) \{

myvar="one two three"

for x in $myvar; do

echo $x

done

\}

myfunc  _\# call function_

echo $myvar $x

# Making Local Variables

We can use the keyword local to avoid clobbering our global variables\.

\#\!bin/bash

myvar="hello"

myfunc\(\) \{

local x

local myvar="one two three"

for x in $myvar ; do

echo $x

done

\}

myfunc echo $myvar $x

# Return Values

* Strictly speaking\, a function returns  _only_  its exit status\.
* The returned value must be an integer
* You can get the value with $?
* e\.g\.
  * myfunc $1 $2
  * result=$?

# Regular Expressions

* Regular expressions are generalizations of the wildcards often used for simple file commands\.
* A regular expression consists of a pattern that attempts to match text\.
* It contains one or more of:
  * A character set
  * An anchor \(to the line position\)
  * Modifiers
* Without an anchor or repeat it will find the  _leftmost_  match and stop\.

# Regex Character Sets and Modifiers

The character set is the set of characters that must be matched literally\.

Modifiers expand the potential matches\.

\* matches any number of repeats of the pattern before it \(note that this is different from its use in the shell\)  _including_  0 matches\.

? Matches 0 or 1 character \(also different from the shell wildcard\)\.

\+ matches one or more\, but not 0\, occurrences\.

\. matches any single character\, except newline

# More Modifiers

* \\ escapes the preceding character\, meaning it is to be used literally and not as a regex symbol\.
* \\ can also indicate nonprinting characters\, e\.g\. \\t for tab\.
* \(\) group the pattern into a subexpression
* | pipe is or
  * \[gray|grey\] equivalent to \[gr\(a|e\)y\]

# Regex: Ranges and Repetition

\[\] enclose a set of characters to be matched\.

\- indicates a range of characters \(must be a subsequence of the ASCII sequence\)

\{n\} where n is a digit\, indicates exactly n repetitions of the preceding character or range\.

\{n\,m\} matches n to m occurrences\.

\{n\,\} matches n or more occurrences\.

# Regex: Anchors and Negation

^ inside square brackets negates what follows it

^ outside square brackets means “beginning of target string”

$ means “end of target string”

\.  Matches “any character in the indicated position”

Note: the “target string” is usually but not always a line in a file\.

# Regex Examples

AABB\* matches

AAB

AABB

AABBBB

But not

AB

ABB

ABBBB

# Regex Examples (Cont.)

\[a\-zA\-Z\] matches any letter

\[^a\-z\] matches anything  _except_  lower\-case letters

\.all matches all\, call\, ball\, mall\, and so forth\.  Also matches shall \(since it contains hall\)\.

Regex patterns are said to be  _greedy_  since they find a match with the most generous interpretation of the pattern\.

# Extensions and Shortcuts

Most shells and languages support some shortcuts:

\\w : \[A\-Za\-z0\-9\_\]

\\s : \[ \\t\\r\\n\]  some flavors have a few more rare whitespace characters

\\d : \[0\-9\]

\\D : ^\\d

^\\W: ^\\w

^\\S: ^\\s

NB \\D\\S is not the same as ^\\d\\s; in fact it matches anything\. ^\\d\\s matches a but not 1

# Grep, Sed and Awk

grep or egrep can be used with regular expressions\.

sed is the  _stream editor_ \.  It is used to script editing of files\.

awk is a programming language to extract data and print reports\.

# grep examples

* grep "regex" filename
  * The quotes are often needed; they make sure it’s interpreted as a regex
  * We assume Gnu grep on Linux \(it is also called egrep\)
  * egrep matches anywhere in the string
* grep ^root /etc/passwd
* grep :$ /etc/passwd

# sed examples

* sed operates on standard input and outputs to stdout unless told otherwise\.  Thus you must redirect\.
* The \-i option will tell it to overwrite the old file\.  This is often a mistake \(if you do this\, be sure to debug the sed command carefully first\)\.
* sed 'command' \< old > new
  * Note hard quotes—best practice is to use them
* sed 's/day/night/' \< old > new
  * Remember\, expressions are  _greedy_ ; day/night here changes Sunday to Sunnight

# awk examples

awk 'pattern \{actions\}' file

Similar to C\, awk "action" lines end with ;

For each record \(line\) awk splits on whitespace\.  The results are stored as fields and can be referenced as $1\, $2\, etc\.  The entire line is $0 and $NF indicates the number of fields in the line\.

awk 'pattern1 \{actions\} pattern2 \\ \{actions\}' file

awk 'Smith' employees\.txt

awk '\{print $2\, $NF;\}' employees\.txt

