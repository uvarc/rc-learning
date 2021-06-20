# Introduction to Programming in C++

# 

# Compilers versus Interpreters

A __compiler__ produces a stand\-alone binary for a given _platform_ \(cpu\+operatingsystem\)\.  The output of a compiler is an _object file_ \, represented with a\.osuffix on Unix\.

A __linker__ takes the\.ofiles and any external _libraries_ and links them into the executable\.  Normally the linker is invoked through the compiler\.

An __interpreter__ interprets line by line\.  The binary that is run is the interpreter itself\.  Programs for interpreters are often called _scripts_ \.  Scripts are frequently cross platform\, but the interpreter itself must be appropriate to the platform\.

# Compiled Languages

* Compiled languages are:
  * Generally stricter about typing \(static typing\) and memory allocation\.
  * Generally produce faster and more efficient runs\.
* Interpreted languages are:
  * Generally looser about typing \(dynamic typing\)\.
  * Generally have dynamically sized data structures built in\.
  * Often run very slowly\.

# Strengths and Weaknesses

C\+\+ \(not C\)

Fortran

Limited mathematical built\-ins

True multidimensional arrays not possible without add\-on libraries \(Blitz\+\+\, Boost\)

Pretty good string handling \(compared to C\)

Straightforward implementation of classes \(but no modules\)

\(2003/8\) Many math function built\-ins

Multidimensional arrays a first\-class data structure\, array operations supported

Does not support true strings yet\, just character arrays

Classes somewhat clunky\.  Modules fill much of this role\.

# Setting Up Your Environment

# Integrated Development Environments

An Integrated Development Environment \(IDE\) combines an editor and a way to compile and run programs in the environment\.

A well\-known IDE for Microsoft Windows isVisualStudio\. Available through Microsoft Store\, not free for individuals\.

Mac OSX usesXcodeas its native IDE\.Xcodeincludes some compilers\, particularly for Swift\, but it can manage several other languages\.  Available at App Store\, free\.

A full\-featured cross\-platform IDE is Eclipse \([www\.eclipse\.org](http://www.eclipse.org/)\)\.  Free\.

A lighter\-weight IDE for Windows and Linux is Code::Blocks \([www\.codeblocks\.org](http://www.codeblocks.org/)\)\.  Free\.

We will use a very lightweight IDE calledGeanysince it is free\, easy to install and use\, and  works on all three platforms\.

# Linux

* For users of the University of Virginia's cluster\, first load a compiler module\.
* module loadgcc
  * brings a newergcc\, g\+\+\, andgfortraninto the current environment
* module loadgeany
* geany&
* Geanyis also available for all popular Linux distributions and can be installed through the distribution's package manager\.

# Windows and Mac

* Geanycan be installed on Windows and Mac through the usual software installation methods\.
* Geanydoes not install a compiler suite\.  This must be performed independently\.
* Macs withXcodeincludegccand g\+\+ but notgfortran\.
* Windows does not include a default compiler suite\, butVisualStudioincludes Microsoft C and C\+\+\.
* Geanycan be downloaded for Mac or Windows starting from its home page
  * www\.geany\.org

# Installing Compilers on Macs

InstallXcodefrom the App Store\.

If you are going to use Fortran\, download a binary for your version of OSX from

https://gcc\.gnu\.org/wiki/GFortranBinaries

# Installing Compilers on Windows

* MinGWprovides a free distribution ofgcc/g\+\+/gfortran
* Executables produced by the standardMinGWpackage will be 32 bits
* Also install MSYS for a minimalist Unix system\.
  * Download from[www\.mingw\.org](http://www.mingw.org/)
  * Run installer
  * Choose packages to install\, then click Apply\.
  * After the installation\, follow instructions for "After Installing" at[http://www\.mingw\.org/wiki/Getting\_Started](http://www.mingw.org/wiki/Getting_Started)
  * Be sure to modify your path environment variable

# MinGW

<img src="img/Introduction to C++0.png" width=500px />

<img src="img/Introduction to C++1.png" width=500px />

<img src="img/Introduction to C++2.png" width=500px />

<img src="img/Introduction to C++3.png" width=500px />

# Environment Variables in Windows

Control Panel\->System and Security\->Advanced system settings\->Environment Variables

Once you open Path\, click New to add to the Path

<img src="img/Introduction to C++4.png" width=412px />

<img src="img/Introduction to C++5.png" width=500px />

# Geany on Windows

<img src="img/Introduction to C++6.png" width=500px />

# Variables

# Variables in C++

Like most programming languages\, C\+\+ is case sensitive\.  VariablesMeanandmeanare different to the compiler\.

Like most compiled languages\, C\+\+ is _statically_  _typed_ \.  All variables must be _declared_ to be of a specific type before they can be used\.  A variable’s type cannot be changed once it is declared\.

C\+\+ is \(nearly\) strongly typed\.  Mixed\-mode expressions are limited and most conversions must be explicit\.

# Numeric Types: Integer

* Integer \(int\)
  * Quantities with no fractional part
  * Represented by sign bit \+ value in _binary_
    * _Remember that computers do not use base 10 internally_
    * Default integers are of size 32 bits
  * Maximum signed integer is is 232\-1
    * unsignedintis a type that covers only nonnegativeints
  * Other types
    * longmay be either 32 or 64 bits depending on compiler/platform
      * Standard requires only that it be at least 32 bits\.
      * Usually 32 bits on Windows\, 64 bits on other platforms now\.
    * shortis 16 bits
    * longlongis a 64\-bit integer \(C\+\+11 standard\, before that an extension\)

# Numeric Types: Single Precision

* Floating point single precisionfloat
  * Sign\, exponent\, mantissa
  * 32 bits
  * IEEE 754 defines representation and operations
  * Approximately 7\-8 decimal digits of precision\, _approximate_ exponent range is 10\-126to 10127

# Numeric Types: Double Precision

* Double precision floating pointdouble
  * Sign\, exponent\, mantissa
  * 64 bits
    * Number of bits NOT a function of the OS type\!  It is specified by the IEEE 754 standard\!
  * Approximately 15\-17 decimal digits of precision\, approximate exponential range 10\-308to 10308

# Non-numeric Types: Boolean

Booleans are represent truth valuebool

Values can betrueorfalse

Internallytrueis1andfalseis0\, but it’s easier for humans to read and remember true/false\.

# Non-numeric Types: Character

* Character char
  * 1 byte \(8 bits\) per single character
* A character has a fixed length that must be declared at compile time\, unless it is treated asallocatable\(more on that later\)\.
* char\[8\]mychar;
* The default length is 1\, however\.
* char letter;

# Non-numeric Types: String

A string is a sequence of characters of variable length\.

Requires adding a header

\#define <string\.h>

The string is a _class_ \, which is a little beyond our scope right now\.  But you can still use basic functions without understanding the class\.

stringstr\, str1\, str2;

str\.size\(\);  // length of string

str1\+str2; // concatenate two strings

str\.substr\(2\,5\); // substring \(counts from 0\)

# Literals

* Literals aka constants
  * Specified values	e\.g\.
  * 3
  * 3\.2
  * 3\.213e0
  * "This is a string"
  * "Isn’t it true?"
  * true
  * Literals have a type but it is determined from the format rather than a declaration\.

# Variable Declarations

Variables are declared by indicating the type followed by a comma\-separated list of variables followed by a semicolon\.

inti\,j\,k;

float x\, y;

# Initializing at Compile Time

Variables can be declared and initialized at the same time:

float x=1\.e\-8\, y=42\.;

inti\,j\,k\,counter=0;

# Pointers and References

* A pointer is a variable that points to a location in memory\.
  * C\+\+ has ways to avoid pointers\, but they still appear regularly in much code\.
* Pointers are declared with \*
* float \*x\, y;
  * x is a pointer\, y is a variable\.
* The value of a pointer is obtained explicitly by the _dereference operator_ &
* y=99\.;
* x=&y; //x now points to location of y
* cout<<x<<" "<<\*x<<" "<<y<<"\\n";
* 0x7ffe577a0494 99 99

# Example

* StartGeany\(or whatever editor you want to use\)\.  Type
* \#include <iostream>
* intmain\(\) \{
* /\* My first program
* Author:  Your Name
* \*/
  * floatx\,y;
  * inti\,j=11;
  * x=1\.0;
  * y=2\.0;
  * i=j\+2;
  * std::cout<<"Reals are "<<x<<" "<<y<<"\\n";
  * std::cout<<"Integers are "<<i<<" "<<j<<"\\n";
  * return 0;
* \}

# const

In compiled languages\, programmers can declare a variable to have a fixed value that cannot be changed\.

In C/C\+\+ this is indicated by theconstattribute\.

constfloat pi=3\.14159;

Attempting to change the value of a variable declared to be a parameter will result in a fatal compiler error\.

# Type Conversions

Most compilers will automatically cast numeric variables to make mixed expressions consistent\.  The variables are promoted according to their rank\.  Lowest to highest the types are integer\, float\, double\, complex\.

Use explicit casting to be clear\, or in circumstances such as argument lists where the compiler will not do it\.

Strings may be cast to numbers and vice versa by astringstream\(this is the "correct" C\+\+ way\)\.

# Examples

Explicit casting among numeric types\, default kind\.

R=\(float\) I;

I=\(int\) R;

D=\(double\)R;

# Character  Numeric

* Stringstreamsare internal string _buffers_ \.
* Convert numeric to character:
* \#include <iostream>
* \#include \<string>
* \#include <sstream>
* using namespacestd;
* intmain\(\) \{
  * string age;
  * intiage;
  * iage=39
  * stringstreamss;
  * ss<<iage;       //loadiageinto buffer
  * age=ss\.str\(\);
* Convert character to numeric
  * age='51'
  * stringstreamss2\(age\);
  * ss2>>iage;
* \}

# Arithmetic Operations

Operators are defined on integers\, floats\, and doubles

\+ \-add subtract

\* /multiply divide

Operator Precedence is:

\(\* /\) \(\+ \-\)

Evaluation is left to right by precedence unless told otherwise with parentheses

# Integer Operators

* In C\+\+ 2/3 is always zero\!  Why?
  * Because 2 and 3 are both integers\.  Nothing will be promoted to a float\, so / is an integer operation that yields an integer result
* Remainder can be obtained from%
  * Use for negatives is uncommon in alllanguages and may not behave as you expect\.

# Boolean Operators

* Negation
  * \!
    * \!flag
* AND
  * &&
* OR
  * ||

# Conditional Operators

* Numeric
    * equals==
    * not equal/=
    * strictly less than<
    * strictly greater than>
    * less than or equal to<=
    * greater than or equal to>=

# Conditional Operator Precedence

>\,>=\,<\,<=outrank==or\!=

==\,\!=outranks&&

&&outranks||

As always\, use parentheses to change grouping or to improve clarity\.

# Exercise

Exercises with conditionals\.

Be sure to declare variables appropriately\.

a=11\.; b=9\.; c=45; n=3;

cout<<boolalpha\<< \(a>b\)<<"\\n";

cout<< \(a<b && c==n\) << "\\n";

cout<< \(a<b || c==n\) << "\\n";

cout\<< \(a>b || c==n && a<b\) <<"\\n";

cout\<<\(a>b || c==n\) && a<b << "\\n"'

boolis\_equal= a==b;

cout<<is\_equal<<"\\n";

# Expressions and Statements

# Expressions in C++

* C\+\+ expressions are much like those of other languages\.
* a\+3\*c
* 8\.0\*\(double\)i\+pow\(v\,3\)
* sqrt\(abs\(a\-b\)\)
* A||B
  * y > 0\.0&&y < 1\.0
  * myfunc\(x\,y\)

# Statements

* Indentation is not required but _should be_ used\!
* Statements are terminated with a semicolon\.
* __Code Blocks__
  * Code blocks are multiple statements that are logically a single statement\.
  * C\+\+ uses curly braces \{\} to enclose blocks\.
  * Two styles\.  Pickoneand be consistent:
    * if \(cond\) \{			if \(cond\)
    * statements;			\{
    * \}				          statements
            * \}

# Hello World

* \#include <iostream>
* using namespacestd;
* intmain \(intargc\, char \*\*argv\) \{
  * cout<<"Hello world\\n";
  * return 0;
* \}

# Exercise

* Write a program that will declare and set variables as indicated and will print the expressions indicated\. You may use your "hello world" program as a base\.
  * x=17\.
  * Xs=11\.
  * num\_1=10
  * num\_2=14
  * cout<<x<<"\\n";
  * cout<<Xs/x<<"\\n";
  * cout<<\(int\)Xs/x<<"\\n";
  * cout<<int\(Xs\)/int\(x\)<<"\\n";
  * cout<<Xs/x \+ x<<"\\n";
  * cout<<Xs/\(x\+x\)<<"\\n";
  * cout<<x/num\_1<<"\\n";
  * cout<<num\_1/num\_2<<"\\n";
  * cout<<num\_2/num\_1<<"\\n";

Declare string variables large enough to hold the indicated strings\. Include the header

\#include \<string>

string title="This is a string";

string subtitle="Another string"

cout<<title\.size\(\)<<"\\n";

stringnewtitle=title\+":"\+subtitle;

cout<<newtitle<<"\\n";

cout<<newtitle\.substr\(1\,3\)<<"\\n";

//Quiz:Change"This" to "That" innewtitle

# C++ Loops and Conditionals

# Conditionals

* else if/else ifandelseare optional
* if \( comparison \) \{
  * code;
  * \}
  * else if \( comparison\) \{
  * more code;
  * \}
  * else  \{
  * yet more code;
  * \}

# SWITCH

* Manyelse ifs can become confusing\.
* switch \(expression\)\{
* caseconstvalue0:
* code;
* break;  //optional
* caseconstvalue1:
  * code;
  * break;
  * caseconstvalue2:
  * code;
  * break;
  * caseconstvalue3:
  * code;
  * break;
  * default :   // Optional\, usually needs break before
  * code;
  * \}
  * where “constvalue” is either something declaredconstor a literal\.  It must be an integer\, or convertible to an integer \(so char is acceptable but not string\)\.

# SWITCH Example

* switch \(chooser\) \{
  * case  \(0\):
    * y=\-x2;
    * break;
  * case \(1\):
    * y=x2\+3\./x1;
    * Break;
  * case default
    * y=0\.;
* \}

# FOR Loop

* forexecutes a fixed number of iterations unless explicitly terminated\.
* for \(inti=l;i<=u;i\+=s\) \{
  * code;
* \}
  * i: Loop variable
  * l: Lower bound
  * u: Upper bound
  * s: Stride\.  Use\+\+ifor a stride of 1\.
* scan be negative\, in which caselmust be greater thanu\. For \-1 use\-\-ior similar\.

# Early Exit

  * break: leave loop
  * breakis able to break out of _only_ the loop level _in which it appears_ \.  It cannot break from an inner loop all the way out of a nested set of loops\.  This is a case wheregotomay be better than the alternatives\.
  * continue: skip rest of loop and go to next iteration\.
  * gotoSyntax \(use sparingly\):
    * gotoLabel;
    * …
    * Label:
      * Code

# WHILE Loops

* while \(\<logical expression>\) \{
  * statement
  * statement
  * …
* \}
  * Remember that your logical expression must become false at some point\.

# Example

intx\, y\, z;

x=\-20;

y=\-10;

while \(x<0 && y<0\) \{

x=10\-y;

y\+=1;

z=0;

\}

z=1;

# Break/Continue

float x=1\.;

do while \(x>0\.0\) \{

x=x\+1\.;

if \(x>=10000\.0\) break;

if \(x<100\.0\) continue;

x\+=20\.0;

\}

# Repeat-Until

do \{

statement;

statement;

…\.

if \(\<logical expression>\) break;

\}

Thewhilealways tests at the _top_ of the loop\.  Thedo…if/breakform can test anywhere\.

# Example

* \#include <iostream>
* using namespacestd;
* intmain\(\) \{
  * intx\, y\, z;
  * x=\-20;
  * y=\-10;
  * while \(x<0 && y<0\) \{
  * x=10\-y;
  * y=y\+1;
  * z=0;
  * \}
  * z=1;
  * cout<<x<<" "<<y<<" "<<z<<"\\n";
  * return 0;
* \}

# Exercise

* Loop from 0 to 20 by increments of 2\.  Make sure that 20 is included\.  Print the loop variable at each iteration\.
* Start a variable n at 1\.  As long as n is less than 121\, do the following:
    * If n is even\, add 3
    * If n is odd\, add 5
    * Print n for each iteration\.  Why do you get the last value?
* Set a float value x=0\. Loop from 1 to N inclusive by 1\.
  * If the loop variable is less than M\, add 11\. to x\.
  * If x > w and x < z\, skip the iteration\.
  * If x > 100\.\, exit the loop\.
  * Experiment with different values for the variables\.  Start with N=50\, M=25\, w=9\.\, z=13\.

# Arrays

# Terminology

A _scalar_ is a single item \(real/float\, integer\, character/string\, complex\, etc\.\)

An _array_ contains data of the __same type__ with each scalar element addressed by _indexing_ into the array\.

An array has one or more _dimensions_ \.  The _bounds_ are the lowest and highest indexes\.  The _rank_ is the number of dimensions\.

C\+\+ does not have arrays as first\-class data types\.  A C\-style array is a block of memory\.  Other options are available in class libraries\.

# C-Style Arrays

Arrays must be declared by type and either by size or by some indication of the number of dimensions\.

float a\[100\];

intM\[10\]\[10\];

If a variable is used\, it must be aconst

constintN=10;

float z\[N\];

The starting index is always 0\, so for a 100\-element array the items are number 0 to 99\.

# Orientation

* Array elements are _adjacent_ in memory \(this is one of their advantages\) and are arranged linearly no matter how many dimensions you declare\. If you declare a 3x2 array the order in memory is
* \(1\,1\)\, \(1\,2\)\, \(2\,1\)\, \(2\,2\)\, \(3\,1\)\, \(3\,2\)
* “Orientation” refers to how the array is stored _in_  _memory_ \, not to any mathematical properties\.
* C\+\+ and most other languages are _row\-major_ oriented\.  Some \(Fortran\,Matlab\, R\) are _column\-major_ oriented\.
* Loop indices should reflect this whenever possible \(when you need loops\)\.
* Move left to right\.
  * A\(i\,j\,k\)loop order isdo for i/for j/for k

# Initializing Arrays in C++

* Arrays can be initialized when created
  * float A\[3\]=\{10\.\,20\.\,30\.\}
* Curly braces are required\.
* Example
* \#include <iostream>
* using namespacestd;
* intmain\(intargc\, char \*\*argv\)\{
  * constintn=5;
  * float A\[n\]=\{10\.\,20\.\,30\.\,40\.\,50\.\};
  * for \(inti=0;i<n;i\+\+\)\{
    * cout<<A\[i\]<<" ";
  * \}
  * cout<< "\\n";
* return 0;\}

# Initializing (Continued)

Elements not explicitly initialized will be set to 0\.

Try it:

In the program on the previous page\, try setting

float A\[n\]=\{\};

Then try

float A\[n\]=\{10\.\,20\.\,30\.\};

with no other changes to the program

# WARNING WARNING WARNING

C\+\+ happily lets you “walk off” your array\.

Most commonly this occurs when you have variables and you end up attempting to access an element outside of the declared size\.

This usually results in a segmentation violation or sometimes garbage results\.

Example: in your previous code change

cout<< A\[i\]<<"";

To

cout<< A\[i\+1\]<<" ";

# Multidimensional Arrays in C++

* Multidimensional arrays are just "arrays of arrays" in C\+\+\.
* They are declared with multiple brackets:
* float A\[2\]\[5\];
* Elements are referenced like
* A\[0\]\[2\]
* A\[i\]\[j\]
* Initialize like
* A=\{\{1\.\,2\.\,3\.\,4\.\,5\.\}\,
    * \{6\.\,7\.\,8\.\,9\.\,10\.\}\};

# C++ Array Properties

The arrays we have discussed are "C style arrays\)

They are just blocks of memory with no added metadata\.

1D arrays are contiguous in memory but higher\-dimensional arrays need not be\.

The name of the array is also a _pointer_ to the address in memory of the first \(zeroth\) element of the array\.

Higher\-dimensional arrays cannot be fully dynamically defined\.

# Passing Arrays to Procedures

We can only pass the _pointer_ to the first element of the array\.

A pointer is a variable that holds the memory location of another variable\.

Array names are really pointers and in C were usually explicitly so\.

Intheprocedure's argument listyoucan declareyour array withoneempty bracket\. Forhigher\-dimensional arrays only the first dimension can be empty; the others must be specified\.

Higher\-dimensional arrays generally are declared and passed as pointers\, but their dimensions _must_ be passed in this case\.

# Example

In main:

float a\[100\];

In the function

floatmyfunc\(float a\[\]\,intlength\)

Invoke the function with

myfunc\(a\, 100\);

More about this when we get to functions\.

# Allocation and the New Operator

Arrays may be sized at runtime\.

intN;

N=30;

float\* A=new float\[N\];

The\*indicates thatAis a _pointer_ to a block offloatvariables\.  We do not have to use it subsequently\, and can still refer toAby index\, e\.g\.A\[2\]\.

# Multidimensional Arrays with New

* We will only discuss 2d arrays here\.
* Two\-dimensional arrays are 1\-d arrays of pointers to an array\.
* intnrows\,ncols;
  * //Setnrows\,ncolsby some means
* float \*\*A;
* A=new float\*\[nrows\];
* for \(inti=0;i<nrows;\+\+i\) \{
  * A\[i\]=new float\[ncols\];
* \}

# C++ Containers

* A _container_ is a data structure that can contain other types\.
* C\+\+ implements most containers as _templates_ \.
  * This is beyond our scope right now\.
* If you need an array with more functionality there is an array container\.
  * But it's still fixed size and 1D
* Thevectorcontainer can be sized dynamically\.
* Other options includeboost\(most popular\)\,blitz\+\+libraries\.

# Boost arrays

Boost is a popular library of extensions and templates for C\+\+

One of its containers is themulti\_array

To use it\, you must install the library \(or use a computer where it has been installed for you\)

Templates in general\, and boost arrays in particular\, can be slow\.  The example on the next two slides tests this\.

# Boost arrays and C arrays

\#include <ctime>

\#include \<boost/multi\_array\.hpp>

using namespacestd;

intmain\(intargc\, char\*argv\[\]\)

\{

intnrows;

intncols;

//  Iterations are to make the time measurable

constintITERATIONS = 1000;

time\_tstartTime\,endTime;

// Set the array dimensions

nrows=500;

ncols=500;

// Create the boost array

typedefboost::multi\_array\<double\, 2> Array2D;

Array2DboostArray\(boost::extents\[nrows\]\[ncols\]\);

// CreatetheCarray

double \*\*C\_Array;

//

//\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-Measure boost\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-

startTime= time\(NULL\);

for \(inti = 0; i < ITERATIONS; \+\+i\) \{

for \(intx = 0; x <nrows; \+\+x\) \{

for \(inty = 0; y <ncols; \+\+y\) \{

boostArray\[x\]\[y\] = 2\.345;

\}

\}

\}

endTime= time\(NULL\);

cout<<"\[Boost\] Elapsed time: "<<\(endTime\-startTime\)/1000\.0<<" sec\\n";

//\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-Measure native\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-

C\_Array= new double\* \[nrows\];

for \(inti=0;i<nrows;\+\+i\) \{

C\_Array\[i\]=new double\[ncols\];

\}

startTime= time\(NULL\);

for \(inti = 0; i < ITERATIONS; \+\+i\) \{

for \(intx = 0; x <nrows; \+\+x\) \{

for \(inty = 0; y <ncols; \+\+y\) \{

C\_Array\[x\]\[y\] = 2\.345;

\}

\}

\}

endTime= time\(NULL\);

cout<<"\[C style\] Elapsed time: "<<\(endTime\-startTime\)/1000\.0<<" sec\\n";

return 0;

\}

# Exercise

* Write a program to:
* Declare an integer array of 10 elements
* In your program:
  * Print the size of the array
  * Change the fourth element to 11
* Declare a real array of rank 2 \(two dimensions\) and allocate it with new\.  Allocate the array and set each element to the sum of its row and column indices\.

# C++ input/output

# Stream IO

* Stream IO allows the compiler to format the data\.
* Input
* Header required
* \#include <iostream>
  * Read from standard input\.   Requires whitespace separation\.
  * std::cin>> var1 >> var2 >> var3
  * Write to standard output\.\\nis the end of line marker\.
  * std::cout<<var1<<" "<<var2<<" "<<var3<<"\\n"
  * std::cout<<var1<<" "<<var2<<" "<<var3<<std::endl

# Reading from the Command Line

* We can read strings only\.  You must convert if necessary to a numerical type using _string streams_ \.
* \#include <iostream>
* \#include <sstream>
* using namespacestd;
* intmain\(intargc\, char \*\*argv\) \{
  * float value;
  * if \(argc>1\) \{
    * stringstreaminputValue;
    * inputValue<<argv\[1\];
    * inputValue>>value;
  * \}
  * return 0;
* \}

# Formatted IO

Formatted input is rarely needed\.

Formatted output permits greater control over the appearance of output\.  Compilers tend to let stream output sprawl\.

Formatted output also allows programmer control over the number of decimal digits printed for floating\-point numbers\.

# Manipulators

* C\+\+ uses _manipulators_ to modify the output of the stream operatorscinandcout\.
* A few base manipulators:
* Output
  * endlflushes the output and inserts newline
  * endsoutputs null character \(C string terminator\)
  * boolalphatrue/false printed for Booleans
  * left/right/internalleft/right/internal for fillers\.
* Input
  * wsreads and ignores whitespace
  * skipws/noskipwsignore/read initial whitespace as characters

# Header iomanip

\#include <iomanip>

These manipulators stay in effect in a given output stream until cancelled\.

setw\(n\)Set width output quantity will occupy

setprecision\(n\)Set number of places printed for floating\-point numbers

fixed/scientificFixed\-point format or scientific notation format

setfill\(c\)Set a filler characterc

setbase\(n\)Output in basen\(options are 8\, 10\, or 16\, or 0 which reverts to default of decimal\)\.

# Example

* \#include <iostream>
* \#include <iomanip>
* using namespacestd;
* intmain\(\) \{
  * float x=\.00001\, y=17\.\, z=10000\.;
  * cout<<setprecision\(16\)<<z/y<<"\\n";
  * cout<<setw\(20\)<<setfill\('\*'\)<<left<<z<<"\\n";
  * cout<<scientific<<x<<" "<<z<<"\\n";
  * cout<<scientific<<x<<" "<<fixed<<z<<"\\n";
* \}

# Exercise

* Write a program that computes pi using a trig identity such asp=4\*atan\(1\)\. Remember
* \#include <cmath>
* To switch between float and double easily\, use
  * typedeffloat real;
* Switch "float" to "double" to change\.
* Declare variables as
  * real x;
* Using single precision\, print pi in
  * Scientificnotation
  * Scientific notation with 8 decimal places
* Repeat for double precision\, print scientific notation to 12 places

In an “infinite” while loop:

Request an integer from the user without advancing to a new line\, e\.g\.

“Please enter an integer:” \<then read integer>

If the integer is 1\, print “zebra”\.  If it is 2\, print “kangaroo”\.  If it is anything else except for zero\, print “not found”\.  If it is 0\, exit the loop\.

# C++ file io

# File Streams

Standard streams are automatically opened\.  Other files must be opened explicitly\.

Files can be input streams \(ifstream\)\, output streams \(ofstream\)\, or either/both \(fstream\)\.

\#include <fstream>

or

\#include <ofstream>

\#include <ifstream>

as needed\.

# Open

* First a stream object must be declared\.
* ifstreaminput;
* Then the stream can be attached to a named file
* input\.open\(inFileName\);
  * This assumes the file exists and is openedforreading only\.
* For output use
* ofstreamoutput;
* output\.open\(outFileName\);
* This file will be emptied if it exists or created if it does not exist\, and will be opened in write\-only mode\.

# Modifiers

We can control the characteristics of the file with modifiers

ios::inOpen for input \(read\)\. Default forifstream\.

ios::outOpen for output \(write\)\. Default forofstream\.

ios::binaryOpen as binary \(not text\)

ios::appAppend

ios::truncIf file exists\, overwrite \(default forofstream\)

Use a pipe|to combine them

ofstream\.open\("myfile\.dat"\,ios::binary |ios::app\);

# Inquiring

All inquiry methods return abool\(Boolean\)\.

To check whether a file is open

infile\.is\_open\(\)

To check whether a file opened for reading is at the end

infile\.eof\(\)

Generic testing

mystream\.good\(\)

# Close

Much of the time\, it is not necessary to close a file explicitly\.  Files are automatically closed when execution terminates\.

If many files are opened\, it is good practice to close them before the end of the run\.

mystream\.close\(\);

# REWIND

An open unit can be rewound\.  This places the _file pointer_ back to the beginning of the file\.

The default is to rewind a file automatically when it is closed\.

These are C\-style functions and are in<cstdio>

rewind\(mystream\)

You can also seek to position 0

fseek\(mystream\,0\,SEEK\_SET\)

rewindclears the end\-of\-file and error indicators\, whereasfseekdoes not\.

# Reading from a File

* Frequently we usegetline
* Example
  * string line;
  * //note implicit open
  * ifstreammystream\("datafile\.txt"\);
  * if \(mystream\.is\_open\(\)\) \{
    * while \(getline\(mystream\,line\)\) \{
      * //do something with line
    * \}
  * \}
  * else \{
    * cout<<"Unable to open file";

# getline

* Getline'sname is a little misleading\.
* Getlineactually reads to a delimiter\.  The default delimiter is newline\\n\.
* getline\(istream\,string\,chardelim\)
* getline\(istream\,string\)
* The delimiter character is discarded from the string\.
* Example:
  * cout<<"Enter your name:";
  * getline\(cin\,name\);

# Reading a CSV file

* We often need to read files where each line contains several fields separated by a comma or other delimiter\.  Example: read four values from each line for 200 lines\, ignoring the second column values\.
  * constintnobs=200;
  * float bf\[nobs\]\,wt\[nobs\]\,ht\[nobs\];
  * string line;
  * ifstreamfin\("datafile\.txt"\);
  * if \(fin\.is\_open\(\)\) \{
    * while \(getline\(fin\,line\)\) \{
      * stringstreamlineStream\(line\);
      * string \*linevals=new string\[4\];
      * intindex=0;
      * while \(getline\(lineStream\,linevals\[index\]\,'\,'\) \) \{
      * \+\+index;
      * \}
      * stringstreamssbf\,sswt\,ssht;
      * ssbf<<linevals\[0\];
      * ssbf>>bf\[lineCount\];
      * sswt<<linevals\[2\];
      * sswt>>wt\[lineCount\];
      * ssht<<linevals\[3\];
      * ssht>>ht\[lineCount\];
      * lineCount\+\+;
      * \}
  * \}
  * else \{
    * cout<<"Unable to open file";
    * return 1;
  * \}

# Writing to a File

* Write to a file much like to a standard stream\.
* ofstreamout\("outfile\.txt"\);
* out<<"column1\,column2\,column3\\n";
* for \(inti=0;i<nlines;\+\+i\) \{
  * out<<var1\[i\]<<"\,"<<var2\[i\]<<"\,"<<var3\[i\]<<"\\n';
* \}

# Exercise

Write a program that creates a file mydata\.txt containing four rows consisting of

1\, 2\, 3

4\, 5\, 6

7\, 8\, 9

10\, 11\, 12

Rewind the file and read the data back\.  Write a loop to add 1 to each value and print each row to the console\.

# Subprograms

# What is a Subprogram

* A subprogram is a self\-contained \(but not standalone\) program unit\.  It performs a specific task\, usually by accepting _parameters_ and returning a result to the unit that invokes \(calls\) it\.
* Subprograms are essential to good code practice\.  Among other benefits\, they are
  * Reusable\.  They can be called anywhere the task is to be performed\.
  * Easier to test and debug than a large\, catch\-all unit\.
  * Effective at reducing errors such as cut and paste mistakes\.

# Functions and Subroutines

Functions take any number \(up to compiler limits\) of arguments and return one item\.  This item can be a compound type\.

Functions must be declared to a type like variables\.

Subroutines take any number of arguments \(up to the compiler limit\) and return any number of arguments\.  All communication is through the argument list\.

Strictly speaking\, all subprograms in C\+\+ are functions\, but the ability to declare avoidreturn "type" means some are effectively subroutines\. Subroutines communicate only through their parameter list\.

In C/C\+\+ either the function or its _prototype_ must appear before any invocation\.

# Functions

The return value is indicated by thereturnstatement\.

\<type>myfunc\(\<type> param1\,\<type>  	param2\,\<type> param3\,\<type> param4\)\{

statements

returnaResult;

\}

# Invoking Functions

* Function
  * Invoke by its name
  * x=myfunc\(z\,w\)
  * y=c\*afunc\(z\,w\)
  * A function is just like a variable except it cannot be an _lvalue_ \(appear on the left\-hand side of =\)

# Passing by Value or by Reference

* Most parameters in C\+\+ functions are passed by _value_ \.  The compiler makes a copy and places it into the corresponding function variable\.
* C\+\+ can pass by _reference_ \(more easily than C\)\. This means that the subprogram receives a pointer to the location in memory of the variable\.
* When passing by reference\, if that argument is changed by the subprogram it will be changed in the caller as well\.  This is a __side effect__ \.
* Subroutines operate _entirely_ by side effects\.
  * Sometimes this is not called a “side effect” when it is intentional\, only when it is unintentional\.

# "Subroutines" in C++

The & indicates we are passing by reference\. This is the value that will be modified in this example\.

voidmysub\(\<type> param1\,\<type>   	param2\,\<type> &param3\)\{

statements

\}

Invoke with its name

mysub\(param1\, param2\, param3\);

# Exercise

1\. Write a function that computes Euclidean distance between pointsx1\,y1andx2\,y2\. Include the<cmath>header to get thesqrtintrinsic that you should use\.

\#include <cmath>

Write the main program to call this function for

x1=\-1\, y1=2\, x2=3\, y2=5

x1=11\,y1=4\, x2=7\, y2=9

2\. Given two pointsx1\,y1andx2\,y2\,write a subroutine to determine which is closer to a third pointx3\,y3\.  It should print a message\.  You can pass in the points and call the Euclidean distance function from the subroutine\, or you can pass in the two distances\.  \(The former would be better programming but if you feel uncertain please go ahead and compute distances separately for now\.\)  Test with

x3=10\, y3=5

# Passing Arrays to Subprograms

One\-dimensional arrays may be passed as pointers or with empty square brackets \[\]\.  The size must be passed as well\.

float mean\(float A\[\]\,intn\);

myMean=mean\(A\,n\);

This is equivalent to

float mean\(float \*A\,intn\);

C\-style arrays are always passed by reference\.

Containers such as vectors may be passed either by copying or by reference\.

# Examples

\#include <iostream>

using namespacestd;

float mean\(float A\[\]\,intn\)\{

float sum=0;

for \(inti=0;i<n;\+\+i\)\{

sum\+=A\[i\];

\}

return sum/\(float\)n;

\}

float mean2d\(float \*\*A\,intn\,intm\)\{

float sum=0;

for \(inti=0;i<n;\+\+i\) \{

for \(intj=0;j<m;\+\+j\) \{

sum\+=A\[i\]\[j\];

\}

\}

return sum/\(float\)\(n\*m\);

\}

# Examples (Continued)

intmain\(intargc\, char \*\*argv\) \{

intn=6\, m=4;

float \*A=new float\[n\];

float \*\*B=new float\*\[n\];

for \(inti=0;i<n;\+\+i\) \{

B\[i\]=new float\[m\];

\}

for \(inti=0;i<n;\+\+i\) \{

A\[i\]=i\+1;

for \(intj=0;j<m;\+\+j\) \{

B\[i\]\[j\]=i\+j\+2;

\}

\}

floatmymean=mean\(A\,n\);

cout<<mymean<<"\\n";

float mymean2d=mean2d\(B\,n\,m\);

cout<<mymean2d<<"\\n";

return 0;

\}

# Local Arrays

Arrays that are local to a subprogram may be sized using an integer passed to the subprogram\.

Local array memory __must__ be released in the subprogram or a _memory leak_ will result\.

Wrong:

floatnewmean\(float A\[\]\,intn\)\{

float \*B=new float\[n\];

float sum=0;

for \(inti=0;i<n;\+\+i\)\{

B\[i\]=A\[i\]\+2;

sum\+=B\[i\];

\}

return sum/\(float\)n;

\}

# Delete Operator

* Thedeleteoperator releases memory allocated bynew\.
* In principle\, eachnewshould have a correspondingdelete\.  In main programs\, however\, the memory will be automatically released when the program ends\. Best practice is to always usedelete\, however\.
* It is essential to pairdeletewithnewin subprograms\.
* floatnewmean\(float A\[\]\,intn\)\{
* float \*B=new float\[n\];
* float sum=0;
* for \(inti=0;i<n;\+\+i\)\{
* B\[i\]=A\[i\]\+2;
* sum\+=B\[i\];
* \}
    * delete \[\] B;  //frees all memory associated w/ B
* return sum/\(float\)n;
* \}

# Prototypes

Traditionally\, the entire function bodies were placed at the top of the file where they were used\.

Modern practice in C\+\+ is to write prototypes separate from bodies\.

Prototypes enable the compiler to check that the _number_ and _type_ of the argument list in invocations agrees with the declared parameter list\.

Prototypes are frequently collected into files ending in\.h\(header files\) with function bodies in a corresponding\.cxx \(or\.cpp\)file\.  The prototypes are the _interface_ and the bodies are the _implementation_ \.

Interface\+implementationmakes it easy to reuse the code\.

# Example

Note: in prototypes we need only specify number and type of the parameters\.

means\.h

float mean\(float \*\,int\);

float mean2d\(float \*\*\,int\,int\);

means\.cxx

float mean\(float A\[\]\,intn\)\{

float sum=0;

for \(inti=0;i<n;\+\+i\)\{

sum\+=A\[i\];

\}

return sum/\(float\)n;

\}

float mean2d\(float \*\*A\,intn\,intm\)\{

float sum=0;

for \(inti=0;i<n;\+\+i\) \{

for \(intj=0;j<m;\+\+j\) \{

sum\+=A\[i\]\[j\];

\}

\}

return sum/\(float\)\(n\*m\);

\}

# Using Prototypes

Headers not in the system are usually specified in quotes rather than angle brackets\.

\#include <iostream>

\#include "means\.h"

using namespacestd;

intmain\(intargc\, char \*\*argv\) \{

intn=6\, m=4;

float \*A=new float\[n\];

float \*\*B=new float\*\[n\];

for \(inti=0;i<n;\+\+i\) \{

B\[i\]=new float\[m\];

\}

for \(inti=0;i<n;\+\+i\) \{

A\[i\]=i\+1;

for \(intj=0;j<m;\+\+j\) \{

B\[i\]\[j\]=i\+j\+2;

\}

\}

floatmymean=mean\(A\,n\);

cout<<mymean<<"\\n";

float mymean2d=mean2d\(B\,n\,m\);

cout<<mymean2d<<"\\n";

return 0;

\}

# Compiling and Linking

We now need more than one file to build the executable\.

Under Linux we can compile with

g\+\+ \-c means\.cxx

g\+\+ \-c main\.cxx

g\+\+ \-o meansmain\.omeans\.o

# Exercise

Correct your previous exercise with Euclidean distance function and subroutine to use prototypes\.  You may use a single file for this exercise \(prototypes at the top followingincludeandusinglines\, bodies after main\)\.

# DeFAUlT arguments

# Default Arguments

Subprogram may take default \(optional\) arguments\.   Such arguments need not be passed\.  If they are passed\, they take on the passed value\. They are declared by specifying a default value\.

intmyfunc\(float x\, floaty\,floatz=0\.\,float w=1\.\);

or in a prototype

intmyfunc\(float\, float\, 	float=0\.\,float=1\.\);

Arguments that are not optional are _positional_ and their order matters\. Positional arguments must precede all optional arguments\.

# Using Default Arguments

The call to the previously\-defined subroutine could be

n=myfunc\(a\,b\)

in which case c and d would have their default values\.  The call could also be

m=myfunc\(a\,b\,c\)

or

l=myfunc\(a\,b\,c\,d\)

depending on how many of the default arguments needed to be passed\.

Note: C\+\+ does not support named \(keyword\) arguments\, unlike Python\, Fortran\, and some other languages\.

# SCOPE

# Variable Scope

In C\+\+\, scope is defined by the _code block_ \.  Code blocks are enclosed in curly braces \{\}

A scope unit may have a variable namedx\, and a function may also have a variable namedx\, and ifxis not an argument to the function then it will be distinct from thexin the calling unit\.

float x=20\.

float z=sub\(x\)

etc\.

float sub\(float y\)\{

float x=10\.

float y=30\.

\}

# Code Block Examples

* Loops
* for \(inti=0;i<4;\+\+i\) \{
* cout<< i << "\\n";
* \}
* cout<< i << "\\n";
* Results in:warning: name lookup of ‘i’ changed for ISO ‘for’ scoping \[\-fpermissive\]
* Free\-standing blocks
* j=12;
* \{j=13;
  * cout<<j<<"\\n";
* \}
* cout<<j<<"\\n";

# Compilers, Linkers, and Make

# Building an Executable

The compiler first produces an _object file_ for each _source file_ \.  In Unix these end in\.o

Object files are binary \(machine language\) but cannot be executed\.  They must be linked into an executable\.

If not told otherwise a compiler will attempt to compile and link the source file\(s\) it is instructed to compile\.

For Unix compilers the\-coption suppresses linking\.  The compiler must then be run again to build the executable from the object files\.

The option\-ois used to name the binary something other thana\.out

# Linkers and Libraries

When the executable is created any external libraries must also be linked\.

The compiler will search a standard path for libraries\.  On Unix this is typically/usr/lib\, /usr/lib64\, /usr/local/lib\, /lib

If you have others you must give the compiler the path\.\-Lfollowed by a path works\, then the libraries must be namedlibfoo\.aorlibfoo\.soand it is referenced\-lfoo

Example:

gfortran–omycode–L/usr/lib64/foolibmymain\.omysub\.o\-lfoo

# Make

makeis a tool to manage builds\, especially with multiple files\.

It has a rigid and peculiar syntax\.

It will look for amakefilefirst\, followed byMakefile\(on case\-sensitive systems\)\.

Themakefiledefines one or more _targets_ \.  The target is the product of one or more _rules_ \.

The target is defined with a colon following its name\.  If there are _dependencies_ those follow the colon\.

Dependencies are other files that are required to create the current target\.

# Targets and Rules

Example:

myexec:main\.omodule\.o

\<tab>gfortran\-omyexecmain\.omodule\.o

The tab is _required_ in the rule\.  Don’t ask why\.

Macros \(automatic targets\) for rules:

$@the file name of the current target

$<the name of the first prerequisite

# Variables and Comments

We can define variables inmakefiles

F90=gfortran

CXX=g\+\+

We then refer to them as$\(F90\)\,$\(CXX\)\, etc\.

Common variables:F90\, CC\, CXX\, FFLAGS\, F90FLAGS\, CFLAGS\, CXXFLAGS\, CPPFLAGS\(for the preprocessor\)\, LDFLAGS

# Suffix Rules

* If all \.cxx \(or \.cc or whatever\) files are to be compiled the same way\, we can write a _suffix rule_ to handle them\.
* It uses a _phony target_ called \.SUFFIXES\.
* \.SUFFIXES: \.cxx \.o
  * $\(CXX\) \-c $\(CXXFLAGS\) –c $<

# Pattern Rules

* An extension by Gnu make \(gmake\)\, but nearly everymakeisgmakenow\.
* Similar to suffix rules\.
* Useful for Fortran 90\+:
* %\.mod: %\.o
* Pattern for creating the \.o:
* %\.o: %\.f90
  * $\(F90\) $\(F90FLAGS\) \-c $<

# Example

PROG =bmi

SRCS =  bmi\.cxx bmistats\.cxx stats\.cxx

OBJS =bmi\.obmistats\.ostats\.o

LIBS =

CC =gcc

CXX = g\+\+

CFLAGS = \-O

CXXFLAGS = \-O \-std=c\+\+11

LDFLAGS =

all: $\(PROG\)

$\(PROG\): $\(OBJS\)

$\(CXX\) $\(LDFLAGS\) \-o $@ $\(OBJS\) $\(LIBS\)

\.PHONY: clean

clean:

rm\-f $\(PROG\) $\(OBJS\) \*\.mod

\.SUFFIXES: $\(SUFFIXES\) \.c \.cpp\.cxx

\.c\.o:

$\(CC\) $\(CFLAGS\) \-c $<

\.cpp\.o\.cxx\.o:

$\(CXX\) $\(CXXFLAGS\) \-c $<

bmi\.o: bmi\.cxxstats\.hbmistats\.h

bmistats\.o: bmistats\.cxx

stats\.o: stats\.cxx

# abstract types

# Derived Types

* In C\+\+ abstract types are called _structs_ \.
* The syntax is extremely simple \(ptypestands for a primitive type\)
* structmytype\{
  * <ptype> var1;
  * <ptype> var2;
* \};
* Example
* structEmployee \{
  * string name\, department;
  * intID;
  * float salary;
* \};
* Each variable belonging to astructis called a _member_ \.
* The variables declared as astructare often called _instances_ of thatstruct\.
* Note: it is customary for the name of astruct\(or class\) to be capitalized\, or to use "camel case\."

# Declaring Types and Accessing Fields

Employeefred\, bill\,susan;

To access the fields of the type use the name of the type\, a decimal point as a separator\, and the name of the field\.

fred\.name="Frederick Jones";

fred\.ID=1234;

fred\.department="Accounting";

fred\.salary=75200\.00;

# Structs in Structs

* Structmembers may be instances of otherstructs\.
* structAddress \{
* stringstreetAddress;
* string city\, state;
* intzipCode;
* \};
* structEmployee \{
  * string name\, department;
  * intID;
  * float salary;
  * AddresshomeAddress;
* \};

# The Arrow Operator

As for other types\, variables can be declared pointer tostruct

Employee \*jane;

This is particularly common when passingstruct\(and class\) instances to functions\, to avoid a copy\.

When using a pointer\, the\.field separator is replaced with the _arrow operator_

jane\->name="Jane Smith"

# Vectors

_Containers_ are data structures that can be filled with any type \(or at least multiple ones\)\.  Several are available but here we will only discuss the __vector__ \.

Using a vector requires including its header

\#include \<vector>

A vector is a _template_ so it must be told what type it is going to be using\.

std::vector\<float> V;

std::can be omitted ifusing namespacestd\(but avoid theusingstatement in\.hfiles\)\.

We'll assuming thestdnamespace for the notes\.

# Initializing Vectors

* Vectors are similar to one\-dimensional arrays
  * They represent an ordered sequence of elements
  * Elements are accessed by integers 0…N\-1 \(for size N\)
* But unlike arrays\, vectors are dynamic\.
  * It's possible to enlarge and shrink them\.
* Initializing vectors
  * Loops \(like an array\)
    * vector\<float> V\(N\);
    * for \(inti=0;i<N;\+\+i\) \{
      * V\[i\]=\(float\) i;
    * \}
  * Dynamic Sizing
    * vector \<float> V=\{\};
    * for \(inti=0;i<N;\+\+i\) \{
      * V\.push\_back\(i\);   //appends i at the end of V
  * \}
  * Initializer List \(C\+\+11\)
    * vector\<float> V=\{1\.\, 2\.\, 3\.\, 4\.\, 5\.\, 6\.\}

# Useful Vector Methods

* For a vector V:
  * V\.push\_back\(item\)
    * Append item to V
  * V\.at\(index\)
    * Access \[index\] with bounds checking \(\[index\] does no checking\)
  * V\.start\(\)
    * Starting point for iterator
  * V\.end\(\)
    * End point \(beyond last element\) of iterator
  * V\.size\(\)
    * Number of elements
  * V\.clear\(\)
    * Empty V and make it size 0

# Vectors and Structs

* Vectors can be members ofstructs
* structData \{
  * intnobs;
  * vector\<float>obs;
* \}
* Vector elements can bestructinstances
* vector\<Data>dataList;

# Example

This struct encapsulates a set of observations for birds denoted by their common name\.

structbirdData\{

//Input values\.

stringcommonName;

vector\<float> observations;

\};

# classes

# Object-Oriented Programming

An __object__ is a data structure which has associated _data_ \(variables\) and _behaviors_ \(subprograms\)\.

Objects work on their own data\, communicating with outside units through an interface\.

Objects _encapsulate_ related concepts and keep them unified\.

In most object\-oriented programming languages\, objects are represented by _classes_ \.

# OOP Terminology

* An _instance_ of a type or class is a variable of that type/class\.
* MyclassA\, B;
    * AandBare instances ofMyclass
* A variable that is a member of the type/class is often called an _attribute_ \.
* A _method_ is a subprogram that is a member of a class\.
* An invocation of a method is often called a _message_ \.

# Data Hiding

One of the main purposes of OOP is to prevent outside units from accessing members in an uncontrolled way\.

Making a member _public_ “exposes” it and allows anything that uses the class direct access to the member\.

Typically\, attributes are _private_ and methods are public\.

Methods can be written to obtain or change the value of an attribute\.

# Access Categories

* public
  * Accessible directly through an object
  * myobj\.var1
* private
  * Accessible only within the class or to "friend" classes\.  Must be communicated outside the class through anaccessor\("getter"\) and changed through amutator\("setter"\)\.  Default for a C\+\+ class member\.
* protected
  * private within the class\, accessible to "friend" classes and to descendant classes\.

# Classes in C++

* We define a class with the keyword class
* classMyclass\{
  * public:
    * double var1;
    * double var2;
    * intival;
* \};
* Note: no methods in this class and all attributes are public\.  So it's equivalent to astruct\.

# Methods

* The class definition contains only the prototypes of the methods\.
* classMyClass\{
  * public:
    * double var1\, var2;
    * intvar3;
    * double function1\(double x\, double y\);
    * double function2\(double x\);
* \};

* The methods are defined with class::function
* doubleMyClass::function1\(double x\, 						double y\) \{
  * returnx\+y;
* \}
* doubleMyClass::function2\(double x\) \{
* var1=x\*var2;
  * return;
* \}

# Constructors and Destructors

* A class always has a _constructor_ and a _destructor_ \.  If you don't write them the compiler will try to do it for you\.
* The constructor is automatically called when an object \(instance\) of the class is created\.
  * If you declare a variable of the type this calls the constructor
  * If you declare a variable of type pointer\-to\-class the constructor is called by the new operator\.
* The destructor is called when the object is released\.
* The constructor has the same name as the class\.
* The destructor has the same name as the class but preceded by~
* Constructors usually must be public\.

* classMyClass\{
  * public:
    * double var1\, var2;
    * intvar3;
    * MyClass\(double v1\, double v2\,intv3\);
    * ~MyClass\(\); // destructors never have arguments
    * double function1\(double x\, double y\);
    * double function2\(double x\);
* \};

# Methods

* MyClass::MyClass\(double v1\, double v2\,intv3\) \{
* var1=v1; var2=v2; var3=v3;
* MyClass::~MyClass\(\)\{
* //Not much to do in this example
* \}
* doubleMyClass::function1\(double x\, double y\) \{
  * returnx\+y;
* \}
* doubleMyClass::function2\(double x\) \{
* var1=x\*var2;
  * return;
* \}

# Example

* /\*
* \*testclass\.cxx\*
* \*/
* classMyClass\{
  * public:
  * double var1\, var2;
* intvar3;
* MyClass\(\);
* ~MyClass\(\);
  * private:
    * doubleprivatevar;
* \};
* MyClass::MyClass\(double var1\, double var2\,intvar3\)\{code\}
* MyClass::~MyClass\(\)\{\}
* \#include <iostream>
* intmain\(intargc\, char \*\*argv\)\{
  * MyClassmytest;
  * mytest\.var1=11\.; mytest\.var2=25\.; mytest\.var3=5;
  * mytest\.privatevar=13\.;  ILLEGAL
* return 0;\}

# Correct (and squished)

/\*

\*testclass\.cxx

\*

\*/

classMyClass\{

public:

double var1\, var2;

intvar3;

MyClass\(\);

~MyClass\(\);

voidset\_privatevar\(double value\);

doubleget\_privatevar\(\);

private:

doubleprivatevar;	\};

MyClass::MyClass\(double v1\, double v2\,intv3\)\{var1=v1;var2=v2;var3=v3;\}

MyClass::~MyClass\(\)\{\}

voidMyClass::set\_privatevar\(double value\) \{privatevar=value;\}

doubleMyClass::get\_privatevar\(\)\{returnprivatevar;\}

\#include <iostream>

using namespacestd;

intmain\(intargc\, char \*\*argv\)\{

MyClassmytest;

mytest\.var1=11\.; mytest\.var2=25\.; mytest\.var3=5;

mytest\.set\_privatevar\(13\.\);

cout<<mytest\.get\_privatevar\(\)<<"\\n";

return 0;\}

# this

C\+\+ does not pass the instance variableexplicitly \(but it is there\)\.

If you need access to it in a method\, use thethisparameter\.

thisis a pointer so requires the arrow operator\.

One example is using the same variable name as an argument and anattribute\.

Myclass::Myclass\(x\,y\,z\) \{

this\->x=x;

this\->y=y;

this\->z=z;

\}

# Members in Methods

If you change a class member variable \(attribute\) in a method do not return that variable\.

Methods that do nothing but set one or more attributes \(mutators\) do not return anything\.

voidMyclass::setx\(x\) \{

this\->x=x;

return;

\}

Anything delivered outside the instance is returned\.  E\.g\. accessors

floatMyclass::getx\(\)\{

return x;

\}

# Exercise

* Write a class Atom that contains the following attributes:
  * Element symbol
  * Element name
  * Atomic mass
  * Atomic number
* The methods should be
  * Constructor to set attributes
  * Compute and return the number of neutrons from the mass and number \(n=mass\-number\)

# Inheritance and Polymorphism

Classes can inherit from parent classes\.

New classes are called _derived classes_ or _child_ classes\.

Multiple parents are allowed\, but this is generally discouraged\.

Attributes are not inherited if they are declaredprivate\.  They must be public to be transmitted\.

Constructors are a little more complicated with inheritance\.

We will show just one simple example to illustrate\.

# Attribute Inheritance

The child type inherits all the attributes of its parent\.

Childbilly;

The child inherits the constructor andaccessorfrom the parent\.

Butagedoes not refer back to the parent\, since that variable occurs only in the child\.

# Example

\#include <iostream>

using namespacestd;

class Parent \{

protected:

intmyID;

string name;

public:

Parent\(string name\,intmyID\);

stringgetName\(\);

intgetID\(\);

\};

Parent::Parent\(string name\,intmyID\) \{

this\->name=name;

this\->myID=myID;

\}

string Parent::getName\(\) \{ return this\->name; \}

intParent::getID\(\) \{ return this\->myID; \}

# Example (Continued)

class Child: public Parent \{

private:

intage;

public:

Child\(string name\,intmyID\,intage\);

intgetAge\(\);

\};

Child::Child\(string name\,intmyID\,intage\) : Parent\(name\,myID\) \{

this\->age=age;

\}

intChild::getAge\(\) \{ return this\->age; \}

intmain\(\) \{

Childbilly\("Bill"\,345\,20\);

cout<<billy\.getName\(\)<<" "<<billy\.getID\(\)<<" "<<billy\.getAge\(\)<<"\\n";

return 0;

\}

