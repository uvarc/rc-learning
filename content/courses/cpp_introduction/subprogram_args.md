---
title: "Subprogram Arguments"
toc: true
type: book
weight: 72

---

Passing scalar arguments to a function is straightforward.  It is a little more complicated when pointers or compound types are passed.

## Passing by Value and Reference

Internally, the contents of the variables in the argument list must be passed to the function so it can evaluate them.  C++ uses two methods, _pass by value_ and _pass by reference_.  Pass by value is the default for most arguments; a copy is made of the contents of the variable.  The variable in the caller is unaffected by what happens to it inside the function.

Pass by reference uses the operator `&` to pass a reference to the memory location of the variable.  If a referenced variable is changed in the function, its contents outside will also be changed.

{{< code-download file="/courses/cpp_introduction/codes/subroutine.cxx" lang="c++" >}}

Changing the value of an argument within a function is often called a _side effect_ of the function.

C++ can also pass by _pointer_, which is very similar to passing by reference but has a few subtle distinctions.  A pointer is an actual variable whose value is the memory address to which it points; a reference is an object that holds the memory address of the variable it references.  A pointer can have the value NULL; a reference cannot.  Arithmetic can be carried out with pointers but not with references.
{{< code-download file="/courses/cpp_introduction/codes/pointer_pass.cxx" lang="c++" >}}

## Passing Arrays to Subprograms

One-dimensional C-style arrays may be passed as pointers or with empty square brackets [].  The size must be passed as well.
```c++
float mean(float A[],int n);
//code
myMean=mean(A,n);
```
This is equivalent to
```c++
float mean(float *A,int n);
```
C-style arrays are always passed by pointer.
Containers such as vectors may be passed either by copying or by reference.
The choice depends on whether it is important to avoid changing the variable and how much speed matters.  Copying, especially of a large compound type, can be quite slow.

**Example**
```c++
#include <iostream>
using namespace std;

float mean(float A[],int n) {
   float sum=0;
   for (int i=0;i<n;++i){
      sum+=A[i];
   }
   return sum/(float)n;
}

float mean2d(float **A,int n,int m) {
   float sum=0;
   for (int i=0;i<n;++i) {
      for (int j=0;j<m;++j) {
         sum+=A[i][j];
      }
   }
   return sum/(float)(n*m);
}

int main(int argc, char **argv) {
   int n=6, m=4;
   float *A=new float[n];
   float **B=new float*[n];
   for (int i=0;i<n;++i) {
      B[i]=new float[m];
   }

   for (int i=0;i<n;++i) {
      A[i]=i+1;
      for (int j=0;j<m;++j) {
         B[i][j]=i+j+2;
      }
   }

   float mymean=mean(A,n);
   cout<<mymean<<"\n";
   float mymean2d=mean2d(B,n,m);
   return 0;
}
```
In this example, deleting the memory allocated for `A` and `B` is not necessary because it occurs in the main program, and all memory will be released when the program exits.  Best practice would generally be to delete the memory explicitly, however.

## Local Arrays

Arrays that are local to a subprogram may be sized using an integer passed to the subprogram.
Local array memory __must__ be released in the subprogram or a _memory leak_ will result.

Wrong:
```c++
float newmean(float A[],int n){
   float *B=new float[n];
   float sum=0;

   for (int i=0;i<n;++i){
      B[i]=A[i]+2;
      sum+=B[i];
   }
   return sum/(float)n;
}
```
Right:
```c++
float newmean(float A[],int n){
   float *B=new float[n];
   float sum=0;

   for (int i=0;i<n;++i){
      B[i]=A[i]+2;
      sum+=B[i];
   }
   delete[] B;
   return sum/(float)n;
}
```

## Passing Function Names

The name of a subprogram can be passed to another subprogram.  One method is to pass a _function pointer_ to the subprogram.  A newer method is to use the standard _functional_ templates.  

**Example**
a numerical-integration subroutine needs the function to be integrated.
```c++
#include <functional>

//Function pointer
//float trap(float a, float b, float h, int n, float (*function)(float)) {
//Templated function object
float trap(float a, float b, float h, int n, function<float(float)> f) {
```
where f is a function.

{{< spoiler text="Full example of passing a subprogram as a dummy variable" >}}
{{< code-download file="/courses/cpp_introduction/codes/trap.cxx" lang="c++" >}}
{{< /spoiler >}}


