#include <stdio.h>
#include <stdlib.h>
 
int main(void)
{
    float *p1 = calloc(4, sizeof(float));    // allocate and zero out an array of 4 int
    float *p2 = calloc(1, sizeof(float[4])); // same, naming the array type directly
    float *p3 = calloc(4, sizeof *p3);     // same, without repeating the type name
 
    if(p2) {
        for(int n=0; n<4; ++n) // print the array
            printf("p2[%f] == %f\n", n, p2[n]);
    }
 
    free(p1);
    free(p2);
    free(p3);
}
