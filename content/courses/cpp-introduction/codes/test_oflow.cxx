#include <iostream>
#include <string>
using namespace std;

int main() {
   char name[10];
   int age;

   cout<<"Enter your name: ";
   
   cin>>name;

   cout<<"Enter your age: ";
   cin>>age;

   printf("Your name is %s and you are %d years old.\n", name, age);

   return 0;
}
