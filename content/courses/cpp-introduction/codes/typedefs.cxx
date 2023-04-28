#include <iostream>
#include <string>
using namespace std;

int main() {

   struct Employee {
      int     ID;
      string  name, manager, department;
      float   salary;
   };

   typedef string ID;

   ID e1;
   e1="11";

   Employee employee1;

   employee1.ID=11;

}
