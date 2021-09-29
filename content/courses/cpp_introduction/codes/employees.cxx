#include <iostream>
#include <string>
#include <vector>
using namespace std;

int main() {

   struct Employee {
      int     ID;
      string  name, manager, department;
      float   salary;
   };

   Employee employees[2];

   employees[0]={1234,"Fred","Janice","Construction",45000.50};
   employees[1]={1235,"Barney","Janice","Construction",43900.10};

   vector<Employee> newHires;

   newHires.push_back(Employee());
   newHires[0]={2234,"Jack","Tim","Accounting",55000.};
   newHires.push_back(Employee());
   newHires[1]={2235,"Jill","Tim","Accounting",48000.};

   cout<<"Welcome "<<newHires[0].name<<" and "<<newHires[1].name<<" to the group.\n";

}

   


