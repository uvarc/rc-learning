#include <iostream>
#include <string>
#include <vector>
#include "Employee.h"

using namespace std;

int main() {

   vector<Employee> staff;
   int ID;
   string name, manager, department;
   float salary;

   staff.push_back(Employee(1234,"Fred Flintstone","Slate","Heavy Equipment Operations",45000.));
   staff.push_back(Employee(1235,"Barney Rubble","Slate","Engineering",43000.));

   for (int i; i<staff.size(); ++i) {
      if (staff[i].getName()=="Barney Rubble") {
          staff[i].updateSalary(3.);
      }
   }

   cout<<staff[1].getName()<<" now has a salary of "<<staff[1].getSalary()<<endl;

   return 0;

}
