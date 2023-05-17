#include <iostream>
#include <string>
#include <vector>

using namespace std;

class Employee {

   public:

   int     ID;
   std::string  name, manager, department;
   float   salary;
   
   Employee(int, std::string, std::string, std::string, float);
   void updateSalary(float);
};

Employee::Employee(int ID, std::string name, std::string manager, std::string department, float salary){
    this->ID=ID;
    this->name=name;
    this->manager=manager;
    this->department=department;
    this->salary=salary;
}

void Employee::updateSalary(float raise) {
    //takes percent
    salary+=salary*raise/100.;
}

int main() {

   vector<Employee> staff;
   int ID;
   string name, manager, department;
   float salary;

   staff.push_back(Employee(1234,"Fred Flintstone","Slate","Heavy Equipment Operations",45000.));
   staff.push_back(Employee(1235,"Barney Rubble","Slate","Engineering",43000.));

   for (int i; i<staff.size(); ++i) {
      if (staff[i].name=="Barney Rubble") {
          staff[i].updateSalary(3.);
      }
   }

   cout<<staff[1].name<<" now has a salary of "<<staff[1].salary<<endl;

   return 0;

}
