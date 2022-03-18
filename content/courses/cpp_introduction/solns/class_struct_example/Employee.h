#include <string>
#include <vector>

class Employee {
   int     ID;
   std::string  name, manager, department;
   float   salary;
   
   public:
   Employee(int, std::string, std::string, std::string, float);
   void updateSalary(float);
   int getID();
   std::string getName();
   std::string getManager();
   std::string getDepartment();
   float getSalary();
};

