#include "Employee.h"

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

int Employee::getID(){
    return ID;
}

std::string Employee::getName(){
    return name;
}

std::string Employee::getManager(){
    return manager;
}

std::string Employee::getDepartment(){
    return department;
}

float Employee::getSalary(){
    return salary;
}
