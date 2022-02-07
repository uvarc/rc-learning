class Person:
    def __init__(self,name,address):
        self.name=name
        self.address=address

    def getName(self):
        return self.name

    def getAddress(self):
        return self.address

class Employee(Person):
    def __init__(self,name,address,employee_id,salary):
        Person.__init__(self,name,address)
        self.employee_id=employee_id
        self.salary=salary

    def setSalary(self,salary):
        self.salary=salary

    def getSalary(self):
        return self.salary

    def getID(self):
        return self.employee_id

name="Tom Jones"
address="1234 Mystreet, Thecity"
employee_id=6789
salary=45000.
an_employee=Employee(name,address,employee_id,salary)

print("Employee " +an_employee.getName() + " lives at " +an_employee.getAddress()  + " and makes $" + str(an_employee.getSalary()))
