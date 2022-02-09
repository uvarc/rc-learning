class Employee:
    "Describes an employee."

    def __init__(self,name,ID,salary):
        self._name=name
        self._ID=ID
        self._salary=salary

    def raise_salary(self,percent):
        self._salary=(1.+percent/100.)*self._salary

    def get_name(self):
        return self._name

    def get_ID(self):
        return self._ID

    def get_salary(self):
        return self._salary

def main():
    fred=Employee("Frederick Jones",1234,55000.)
    fred.raise_salary(3.)
    print(fred.get_name(),"'s new salary is ",fred.get_salary())

if __name__=="__main__":
    main()
