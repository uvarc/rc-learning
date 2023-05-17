#include <iostream>
#include <string>
using namespace std;

class Parent {
    protected:
        int myID;
        string name;

    public:
        Parent(string name, int myID);
        string getName();
        int getID();
};

Parent::Parent(string name, int myID) {
    this->name=name;
    this->myID=myID;
}

string Parent::getName() { 
    return name; 
}

int Parent::getID() { 
    return myID; 
}

class Child: public Parent {
    private:
        int age;
    public:
        Child(string name, int myID, int age);
        int getAge();
};

Child::Child(string name, int myID, int age) : Parent(name, myID) {
    this->age=age;
}

int Child::getAge() { 
    return age; 
}

int main() {
    Child billy("Bill",345,20);
    cout<<billy.getName()<<" "<<billy.getID()<<" "<<billy.getAge()<<"\n";
    return 0;

}
