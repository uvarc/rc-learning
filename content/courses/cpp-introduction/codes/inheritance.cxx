#include <iostream>
#include <string>
using namespace std;

class Animal {
   protected:
      string name;
      string food;
      string vocalization;
      float foodQuantity;
   public:
       void Iam(string name, string vocalization);
       void feedme(string food, float foodQuantity);
       void printme(); 
};

void Animal::Iam(string name, string vocalization) {
    this->name=name;
    this->vocalization=vocalization;
}

void Animal::feedme(string food, float foodQuantity) {
    this->food=food;
    this->foodQuantity=foodQuantity;
}

void Animal::printme() {
    cout<<"I am "<<name<<" I eat "<<food<<" at "<<foodQuantity<<" per day.\n";
}

class Antelope : public Animal {
   public:
	string species;
	string getSpecies();
};

string Antelope::getSpecies() {
    return species;
}

class Reptile : public Animal {
   public:
	string order;
	string getOrder();
};

string Reptile::getOrder() {
    return order;
}

int main() {

    Antelope jumper;
    jumper.Iam("Jenny","urk");
    jumper.feedme("hay",12.0);
    jumper.species="springbok";
    cout<<"I'm a "<<jumper.getSpecies()<<" ",jumper.printme();

    Reptile lizard;
    lizard.Iam("Jimmy","silent");
    lizard.feedme("bugs",0.5);
    lizard.order="lepidosaur";
    cout<<"I'm a "<<lizard.getOrder()<<" ",lizard.printme();

    return 0;
}
