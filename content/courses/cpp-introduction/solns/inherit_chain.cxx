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

class Reptile : public Animal {
   public:
	string scaleColor;
	string order;
	string getOrder();
};

string Reptile::getOrder() {
    return order;
}

class Mammal : public Animal {
   public:
	string furColor;
	string order;
	string getOrder();
};

string Mammal::getOrder() {
    return order;
}

class Antelope : public Mammal {
   public:
	string species;
	string getSpecies();
};

string Antelope::getSpecies() {
    return species;
}

int main() {

    Antelope jumper;
    jumper.Iam("Jenny","urk");
    jumper.feedme("hay",12.0);
    jumper.order="artiodactyla";
    jumper.furColor="tan";
    jumper.species="springbok";
    cout<<"I'm a "<<jumper.getSpecies()<<" ",jumper.printme();
    cout<<"My fur is "<<jumper.furColor<<".\n";

    Reptile lizard;
    lizard.Iam("Jimmy","silent");
    lizard.feedme("bugs",0.5);
    lizard.order="lepidosaur";
    cout<<"I'm a "<<lizard.getOrder()<<" ",lizard.printme();

    return 0;
}
