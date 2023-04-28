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
       Animal(string name, string vocalization, string food, float foodQuantity);
       void printme(); 
};

Animal::Animal(string name, string vocalization, string food, float foodQuantity) {
    this->name=name;
    this->vocalization=vocalization;
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
	Reptile(string name, string vocalization, string food, float foodQuantity, string scaleColor, string order);
};

Reptile::Reptile(string name, string vocalization, string food, float foodQuantity, string scaleColor, string order) : Animal(name,vocalization,food,foodQuantity) {
    this->name=name;
    this->vocalization=vocalization;
    this->food=food;
    this->foodQuantity=foodQuantity;
    this->order=order;
    this->scaleColor=scaleColor;
}

string Reptile::getOrder() {
    return order;
}

class Mammal : public Animal {
   public:
	string furColor;
	string order;
	string getOrder();
	Mammal(string name, string vocalization, string food, float foodQuantity, string furColor, string order);
};

Mammal::Mammal(string name, string vocalization, string food, float foodQuantity, string furColor, string order) : Animal(name,vocalization,food,foodQuantity) {
    this->name=name;
    this->vocalization=vocalization;
    this->food=food;
    this->foodQuantity=foodQuantity;
    this->order=order;
    this->furColor=furColor;
}

string Mammal::getOrder() {
    return order;
}

class Antelope : public Mammal {
   public:
	string species;
	string getSpecies();
	using Mammal::Mammal;
};

string Antelope::getSpecies() {
    return species;
}

int main() {

    Antelope jumper("Jenny","urk","hay",12.0,"tan","artiodactyla");
    jumper.species="springbok";
    cout<<"I'm a "<<jumper.getSpecies()<<" ",jumper.printme();
    cout<<"My fur is "<<jumper.furColor<<".\n";

    Reptile lizard("Jimmy","silent","bugs",0.5,"green","lepidosaur");
    cout<<"I'm a "<<lizard.getOrder()<<" ",lizard.printme();

    return 0;
}
