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
	string type;
	string getType();
};

string Reptile::getType() {
    return type;
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
    lizard.type="lepidosaur";
    cout<<"I'm a "<<lizard.getType()<<" ",lizard.printme();

    return 0;
}
