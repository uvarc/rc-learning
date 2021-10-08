#include<iostream>
#include <string>

using namespace std;

class Animal {
   public:
   virtual string speak() {
       return "nothing";
   }
};

class Horse: public Animal {
   public:
   string speak() {
      return "whinny";
   }
};

class Wolf: public Animal {
   public:
   string speak() {
      return "howl";
   }
};

class Lion: public Animal {
   public:
   string speak() {
      return "roar";
   }
};

void printme(Animal* a) {
   cout<<"I "<<a->speak()<<"\n";
}

int main(void) {

   Horse* secretariat = new Horse;
   Lion* simba = new Lion;
   Wolf* fang = new Wolf;

   printme(secretariat);
   printme(simba);
   printme(fang);

   return 0;
}
