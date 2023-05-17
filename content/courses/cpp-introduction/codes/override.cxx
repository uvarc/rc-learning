#include<iostream>
#include <string>

using namespace std;

class Animal {
   public:
   string speak() {
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

int main(void) {

   Horse secretariat;
   Lion simba;
   Wolf fang;

   cout<<"I am a horse and I "<<secretariat.speak()<<".\n";
   cout<<"I am a lion and I "<<simba.speak()<<".\n";
   cout<<"I am a wolf and I "<<fang.speak()<<".\n";

   return 0;
}
