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
   string speak() override {
      return "whinny";
   }
};

class Wolf: public Animal {
   public:
   string speak() override {
      return "howl";
   }
};

class Lion: public Animal {
   public:
   string speak() override {
      return "roar";
   }
};

int main(void) {

   Horse secretariat;
   Lion simba;
   Wolf fang;

   Horse& s=secretariat;
   Lion&  l=simba;
   Wolf&  w=fang;

   cout<<"I am a horse and I "<<s.speak()<<".\n";
   cout<<"I am a lion and I "<<l.speak()<<".\n";
   cout<<"I am a wolf and I "<<w.speak()<<".\n";

   return 0;
}
