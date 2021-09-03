#include <string>
using namespace std;
class ColorTypes {
   int red, green, blue;
   public:
      ColorTypes(int, int, int);
      friend string color(int, int, int);
};

string color(int, int, int) {
   string colorname;
   // implementation of some lookup table between RGB and a color name
   return colorname;
}

