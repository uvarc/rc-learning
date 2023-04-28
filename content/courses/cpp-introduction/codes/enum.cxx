#include <iostream>

int main() {

    enum numbers { zero, one, two };

    enum class color {red, yellow, orange};
    enum class fruit {apple, orange, pear};

    numbers count=zero;
    // implicit cast
    int w=count-12;
    std::cout<<"Can cast to int "<<w<<"\n";

    // static_cast for int to type
    color paint=static_cast<color>(0);
    // must cast to int to print (cout does not know how to print an enum)
    std::cout<<"Paint color "<<static_cast<int>(paint)<<"\n";

    //Example of usage
    if ( paint == color::red ) {
        std::cout<<"The paint is red \n";
    } else if ( paint == color::yellow) {
        std::cout<<"The paint is yellow \n";
    } else if ( paint == color::orange) {
        std::cout<<"The paint is orange \n";
    }
 
    color cl=color::orange;
    fruit fr=fruit::orange;

    // must cast explicitly to compare
    int c= int(cl);
    int f= int(fr);
    if ( c == f ) {
        std::cout<<"Same value "<<c<<"\n";
    } else {
        std::cout<<"Different value "<<c<<" "<<f<<"\n";
    } 
 
    return 0;
}
