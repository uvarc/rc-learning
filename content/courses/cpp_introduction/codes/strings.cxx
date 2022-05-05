#include <iostream>
#include <string>

int main() {

    std::string title="This is a string";
    std::string subtitle="Another string";

    std::cout<<title.size()<<"\n";

    std::string newtitle=title+":"+subtitle;

    std::cout<<newtitle<<"\n";
    std::cout<<newtitle.substr(1,3)<<"\n";
    std::cout<<newtitle[4]<<"\n";
    std::cout<<newtitle.at(5)<<"\n";

    subtitle.clear();
    subtitle="The Sequel";
    subtitle.append("!");

    std::cout<<subtitle<<"\n";

    int start=newtitle.find("Another");
    int end=newtitle.size();
    int n=end-start;
    newtitle.replace(start,end,subtitle);
    std::cout<<newtitle<<"\n";
    int pos=newtitle.find(":");
    newtitle.insert(pos+1," ");
    std::cout<<newtitle<<"\n";
}

 
    

