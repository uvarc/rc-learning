#include <iostream>
#include <string>
using namespace std;

int main() {

    string title="This is a string";
    string subtitle="Another string";

    cout<<title.size()<<"\n";

    string newtitle=title+":"+subtitle;

    cout<<newtitle<<"\n";
    cout<<newtitle.substr(1,3)<<"\n";
    cout<<newtitle[4]<<"\n";
    cout<<newtitle.at(5)<<"\n";

    subtitle.clear();
    subtitle="The Sequel";
    subtitle.append("!");

    cout<<subtitle<<"\n";

    int start=newtitle.find("Another");
    int end=newtitle.size();
    int n=end-start;
    newtitle.replace(start,end,subtitle);
    cout<<newtitle<<"\n";
    int pos=newtitle.find(":");
    newtitle.insert(pos+1," ");
    cout<<newtitle<<"\n";
}

 
    

