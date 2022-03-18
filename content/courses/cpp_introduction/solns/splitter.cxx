#include <iostream>
#include <string>
#include <vector>

using namespace std;

int main() {

    string input="Sample ;string ;for ; this; project \n";
    char delimiter;
    delimiter=';';

    string element;
    vector<string> output;

    for (char ch : input) {
        if (ch == delimiter) {
            output.push_back(element);
            element="";
        }
        element+=ch;
    }
    //Add in the last one
    output.push_back(element);

    for (int i=0;i<output.size();++i){
       cout<<output[i];
    }

    return 0;
}

   
