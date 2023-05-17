#include <iostream>
#include <sstream>
#include <string>
#include <vector>

int main() {

    std::string num_str;
    float number;

    num_str="17.1";
    std::stringstream ss(num_str);
    ss>>number;
    std::cout<<"The number is "<<number<<std::endl;

    std::string another_str;
    float another_num=11.3;
    std::stringstream st;
    st<<another_num;
    std::cout<<"As a string "+st.str()<<"\n";

    std::stringstream line("This is an example.");
    std::string word;
    std::vector<std::string> words;
    while ( line >> word ) {
       words.push_back(word);
    }
    
    for (std::size_t i=0; i < words.size(); i++ ) {
        std::cout<<words[i]<<":";
    }
    std::cout<<std::endl;

    return 0;

}
    
