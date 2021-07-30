#include <iostream>
#include <vector>

using namespace std;

int main(void) {
   const int N = 10;
   vector<int> Num(N);
   vector<int>::iterator it;


   for (int k = 0; k < N; k++)
      Num[k] = 2*k + 1;

   cout << "Initial contents of vector Num:" << endl;
   for (int k = 0; k < Num.size(); k++)
      cout << Num[k] << " ";
   cout << endl << endl;

   cout << "Append a value to the end\n";
   cout << endl;
   Num.push_back(-20);

   cout << "Insert a value " << endl;
   it = Num.begin();
   Num.insert(it+3,15);
   for (int i: Num) 
      cout << i << " ";
   cout << endl << endl;

   cout << "Change some elements\n";
   cout << endl;
   Num[0] += 100;
   Num.at(1)=102;

   cout << endl;
   cout << "Iterator\n";
   for (it = Num.begin(); it != Num.end(); it++)
      cout << *it << " ";
   cout << endl << endl;

   cout << "Range for loop\n";
   for (int i: Num) 
      cout << i << " ";
   cout << endl;

   return 0;
}
