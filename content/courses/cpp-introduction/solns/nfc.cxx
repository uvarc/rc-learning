#include <iostream>


int main() {
    bool packers_win, seahawks_win, saints_win;
    bool packers_lose, seahawks_lose, saints_lose;

    bool packers_advance=false;
    bool seahawks_advance=false;
    bool saints_advance=false;

    //Fill in with predictions
    packers_win=false; packers_lose=true;
    seahawks_win=true; seahawks_lose=false;
    saints_win=false; saints_lose=false;

    if ( packers_win || ( !seahawks_win ) ) {
       packers_advance=true;
    }

    if ( seahawks_win ) {
    }  if ( packers_lose && !saints_win ) {
          seahawks_advance=true;
    }

    if ( saints_win ) {
    }  if ( packers_lose && seahawks_win ) {
          saints_advance=true;
    }

    if ( packers_advance ) {
        std::cout<<"Packers advance!\n";
    }   else if ( seahawks_advance )  {
        std::cout<<"Seahawks advance!\n";
    }   else if ( saints_advance )  {
        std::cout<<"Saints advance!\n";
    }   else {
        std::cout<<"Error: no match to conditions\n";
    }

    return 0;
}

