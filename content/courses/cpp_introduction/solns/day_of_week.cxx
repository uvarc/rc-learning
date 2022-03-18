#include <iostream>

using namespace std;

/*******************************************************************************
  ! This program computes the day of the week given a date in the
  ! Gregorian calendar.
  !
  ! Author:    K. Holcomb
  ! Changelog: 2013-06-05 Initial version
  !            2014-01-29 Bug fix to correct leap_year test for century
  !            2014-01-29 Added a loop for user input
  !            2015-01-29 Modification to use only conditionals (no arrays)
*******************************************************************************/

int main() {

    int  maxYear, minYear;
    int  day, month, year, century;
    int  D, M, Y, C, L, W;
    bool leapYear, centuryLeapYear;

    day=2;
    month=3;
    year=2016;

    D=day;
    century=100*(int(year)/100);
    Y=year-century;

    centuryLeapYear=(century%400)==0;

    leapYear=false;

    if (Y>0) {
        leapYear=(year%4)==0;
    }
    else if (centuryLeapYear) {
        leapYear=true;
    }

    L=int(Y)/4;

    if (centuryLeapYear) L+=1;

    if (leapYear && month<3) L-=1;

    if (month==1 || month==10) {
        M=0;
    }
    else if (month==2 || month==3 || month==11) {
        M=3;
    }
    else if (month==4 || month==7) {
        M=6;
    }
    else if (month==5) {
        M=1;
    }
    else if (month==6) {
        M=4;
    }
    else if (month==8) {
        M=2;
    }
    else {
        M=5;
    }

    if  ( century==1400 || century==1800 || century==2200) {
        C=2;
    }
    else if  ( century==1500 || century==1900 || century==2300) {
        C=0;
    }
    else if  ( century==1600 || century==2000 || century==2400) {
        C=5;
    }
    else if  ( century==1700 || century==2100 || century==2500) {
        C=4;
    }
    else {
        cout<<"This algorithm doesn't cover the century requested.\n";
    }

    W=(C+Y+L+M+D)%7;

    switch (W) {
        case(0):
            cout<<"Sunday\n";
            break;
        case(1):
            cout<<"Monday\n";
            break;
        case(2):
            cout<<"Tuesday\n";
            break;
        case(3):
            cout<<"Wednesday\n";
            break;
        case(4):
            cout<<"Thursday\n";
            break;
        case(5):
            cout<<"Friday\n";
            break;
        case(6):
            cout<<"Saturday\n";
            break;
        case(7):
            cout<<"Sunday\n";
            break;
    }

    return 0;

}

