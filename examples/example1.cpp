#include "tunable.hpp"
#include <iostream>
using namespace std;

int main()
{
    string str = "Hello world";
    tunable(str);

    int x = 5;
    tunable(x);

    {
        int out_of_scope = 10;
        tunable(out_of_scope);
    }

    double numbers[] = {1.1, 2.2, 3.3};
    tunable(numbers);

    tunable_cmd();

    cout << "str:     " << str << endl;
    cout << "x:       " << x   << endl;
    cout << "numbers: ";
    for (auto &n : numbers) cout << n << ",";
    cout << endl;

    return 0;
}

/*

Example at runtime:

--- TUNABLE BEGIN ---
Special commands:
      ;q ;quit ;exit - quit tunable command line
            ;h ;help - show help
               ;vars - show all variables
             ;values - show all variables with values
$ ;values
numbers=0x7ffd5e971f00
str=Hello world
x=5
$ str="Bye"
Bye
$ ++(++x)
7
$ out_of_scope
undefined
$ n=numbers
0x7ffd5e971f00
$ n[0];n[1];n[2]
1.1
2.2
3.3
$ n[1] = x*x
49
$ ;q
--- TUNABLE END ---
str:     Bye
x:       7
numbers: 1.1,49,3.3,

*/