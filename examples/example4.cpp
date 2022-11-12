#include "tunable.hpp"
#include <iostream>
#include <complex>
using namespace std;

int main()
{
    tunable_op(complex<double>, double);

    complex<double> c, i = 1i;
    tunable(c);
    tunable(i);

    tunable_cmd();

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
c=(0,0)
i=(0,1)
$ i*i
(-1,0)
$ c=2
(2,0)
$ c2=1.+2.*i
(1,2)
$ c2 * (1.-2.*i)
(5,0)
$ c3 = c+c2
(3,2)
$ c4 = c2/c3
(0.538462,0.307692)
$ ;values
c=(2,0)
c2=(1,2)
c3=(3,2)
c4=(0.538462,0.307692)
i=(0,1)
$ c2 == c3
false
$ c3 -= 2.0
(1,2)
$ c2 == c3
true
$ ;q
--- TUNABLE END ---

*/