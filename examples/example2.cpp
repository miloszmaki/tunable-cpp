#include "tunable.hpp"
#include <iostream>
using namespace std;

struct Point {
    double x{0.};
    double y{0.};
};

ostream& operator<<(ostream& stream, const Point& p) { return stream << "{" << p.x << "," << p.y << "}"; }
istream& operator>>(istream& stream, Point& p) { char c; return stream >> p.x >> c >> p.y; }

struct Circle {
    Point center;
    double radius;
};

ostream& operator<<(ostream& stream, const Circle& c) { return stream << "{center=" << c.center << ",radius=" << c.radius << "}"; }

int main()
{
    tunable(Point, x);
    tunable(Point, y);
    tunable(Circle, center);
    tunable(Circle, radius);
    
    Point p1{1.5, -2.}, p2{-0.4, 3.2};
    tunable(p1);
    tunable(p2);

    Circle c{p1, 5.};
    tunable(c);

    tunablecmd();

    cout << "p1: " << p1 << endl;
    cout << "p2: " << p2 << endl;
    cout << "c:  " << c  << endl;

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
c={center={1.5,-2},radius=5}
p1={1.5,-2}
p2={-0.4,3.2}
$ c.center=p2
{-0.4,3.2}
$ c.radius*=1.5
7.5
$ p2.y = p2.x - p1.y
1.6
$ ;q
--- TUNABLE END ---
p1: {1.5,-2}
p2: {-0.4,1.6}
c:  {center={-0.4,3.2},radius=7.5}

*/