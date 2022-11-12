#include "tunable.hpp"
#include <iostream>
using namespace std;

struct Point {
    double x{0.};
    double y{0.};
};

bool operator==(Point const& p1, Point const& p2) { return p1.x == p2.x && p1.y == p2.y; }

ostream& operator<<(ostream& stream, const Point& p) { return stream << "{" << p.x << "," << p.y << "}"; }
istream& operator>>(istream& stream, Point& p) { char c; return stream >> p.x >> c >> p.y; }

template<class T>
ostream& operator<<(ostream& stream, const std::vector<T>& v) {
    stream << "{";
    for (auto &x : v) stream << x << ",";
    return stream << "}";
}

int main()
{
    tunable(Point, x);
    tunable(Point, y);

    vector<Point> v{{0.,1.}, {2.,-0.5}};
    tunable(v);

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
v={{0,1},{2,-0.5},}
$ v.size()
2
$ v.resize(4)
$ v
{{0,1},{2,-0.5},{0,0},{0,0},}
$ v.back()=v.front()
{0,1}
$ v.push_back(v[1])
$ v
{{0,1},{2,-0.5},{0,0},{0,1},{2,-0.5},}
$ v.size() < 5
false
$ v.pop_back()
$ v.size() < 5
true
$ v.size() == 4
true
$ v.back() == v.front()
true
$ v[v.size()-1].y = v[v[0].x = 1].x
2
$ v
{{1,1},{2,-0.5},{0,0},{0,2},}
$ p1 = v[0]; p2 = v[1]; p3 = v[2]
{1,1}
{2,-0.5}
{0,0}
$ v.clear()
$ v.empty()
true
$ v.push_back(p1);v.push_back(p2);v.push_back(p3);v
{{1,1},{2,-0.5},{0,0},}
$ v.empty();v.size()
false
3
$ ;q
--- TUNABLE END ---

*/