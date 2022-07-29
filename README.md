# tunable-cpp
Simple C++ library for tweaking the variables at runtime

## Installation
tunable-cpp is a header-only library, so it's enough to include the downloaded header file:
```cpp
#include "tunable.hpp"
```

The minimum required C++ standard is C++17.

## Usage

Use `tunable(x)` to capture variable `x` for tweaking at runtime.

Use `tunable(Class, x)` to capture member variable `Class::x` for all tunable instances of `Class`.

For custom types you should overload the stream operators `<<` and `>>`.

Call `tunablecmd()` to enter the interactive command line.

Possible run-time interactions:
- read a variable
- assign a variable with a value or another variable (implicit type casting)
- create a new variable

There are also special commands, type `;help` in the interactive command line for more information.

## Example

```cpp
#include "tunable.hpp"

struct vec3 {
    double x=0, y=0, z=0;
};

std::ostream& operator<<(std::ostream& stream, const vec3& v);
std::istream& operator>>(std::istream& stream, vec3& v);

struct Triangle {
    vec3 p[3];
};

std::ostream& operator<<(std::ostream& stream, const Triangle& v);
std::istream& operator>>(std::istream& stream, Triangle& v);

void register_tunable_classes() {
    tunable(vec3, x);
    tunable(vec3, y);
    tunable(vec3, z);

    tunable(Triangle, p[0]);
    tunable(Triangle, p[1]);
    tunable(Triangle, p[2]);
}

int main()
{
    register_tunable_classes();

    int x = 12;
    tunable(x);

    float t[2] = {3.2f, 4.1f};
    tunable(t[0]);
    tunable(t[1]);

    double f = -245.124;
    tunable(f);

    std::string s = "test";
    tunable(s);

    bool b = true;
    tunable(b);

    {
        int out_of_scope = 10;
        tunable(out_of_scope);
    }

    vec3 v{1.0,0.4,-0.2};
    tunable(v);

    vec3 v2{2,5,3};
    tunable(v2);

    Triangle tri;
    tunable(tri);

    // view and modify tunables in command line
    tunablecmd();

    std::cout << "x=" << x << "\n";
    std::cout << "t=" << "{" << t[0] << ", " << t[1] << "}\n";
    std::cout << "f=" << f << "\n";
    std::cout << "s=" << s << "\n";
    std::cout << "b=" << std::boolalpha << b << "\n";
    std::cout << "v=" << v << "\n";
    std::cout << "v2=" << v2 << "\n";
    std::cout << "tri=" << tri << "\n";

    return 0;
}

std::ostream& operator<<(std::ostream& stream, const vec3& v) {
    return stream << "[" << v.x << "," << v.y << "," << v.z << "]";
}

std::istream& operator>>(std::istream& stream, vec3& v) {
    char c;
    return stream >> v.x >> c >> v.y >> c >> v.z;
}

std::ostream& operator<<(std::ostream& stream, const Triangle& v) {
    return stream << "[" << v.p[0] << "," << v.p[1] << "," << v.p[2] << "]";
}

std::istream& operator>>(std::istream& stream, Triangle& v) {
    char c;
    return stream >> v.p[0] >> c >> v.p[1] >> c >> v.p[2];
}
```

Below you can find possible interactions performed at runtime.

```
--- TUNABLE BEGIN ---
Special commands:
      ;q ;quit ;exit - quit tunable command line
            ;h ;help - show help
               ;vars - show all variables
             ;values - show all variables with values
$ ;values
b=true
f=-245.124
s=test
t[0]=3.2
t[1]=4.1
tri=[[0,0,0],[0,0,0],[0,0,0]]
v=[1,0.4,-0.2]
v2=[2,5,3]
x=12
$ x=5
$ x
5
$ out_of_scope
undefined
$ tri.p[0]=v
$ tri
[[1,0.4,-0.2],[0,0,0],[0,0,0]]
$ tri.p[1].x=0.1
$ tri.p[1].z=0.3
$ tri.p[2]=v2
$ tri
[[1,0.4,-0.2],[0.1,0,0.3],[2,5,3]]
$ v.y=-1
$ b=false
$ s="hello world"
$ t[0]=1.7
$ f=t[1]
$ N=10
$ x=N
$ ;values
N=10
b=false
f=4.1
s=hello world
t[0]=1.7
t[1]=4.1
tri=[[1,0.4,-0.2],[0.1,0,0.3],[2,5,3]]
v=[1,-1,-0.2]
v2=[2,5,3]
x=10
$ x=10;x;x=15;s;f=-3.5;x=f;x
10
hello world
-3
$ ;q
--- TUNABLE END ---
x=-3
t={1.7, 4.1}
f=-3.5
s=hello world
b=false
v=[1,-1,-0.2]
v2=[2,5,3]
tri=[[1,0.4,-0.2],[0.1,0,0.3],[2,5,3]]
```
