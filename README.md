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
For custom types you should overload the stream operators `<<` and `>>`.

Call `tunablecmd()` to enter the interactive command line.

Possible run-time interactions:
- read a variable
- assign a variable with a value or another variable (implicit type casting)
- create a new variable

## Example

```cpp
#include "tunable.hpp"

struct vec3 {
    double x,y,z;
};

std::ostream& operator<<(std::ostream& stream, const vec3& v) {
    return stream << "[" << v.x << "," << v.y << "," << v.z << "]";
}

std::istream& operator>>(std::istream& stream, vec3& v) {
    char c;
    return stream >> v.x >> c >> v.y >> c >> v.z;
}

int main()
{
    int x = 12;
    tunable(x);

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
    tunable(v.x);
    tunable(v.y);
    tunable(v.z);

    vec3 v2{2,5,3};
    tunable(v2);

    // view and modify tunables in command line
    tunablecmd();

    std::cout << "x=" << x << "\n";
    std::cout << "f=" << f << "\n";
    std::cout << "s=" << s << "\n";
    std::cout << "b=" << std::boolalpha << b << "\n";
    std::cout << "v=" << v << "\n";
    std::cout << "v2=" << v2 << "\n";

    return 0;
}
```

Below you can find possible interactions performed at runtime.

```
--- TUNABLE BEGIN ---    (CTRL+D to quit)
$ x
12
$ f
-245.124
$ s
test
$ b
true
$ x=100
$ x
100
$ f=x
$ f
100
$ f=0.123
$ s=f
$ s
0.123
$ b=false
$ b
false
$ a="hello world"
$ s=a
$ s
hello world
$ v
[1,0.4,-0.2]
$ v.x=8
$ v.z=3
$ v
[8,0.4,3]
$ v=-1,2,-1
$ v
[-1,2,-1]
$ v2
[2,5,3]
$ v2=v
$ v2
[-1,2,-1]
$ v.y=5
$ out_of_scope
undefined
$
--- TUNABLE END ---
x=100
f=0.123
s=hello world
b=false
v=[-1,5,-1]
v2=[-1,2,-1]
```
