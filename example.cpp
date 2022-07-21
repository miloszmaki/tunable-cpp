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