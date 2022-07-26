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