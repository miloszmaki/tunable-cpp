#include "tunable.hpp"
#include <iostream>
using namespace std;

void test(int line, std::string const& expr, std::string const& result) {
    auto eval = tunable_eval(expr);
    while (!eval.empty() && eval.back() == '\n') eval.pop_back();
    if (eval != result) {
        cout << "--- Test failed (line " << line << ") ---\n";
        cout << "Expression: " << expr << "\n";
        cout << "Evaluated:  " << eval << "\n";
        cout << "Expected:   " << result << "\n";
        exit(1);
    }
}

template<class LT, class RT>
void check(int line, bool result, std::string const& op,
           std::string const& lhs, LT const& lhs_eval,
           std::string const& rhs, RT const& rhs_eval) {
    if (result) return;
    cout << "--- Check failed (line " << line << ") ---\n";
    cout << "Expression: " << lhs << " " << op << " " << rhs << "\n";
    cout << "Evaluated:  " << lhs_eval << " " << op << " " << rhs_eval << "\n";
    exit(1);
}

void runTests() {
#define TEST(expr, result) test(__LINE__, expr, result)
#define CHECK_OP(lhs, op, rhs) check(__LINE__, (lhs op rhs), #op, #lhs, lhs, #rhs, rhs)
#define CHECK(lhs, rhs) CHECK_OP(lhs, ==, rhs)

    TEST("2+2", "4");
    TEST("-10 -8", "-18");
    TEST("1. - 3.5/2.", "-0.75");
    TEST("2/ 5", "0");
    TEST("2./5", "0.4");
    TEST("2 / 5.", "0.4");
    TEST("(2+8+32+512) & 128", "0");
    TEST("(2+8+32+512) & 32", "32");

    TEST("1+2()", "Expression evaluation error: invalid syntax\n1+2()\n   ^^");
    TEST("1[]+2", "Expression evaluation error: invalid syntax\n1[]+2\n ^^");
    TEST("(1+1", "Parsing error: unmatched_bracket\n(1+1\n^");
    TEST("1+1)", "Parsing error: unmatched_bracket\n1+1)\n   ^");
    TEST("1+(1", "Parsing error: unmatched_bracket\n1+(1\n  ^");

    TEST("0==0", "true");
    TEST("0.123==0.123", "true");
    TEST("1==-1", "false");
    TEST("0.5==0.499999999999", "false");
    TEST("0.5!=0.500000000000", "false");
    TEST("1==7-2*3", "true");
    TEST("0<1", "true");
    TEST("4>=4", "true");
    TEST("-10<-100", "false");
    TEST("((2*5)==(11-1))", "true");
    TEST("true==true", "true");
    TEST("true==false", "false");
    TEST("(2==3 || 4<=10) && (false || !false)", "true");
    TEST("((3<4 && -3>-4) || !!false) && (-4*-1 > 5 && 2*4==8)", "false");

    TEST("x", "undefined");
    int x=3;
    tunable(x);
    TEST("x", "3");
    TEST("y=x+5", "8");
    TEST("x==y", "false");
    TEST("x<y", "true");
    TEST("x+0.4*y", "6.2");
    CHECK(x, 3);

    if constexpr (_tunable_impl::max_addr_recursion) {
        tunable_eval("p=&x");
        TEST("*p", "3");
        TEST("++*p", "4");
        TEST("x", "4");
        CHECK(x, 4);
    }
    else x = 4;

    TEST("x++y", "Expression evaluation error: invalid syntax\nx++y\n ^^");
    TEST("x++--", "Expression evaluation error: can't modify an rvalue\nx++--\n   ^^");
    CHECK(x, 4);
    TEST("*x++", "Expression evaluation error: no operator for this operand\n*x++\n^");
    CHECK(x, 4);

    TEST("++--++--x", "4");
    TEST("- - x", "4");
    TEST("-+----++++++--x", "-4");
    CHECK(x, 4);
    TEST("x++-++y;x--;--y", "-5\n5\n8");
    CHECK(x, 4);
    TEST("x---y;x++;y", "-4\n3\n8");
    CHECK(x, 4);
    TEST("y=-++x", "-5");
    CHECK(x, 5);

    vector<int> v{2,0,1,3};
    tunable(v);
    TEST("v.size()", "4");
    TEST("!v.empty()", "true");
    TEST("v.push_back(v.front()); v.front() != v.back()", "false");
    CHECK(v.back(), v.front());
    v.back() = -1;
    x = (int)v.size() - 1;
    TEST("v[x]", "-1");
    v.pop_back();
    TEST("4 == v.size()", "true");
    TEST("v[x]", "Expression evaluation error: index out of bounds\nx\n^");
    TEST("v[--x]", "3");
    CHECK(x, 3);
    TEST("*v[--x]", "Expression evaluation error: no operator for this operand\n*v[--x]\n^");
    CHECK(x, 3);
    TEST("v[1]=5*v[2];v[1]", "5\n5");
    CHECK(v[1], 5);
    TEST("v[10]", "Expression evaluation error: index out of bounds\n10\n^^");
    TEST("v[]", "Expression evaluation error: invalid syntax\nv[]\n ^^");
    TEST("v()", "Expression evaluation error: invalid syntax\nv()\n ^^");
    TEST("v{}", "Expression evaluation error: invalid syntax\nv{}\n ^^");
    TEST("v[foo]", "Expression evaluation error: invalid syntax\nfoo\n^^^");

#undef CHECK
#undef CHECK_OP
#undef TEST
}

int main() {
    runTests();
    cout << "All tests passed\n";
    return 0;
}