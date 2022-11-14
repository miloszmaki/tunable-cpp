/*
tunable-cpp
Simple C++ library for tweaking the variables at runtime
https://github.com/miloszmaki/tunable-cpp

Licensed under the MIT License

Copyright (c) 2022 Miłosz Makowski

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

#ifndef tunable

#include <string>
#include <vector>
#include <set>
#include <map>
#include <iostream>
#include <iomanip>
#include <sstream>
#include <optional>
#include <memory>
#include <regex>
#include <typeindex>
#include <variant>

namespace _tunable_impl {

// this is required to limit the max. number of address-of operators `&`
// otherwise it would be compiling infinitely
constexpr int max_addr_recursion = 1;

inline void _check(bool condition, std::string text, int line) {
    if (!condition)
        throw std::runtime_error(
            "Check failed in line " + std::to_string(line) + ": " + text + "\n"
            + "If you see this message, it means there is a bug in tunable-cpp.\n"
            + "Please report it at https://github.com/miloszmaki/tunable-cpp/issues");
}
#define _tunable_check(condition) _check(condition, #condition, __LINE__)

template <typename T, class = void>
struct is_out_streamable : std::false_type {};
template <typename T>
struct is_out_streamable<T, std::void_t<decltype(std::cout << *(T*)1)>> : std::true_type {};

template <typename T, class = void>
struct is_in_streamable : std::false_type {};
template <typename T>
struct is_in_streamable<T, std::void_t<decltype(std::cin >> *(T*)1)>> : std::true_type {};

#define  _tunable_unary_operator_checker(op, name) \
    template <typename T, class = void> \
    struct has_unary_operator_##name : std::false_type {}; \
    template <typename T> \
    struct has_unary_operator_##name <T, std::void_t<decltype(op(*(T*)1))>> : std::true_type {};

#define  _tunable_unary_postfix_operator_checker(op, name) \
    template <typename T, class = void> \
    struct has_unary_postfix_operator_##name : std::false_type {}; \
    template <typename T> \
    struct has_unary_postfix_operator_##name <T, std::void_t<decltype((*(T*)1)op)>> : std::true_type {};

_tunable_unary_operator_checker(+, plus)
_tunable_unary_operator_checker(-, minus)
_tunable_unary_operator_checker(~, bit_neg)
_tunable_unary_operator_checker(++, incr)
_tunable_unary_operator_checker(--, decr)
_tunable_unary_operator_checker(!, neg)
_tunable_unary_operator_checker(*, deref)
_tunable_unary_operator_checker(&, addr)
_tunable_unary_postfix_operator_checker(++, post_incr)
_tunable_unary_postfix_operator_checker(--, post_decr)

// bool fix for Clang
template <> struct has_unary_operator_incr<bool> : std::false_type {};
template <> struct has_unary_operator_decr<bool> : std::false_type {};
template <> struct has_unary_postfix_operator_post_incr<bool> : std::false_type {};
template <> struct has_unary_postfix_operator_post_decr<bool> : std::false_type {};

// nullptr_t fix
template <> struct has_unary_operator_addr<std::nullptr_t> : std::false_type {};

#undef _tunable_unary_postfix_operator_checker
#undef _tunable_unary_operator_checker

#define  _tunable_binary_operator_checker(op, name) \
    template <typename TL, typename TR, class = void> \
    struct has_binary_operator_##name : std::false_type {}; \
    template <typename TL, typename TR> \
    struct has_binary_operator_##name <TL, TR, std::void_t<decltype((*(TL*)1)op(*(TR*)1))>> : std::true_type {};

#define _tunable_binary_operator_checker_by_container_type(container, name) \
    template <typename TL, typename TR> \
    struct has_binary_operator_##name <container<TL>, container<TR>> : has_binary_operator_##name<TL,TR> {};

_tunable_binary_operator_checker(+, plus)
_tunable_binary_operator_checker(-, minus)
_tunable_binary_operator_checker(*, mul)
_tunable_binary_operator_checker(/, div)
_tunable_binary_operator_checker(%, mod)
_tunable_binary_operator_checker(==, eq)
_tunable_binary_operator_checker(!=, neq)
_tunable_binary_operator_checker(<, lt)
_tunable_binary_operator_checker(<=, le)
_tunable_binary_operator_checker(>, gt)
_tunable_binary_operator_checker(>=, ge)
_tunable_binary_operator_checker(&&, and)
_tunable_binary_operator_checker(||, or)
_tunable_binary_operator_checker(&, bit_and)
_tunable_binary_operator_checker(|, bit_or)
_tunable_binary_operator_checker(^, bit_xor)
_tunable_binary_operator_checker(<<, bit_shl)
_tunable_binary_operator_checker(>>, bit_shr)

_tunable_binary_operator_checker(=, assign)
_tunable_binary_operator_checker(+=, add_eq)
_tunable_binary_operator_checker(-=, sub_eq)
_tunable_binary_operator_checker(*=, mul_eq)
_tunable_binary_operator_checker(/=, div_eq)
_tunable_binary_operator_checker(%=, mod_eq)
_tunable_binary_operator_checker(&=, bit_and_eq)
_tunable_binary_operator_checker(|=, bit_or_eq)
_tunable_binary_operator_checker(^=, bit_xor_eq)
_tunable_binary_operator_checker(<<=, bit_shl_eq)
_tunable_binary_operator_checker(>>=, bit_shr_eq)

_tunable_binary_operator_checker_by_container_type(std::vector, eq)
_tunable_binary_operator_checker_by_container_type(std::vector, neq)
_tunable_binary_operator_checker_by_container_type(std::vector, lt)
_tunable_binary_operator_checker_by_container_type(std::vector, le)
_tunable_binary_operator_checker_by_container_type(std::vector, gt)
_tunable_binary_operator_checker_by_container_type(std::vector, ge)

#undef _tunable_binary_operator_checker_by_container_type
#undef _tunable_binary_operator_checker

template <class T>
bool one_of(std::vector<T> const& v, T const& x) { return std::find(v.begin(), v.end(), x) != v.end(); }

inline bool one_of(std::string const& chars, char c) { return chars.find(c) != std::string::npos; }

inline const std::regex reg_var{"[_a-zA-Z][_a-zA-Z0-9]*"};
inline const std::regex reg_num_real{"[-+]?(\\d+\\.\\d*|\\.\\d+)(f|F)?"};
inline const std::regex reg_num_hex{"0[xX][0-9a-fA-F]+"};
inline const std::regex reg_num_dec{"[-+]?\\d+"};
inline const std::regex reg_bool("(true|false)\\b");
inline const std::regex reg_char{"'\\\\?.'"};
inline const std::regex reg_nullptr{"nullptr\\b"};

inline bool is_quoted(std::string const& s) {
    return s.size() >= 2 && s[0]=='"' && s.back() == '"';
}

inline bool is_decimal(std::string const& s) {
    return std::regex_match(s, reg_num_dec);
}

inline bool is_hex(std::string const& s) {
    return std::regex_match(s, reg_num_hex);
}

inline bool is_integer(std::string const& s) {
    return is_decimal(s) || is_hex(s);
}

inline std::string escape(std::string const& s) {
    static const std::map<char, char> escapes{
        {'\n', 'n'}, {'\t', 't'}, {'\v', 'v'}, {'\b', 'b'},
        {'\r', 'r'}, {'\f', 'f'}, {'\a', 'a'}, {'\0', '0'}};
    std::string r;
    for (size_t i = 0; i < s.size(); i++) {
        auto it = escapes.find(s[i]);
        if (it == escapes.end()) r += s[i];
        else r += '\\', r += it->second;
    }
    return r;
}

inline std::string unescape(std::string s) {
    static const std::map<char, char> escapes{
        {'n', '\n'}, {'t', '\t'}, {'v', '\v'}, {'b', '\b'},
        {'r', '\r'}, {'f', '\f'}, {'a', '\a'}, {'0', '\0'}};
    bool esc = false;
    size_t t = 0;
    for (size_t i = 0; i < s.size(); i++) {
        if (s[i] == '\\') { esc = true; continue; }
        if (esc) {
            esc = false;
            auto it = escapes.find(s[i]);
            if (it != escapes.end()) { s[t++] = it->second; continue; }
        }
        s[t++] = s[i];
    }
    s.resize(t);
    return s;
}

template <class T>
bool from_string(T& ref, std::string const& s) {
    if constexpr (is_in_streamable<T>::value) {
        std::stringstream ss(s);
        if (is_hex(s)) ss >> std::hex;
        ss >> ref;
        return true;
    }
    else {
        std::cout << "missing >> overload for type " << typeid(T).name() << "\n";
        return false;
    }
}

template <>
inline bool from_string(std::string& ref, std::string const& s) {
    if (is_quoted(s)) ref = unescape(s.substr(1, s.size()-2));
    else ref = s;
    return true;
}

template <>
inline bool from_string(char& ref, std::string const& s) {
    if (std::regex_match(s, reg_char)) {
        auto p = unescape(s.substr(1, s.size()-2));
        if (p.size() != 1) return false;
        ref = p[0];
        return true;
    }
    if (s.size() != 1) return false;
    ref = s[0];
    return true;
}

template <>
inline bool from_string(bool& ref, std::string const& s) {
    if (is_integer(s)) {
        long long i;
        if (!from_string(i, s)) return false;
        ref = i != 0;
        return true;
    }
    if (s == "true") ref = true;
    else if (s == "false") ref = false;
    else return false;
    return true;
}

template <class T>
bool from_string(T*& ref, std::string const& s) {
    if (s == "nullptr") {
        ref = nullptr;
        return true;
    }
    if (!is_integer(s)) return false;
    unsigned long long addr;
    if (!from_string(addr, s)) return false;
    ref = reinterpret_cast<T*>(addr);
    return true;
}

template <class T>
std::optional<std::string> stringify(T const& ref) {
    if constexpr (is_out_streamable<T>::value) {
        std::stringstream ss;
        ss << ref;
        return ss.str();
    }
    else {
        std::cout << "missing << overload for type " << typeid(T).name() << "\n";
        return std::nullopt;
    }
}

template <>
inline std::optional<std::string> stringify(bool const& ref) {
    return ref ? "true" : "false";
}

template <class T>
std::optional<std::string> stringify(T* const& ref) {
    if (!ref) return "nullptr";
    std::stringstream ss;
    ss << "0x" << std::hex << reinterpret_cast<uintptr_t>(ref);
    return ss.str();
}

struct expression;

struct expr_variable {
    std::string name;
};

enum class expr_const_type { empty, _int, _real, _bool, _char, _string, _nullptr };

struct expr_constant { // number, char or string
    std::string value;
    expr_const_type type = expr_const_type::empty;
};

struct expr_operator {
    std::string type;

    inline static const std::vector<std::string> types{
        "<<=", ">>=",
        "==", "!=", "<=", ">=", "<<", ">>", "->", "&&", "||", "::",
        "++", "--", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=",
        ".", ",", "=", "+", "-", "*", "/", "%", "&", "|", "^", "~", "!", "<", ">", "?", ":" };

    static std::string find(char const* s) {
        if (s[0] == 0) return {};
        char c0 = s[0], c1 = s[1], c2 = (s[1] == 0 ? 0 : s[2]);
        for (auto &op : types) {
            if (op[0] == c0 && (op.size() == 1 || (op[1] == c1 && (op.size() == 2 || op[2] == c2)))) return op;
        }
        return {};
    }
};

struct expr_brackets {
    std::unique_ptr<expression> nested;
    char type; // [ ( {
    char closing() const { return type + (1 + int(type != '(')); }
    std::string to_string() const;
};

struct expression {
    using part = std::variant<expr_variable, expr_constant, expr_operator, expr_brackets>;
    std::vector<part> parts;

    void parse(char const* s, size_t& i);

    std::string to_string(size_t begin_idx = 0, size_t end_idx = -1) const {
        std::string s;
        for (size_t i = begin_idx; i < end_idx && i < parts.size(); i++) {
            auto &p = parts[i];
            if (std::holds_alternative<expr_variable>(p)) s += std::get<expr_variable>(p).name;
            else if (std::holds_alternative<expr_constant>(p)) s += std::get<expr_constant>(p).value;
            else if (std::holds_alternative<expr_operator>(p)) s += std::get<expr_operator>(p).type;
            else if (std::holds_alternative<expr_brackets>(p)) s += std::get<expr_brackets>(p).to_string();
        }
        return s;
    }
};

inline std::string expr_brackets::to_string() const {
    std::string s;
    s += type; // opening bracket
    if (nested) s += nested->to_string();
    s += closing(); // closing bracket
    return s;
}

enum class parsing_error { invalid_syntax, unmatched_bracket };

struct parsing_exception : public std::exception {
    parsing_exception(parsing_error error, char const* str, size_t pos) {
        static const std::map<parsing_error, std::string> errors{
            {parsing_error::invalid_syntax, "invalid syntax"},
            {parsing_error::unmatched_bracket, "unmatched_bracket"}
        };

        std::string indent(pos, ' ');
        std::string marker = "^";

        msg = errors.at(error) + "\n" +
              str + "\n" +
              indent + marker + "\n";
    }

    const char* what() const noexcept override { return msg.c_str(); }

private:
    std::string msg;
};

inline void expression::parse(char const* s, size_t& i) {
    auto reg_flags = std::regex_constants::match_continuous;
    std::cmatch cm;

    for (; s[i]; i++) {
        char const& c = s[i];
        // end of expression
        if (one_of(";])}", c)) break;
        // ignore whitespace
        if (one_of(" \t\n\r", c)) continue;
        // brackets
        if (one_of("[({", c)) {
            const int i0 = i;
            expr_brackets p{.nested = std::make_unique<expression>(), .type = s[i]};
            p.nested->parse(s, ++i);
            if (s[i] != p.closing()) throw parsing_exception(parsing_error::unmatched_bracket, s, i0);
            if (p.nested->parts.empty()) p.nested.reset();
            parts.emplace_back(std::move(p));
            continue;
        }
        // operator
        auto op = expr_operator::find(&c);
        if (!op.empty()) {
            parts.emplace_back(expr_operator{.type=op});
            i += op.size() - 1;
            continue;
        }
        // constant
        auto create_const = [&](std::regex const& reg, expr_const_type type) {
            if (!std::regex_search(&c, cm, reg, reg_flags)) return false;
            size_t n = cm[0].second - &c;
            parts.emplace_back(expr_constant{.value{&c, n}, .type=type});
            i += n - 1;
            return true;
        };
        // nullptr
        if (create_const(reg_nullptr, expr_const_type::_nullptr)) continue;
        // real number
        if (create_const(reg_num_real, expr_const_type::_real)) continue;
        // integer number
        if (create_const(reg_num_hex, expr_const_type::_int)) continue;
        if (create_const(reg_num_dec, expr_const_type::_int)) continue;
        // bool
        if (create_const(reg_bool, expr_const_type::_bool)) continue;
        // char
        if (create_const(reg_char, expr_const_type::_char)) continue;
        // string
        if (c == '"') {
            expr_constant p{.type=expr_const_type::_string};
            ++i;
            bool esc = false;
            for (; s[i] && !(!esc && s[i] == '"'); i++) {
                p.value += s[i];
                if (esc) esc = false;
                else if (s[i] == '\\') esc = true;
            }
            if (!s[i]) throw parsing_exception(parsing_error::invalid_syntax, s, i); // end of string without closing quote
            p.value = unescape(p.value);
            parts.emplace_back(std::move(p));
            continue;
        }
        // variable
        if (std::regex_search(&c, cm, reg_var, reg_flags)) {
            size_t n = cm[0].second - &c;
            parts.emplace_back(expr_variable{.name{&c, n}});
            i += n - 1;
            continue;
        }
        throw parsing_exception(parsing_error::invalid_syntax, s, i);
    }
}

inline std::vector<expression> parse_expressions(std::string const& s) {
    std::vector<expression> expressions;
    expressions.emplace_back();
    auto cs = s.c_str();
    size_t i = 0;
    while (i < s.size()) {
        auto &e = expressions.back();
        e.parse(cs, i);
        if (one_of("])}", cs[i])) throw parsing_exception(parsing_error::unmatched_bracket, cs, i);
        ++i;
        if (!e.parts.empty()) expressions.emplace_back();
    }
    if (expressions.back().parts.empty()) expressions.pop_back();
    return expressions;
}

// tunable base
class tunable_base {
public:
    virtual ~tunable_base() {}
};

using addr_helper_id_t = unsigned int;

// allows to create new tunables
class tunable_factory {
public:
    template <class T>
    static void create(T& ref, std::string const& name, addr_helper_id_t addr_helper_id);
private:
    inline static std::vector<tunable_base*> tunables;
};

enum class expr_eval_error { invalid_syntax, undefined, void_to_value, bad_value_assign, invalid_var_name, idx_out_of_bounds };

struct expr_eval_exception : public std::exception {
    expr_eval_exception(std::string const& error, expression const& expr, size_t part_idx) {
        init(error, expr, part_idx);
    }

    expr_eval_exception(expr_eval_error error, expression const& expr, size_t part_idx) {
        static const std::map<expr_eval_error, std::string> errors{
            {expr_eval_error::invalid_syntax, "invalid syntax"},
            {expr_eval_error::undefined, "undefined"},
            {expr_eval_error::void_to_value, "void cannot be converted to value"},
            {expr_eval_error::bad_value_assign, "bad value assignment"},
            {expr_eval_error::invalid_var_name, "invalid variable name"},
            {expr_eval_error::idx_out_of_bounds, "index out of bounds"}
        };
        init(errors.at(error), expr, part_idx);
    }

    const char* what() const noexcept override { return msg.c_str(); }

private:
    std::string msg;

    void init(std::string const& error, expression const& expr, size_t part_idx) {
        auto prefix = expr.to_string(0, part_idx);
        auto suffix = expr.to_string(part_idx);
        std::string indent(prefix.size(), ' ');
        std::string marker(std::max(1UL, expr.to_string(part_idx, part_idx+1).size()), '^');

        msg = error + "\n" +
              prefix + suffix + "\n" +
              indent + marker + "\n";
    }
};

enum class operator_side { prefix, postfix, inner };
struct operator_context {
    size_t part_idx;
    operator_side side;
    int precedence; // lower goes first
    int associativity; // 1 - left to right, -1 - right to left

    bool operator<(operator_context const& other) const {
        if (precedence != other.precedence) return precedence < other.precedence;
        if (associativity > 0) return part_idx < other.part_idx;
        return part_idx > other.part_idx;
    }
};

inline std::vector<operator_context> compute_precedence(expression const& expr) {
    using op_prec_assoc_map = std::map<std::string, std::pair<int, int>>;
    static const op_prec_assoc_map inner_op_prec_assoc {
        {"::", {1, 1}},
        {".", {2, 1}}, {"->", {2, 1}},
        {"*", {5, 1}}, {"/", {5, 1}}, {"%", {5, 1}},
        {"+", {6, 1}}, {"-", {6, 1}},
        {"<<", {7, 1}}, {">>", {7, 1}},
        {"<", {9, 1}}, {"<=", {9, 1}}, {">", {9, 1}}, {">=", {9, 1}},
        {"==", {10, 1}}, {"!=", {10, 1}},
        {"&", {11, 1}},
        {"^", {12, 1}},
        {"|", {13, 1}},
        {"&&", {14, 1}},
        {"||", {15, 1}},
        {"?", {16, -1}},
        {"=", {16, -1}},
        {"+=", {16, -1}}, {"-=", {16, -1}},
        {"*=", {16, -1}}, {"/=", {16, -1}}, {"%=", {16, -1}},
        {"<<=", {16, -1}}, {">>=", {16, -1}},
        {"&=", {16, -1}}, {"^=", {16, -1}}, {"|=", {16, -1}},
        {",", {17, 1}}
    };
    static const op_prec_assoc_map postfix_op_prec_assoc {
        {"++", {2, 1}},
        {"--", {2, 1}},
    };
    static const op_prec_assoc_map prefix_op_prec_assoc {
        {"++", {3, -1}}, {"--", {3, -1}},
        {"+", {3, -1}}, {"-", {3, -1}},
        {"!", {3, -1}}, {"~", {3, -1}},
        {"*", {3, -1}},
        {"&", {3, -1}},
    };

    std::vector<operator_context> precedence;
    auto add_op_context = [&](size_t i, operator_side side, std::pair<int, int> prec_assoc) {
        precedence.emplace_back(operator_context{i, side, prec_assoc.first, prec_assoc.second});
    };

    for (size_t i=0; i<expr.parts.size(); i++) {
        auto& part = expr.parts[i];
        if (!std::holds_alternative<expr_operator>(part)) continue;
        auto& op = std::get<expr_operator>(part);

        auto inner_op_it = inner_op_prec_assoc.find(op.type);
        auto postfix_op_it = postfix_op_prec_assoc.find(op.type);
        auto prefix_op_it = prefix_op_prec_assoc.find(op.type);
        bool is_inner_op = inner_op_it != inner_op_prec_assoc.end();
        bool is_postfix_op = postfix_op_it != postfix_op_prec_assoc.end();
        bool is_prefix_op = prefix_op_it != prefix_op_prec_assoc.end();

        bool prev_part_exists = i > 0;
        bool next_part_exists = i+1 < expr.parts.size();

        auto prec_size = precedence.size();

        if (is_postfix_op && prev_part_exists) {
            add_op_context(i, operator_side::postfix, postfix_op_it->second);
        }
        if (is_prefix_op && next_part_exists) {
            add_op_context(i, operator_side::prefix, prefix_op_it->second);
        }
        if (is_inner_op && prev_part_exists && next_part_exists) {
            add_op_context(i, operator_side::inner, inner_op_it->second);
        }
        if (prec_size == precedence.size())
            throw expr_eval_exception(expr_eval_error::invalid_syntax, expr, i);
    }

    std::sort(precedence.begin(), precedence.end());
    return precedence;
}

template <class T>
class deferred_ptr {
public:
    deferred_ptr(std::function<T*()> make_ptr) : make_ptr(make_ptr) {}
    T* get() {
        evaluate();
        return ptr;
    }
    void free() {
        if (ptr) delete ptr;
    }
private:
    T* ptr = nullptr;
    std::function<T*()> make_ptr;
    bool evaluated = false;
    void evaluate() {
        if (evaluated) return;
        ptr = make_ptr();
        evaluated = true;
        // todo: clean function??
        // make_ptr = {}
    }
};

class expr_evaluation;
using expr_eval_ptr = std::shared_ptr<expr_evaluation>;

struct expr_eval_result {
    expr_eval_ptr ptr;
    size_t next_part_idx;
};

class expr_evaluation : public/*protected*/ std::enable_shared_from_this<expr_evaluation> {
public:
    expr_evaluation(bool rvalue) : rvalue(rvalue) {}
    virtual ~expr_evaluation() {}

    bool is_rvalue() const { return rvalue; }
    virtual std::type_index type() const = 0;
    template <class T> bool is() const { return type() == std::type_index(typeid(T)); }
    template <class T> T& value();

    virtual bool is_ptr() const = 0;
    virtual std::optional<std::string> to_string() = 0;
    virtual expr_eval_ptr create_var(std::string const& name) = 0;
    virtual std::optional<expr_eval_ptr> get_member_var(std::string const& name) = 0;
    virtual expr_eval_ptr apply_unary_operator(std::string const& type, operator_side side) = 0;
    virtual expr_eval_ptr apply_binary_operator(std::string const& type, expr_eval_ptr rhs) = 0;
    virtual std::optional<expr_eval_result> evaluate_var_expression(expression const& expr, size_t part_idx) = 0;

    template <class T> static expr_eval_ptr make_lvalue(std::function<T*()> make_ptr, addr_helper_id_t addr_helper_id);
    template <class T> static expr_eval_ptr make_rvalue(std::function<T*()> make_ptr, addr_helper_id_t addr_helper_id);
    template <class T> static expr_eval_ptr make_lvalue(std::function<T*()> make_ptr);
    template <class T> static expr_eval_ptr make_rvalue(std::function<T*()> make_ptr);
    template <class T> static expr_eval_ptr make_rvalue_from_string(std::string const& s);

    static expr_eval_ptr make_void() { return {}; }

private:
    bool rvalue = false;
};

template <class T> T* lvalue_ptr(T& ref) { return &ref; }
template <class T> T* rvalue_ptr(T const& ref) { return new T(ref); }

// undefined type
struct undefined_type {};

// undefined variable placeholder, allows for assignment only
class expr_eval_undefined_var : public expr_evaluation {
public:
    expr_eval_undefined_var(std::string name) : expr_evaluation(false), name(name) {}
    virtual ~expr_eval_undefined_var() {}

    std::type_index type() const override { return typeid(undefined_type); }

    bool is_ptr() const override { return false; }
    std::optional<std::string> to_string() override { return "undefined"; }
    expr_eval_ptr create_var(std::string const& name) override { throw std::runtime_error("cannot create variable from undefined"); }
    std::optional<expr_eval_ptr> get_member_var(std::string const& name) override { return std::nullopt; }
    expr_eval_ptr apply_unary_operator(std::string const& type, operator_side side) override { throw std::runtime_error("operator cannot be applied on undefined"); }
    expr_eval_ptr apply_binary_operator(std::string const& type, expr_eval_ptr rhs) override {
        if (type != "=") throw std::runtime_error("operator cannot be applied on undefined");
        return rhs->create_var(name);
    }
    std::optional<expr_eval_result> evaluate_var_expression(expression const& expr, size_t part_idx) override { return std::nullopt; }

private:
    std::string name;
};

// helper for address-of operator
class addr_helper_base {
public:
    const addr_helper_id_t id;
    virtual addr_helper_id_t deref() const = 0;
    template <class T>
    std::optional<expr_eval_ptr> addr_of(expr_eval_ptr ptr) const {
        _tunable_check(is<T>());
        return _addr_of(std::move(ptr));
    }

protected:
    addr_helper_base(addr_helper_id_t id) : id(id) {}

    template <class T> bool is() const { return type() == std::type_index(typeid(T)); }
    virtual std::type_index type() const = 0;
    virtual std::optional<expr_eval_ptr> _addr_of(expr_eval_ptr ptr) const = 0;
};

class addr_helpers {
public:
    static addr_helper_id_t add(addr_helper_base* p_addr_helper) {
        auto &self = get_instance();
        self.helpers.emplace_back(p_addr_helper);
        return self.helpers.size() - 1;
    }
    static addr_helper_base* get(addr_helper_id_t id) {
        auto &self = get_instance();
        _tunable_check(id < self.helpers.size());
        return self.helpers[id];
    }
private:
    addr_helpers() {}

    static addr_helpers& get_instance() {
        static addr_helpers instance;
        return instance;
    }

    std::vector<addr_helper_base*> helpers;
};

template <class T, int addr_recursion = 0>
class addr_helper : public addr_helper_base {
public:
    addr_helper_id_t deref() const override;

private:
    addr_helper() : addr_helper_base(addr_helpers::add(this)) {}

    std::type_index type() const override { return typeid(T); }
    std::optional<expr_eval_ptr> _addr_of(expr_eval_ptr ptr) const override;

    template <class>
    friend class addr_helper_factory;
};

template <class T>
struct addr_helper_factory {
    template <int addr_recursion = 0>
    static addr_helper_id_t get() {
        static addr_helper<T, addr_recursion> instance;
        return instance.id;
    }
};

// array support
template <class T, size_t N>
struct addr_helper_factory<T[N]> {
    template <int addr_recursion = 0>
    static addr_helper_id_t get() {
        static addr_helper<T*, addr_recursion> instance;
        return instance.id;
    }
};

template <class T, int addr_recursion>
addr_helper_id_t addr_helper<T, addr_recursion>::deref() const {
    _tunable_check(has_unary_operator_deref<T>::value);
    if constexpr (has_unary_operator_deref<T>::value) {
        using TO = typename std::remove_reference_t<decltype(*(*(T*)1))>;
        return addr_helper_factory<TO>::template get<std::max(0, addr_recursion - 1)>();
    }
    return -1;
}

template <class T, int addr_recursion>
std::optional<expr_eval_ptr> addr_helper<T, addr_recursion>::_addr_of(expr_eval_ptr ptr) const {
    _tunable_check(has_unary_operator_addr<T>::value);
    if constexpr (has_unary_operator_addr<T>::value && addr_recursion < max_addr_recursion) {
        return expr_evaluation::make_rvalue<T*>(
            [ptr = std::move(ptr)]() { return rvalue_ptr(&(ptr->value<T>())); },
            addr_helper_factory<T*>::template get<addr_recursion + 1>());
    }
    return std::nullopt;
}

// evaluation of binary operator for arbitrary types
template <class TL, class TR>
std::optional<expr_eval_ptr> apply_binary_op(std::string const& op_type, expr_eval_ptr lhs, expr_eval_ptr rhs,
                                             addr_helper_id_t lhs_addr_helper_id) {
    _tunable_check(lhs->is<TL>());
    _tunable_check(rhs->is<TR>());

#define _tunable_apply_binary_op(op, name) \
    if constexpr (has_binary_operator_##name<TL,TR>::value) { \
        if (op_type == #op) { \
            using _TO = decltype((*(TL*)1) op (*(TR*)1)); \
            using TO = std::remove_reference_t<_TO>; \
            auto op_addr_helper_id = lhs_addr_helper_id; \
            if constexpr (!std::is_same_v<TO, TL>) op_addr_helper_id = addr_helper_factory<TO>::get(); \
            if constexpr (std::is_lvalue_reference_v<_TO>) { \
                if (lhs->is_rvalue()) throw std::runtime_error("can't modify an rvalue"); \
                return expr_evaluation::make_lvalue<TO>( \
                    [lhs, rhs]() { return lvalue_ptr(lhs->value<TL>() op ((TR const&)rhs->value<TR>())); }, \
                    op_addr_helper_id); \
            } \
            else return expr_evaluation::make_rvalue<TO>( \
                    [lhs, rhs]() { return rvalue_ptr(((TL const&)lhs->value<TL>()) op ((TR const&)rhs->value<TR>())); }, \
                    op_addr_helper_id); \
        } \
    }

    _tunable_apply_binary_op(+, plus)
    _tunable_apply_binary_op(-, minus)
    _tunable_apply_binary_op(*, mul)
    _tunable_apply_binary_op(/, div)
    _tunable_apply_binary_op(%, mod)
    _tunable_apply_binary_op(==, eq)
    _tunable_apply_binary_op(!=, neq)
    _tunable_apply_binary_op(<, lt)
    _tunable_apply_binary_op(<=, le)
    _tunable_apply_binary_op(>, gt)
    _tunable_apply_binary_op(>=, ge)
    _tunable_apply_binary_op(&&, and)
    _tunable_apply_binary_op(||, or)
    _tunable_apply_binary_op(&, bit_and)
    _tunable_apply_binary_op(|, bit_or)
    _tunable_apply_binary_op(^, bit_xor)
    _tunable_apply_binary_op(<<, bit_shl)
    _tunable_apply_binary_op(>>, bit_shr)

    _tunable_apply_binary_op(=, assign)
    _tunable_apply_binary_op(+=, add_eq)
    _tunable_apply_binary_op(-=, sub_eq)
    _tunable_apply_binary_op(*=, mul_eq)
    _tunable_apply_binary_op(/=, div_eq)
    _tunable_apply_binary_op(%=, mod_eq)
    _tunable_apply_binary_op(&=, bit_and_eq)
    _tunable_apply_binary_op(|=, bit_or_eq)
    _tunable_apply_binary_op(^=, bit_xor_eq)
    _tunable_apply_binary_op(<<=, bit_shl_eq)
    _tunable_apply_binary_op(>>=, bit_shr_eq)

#undef _tunable_apply_binary_op_lvalue
#undef _tunable_apply_binary_op_rvalue
#undef _tunable_apply_binary_op

    return std::nullopt;
}

// allows to call binary operators on arbitrary types
template <class TL>
class binary_operators {
public:
    template <class TR>
    static void register_type() {
        std::type_index type = typeid(TR);
        if (op_funcs.count(type)) return;

        op_funcs[type] = [](std::string const& op_type, expr_eval_ptr lhs, expr_eval_ptr rhs) {
            return apply_binary_op<TL,TR>(op_type, lhs, rhs, addr_helper_factory<TL>::get());
        };
    }

    static std::optional<expr_eval_ptr> call(std::string const& op_type, expr_eval_ptr lhs, expr_eval_ptr rhs) {
        _tunable_check(lhs->is<TL>());
        auto op_func_it = op_funcs.find(rhs->type());
        if (op_func_it == op_funcs.end()) return std::nullopt;
        auto &op_func = op_func_it->second;
        return op_func(op_type, lhs, rhs);
    }

private:
    using op_func = std::function<std::optional<expr_eval_ptr>(std::string const&, expr_eval_ptr, expr_eval_ptr)>;
    inline static std::map<std::type_index, op_func> op_funcs;
};

template <class T>
class expr_eval_typed : public expr_evaluation {
public:
    expr_eval_typed(std::function<T*()> make_ptr, bool owned, bool constant, addr_helper_id_t addr_helper_id)
        : expr_evaluation(constant), ptr(make_ptr), owned(owned), addr_helper_id(addr_helper_id) {}

    virtual ~expr_eval_typed() {
        if (owned) ptr.free();
    }

    std::type_index type() const override { return typeid(T); }

    T& value() { return *ptr.get(); }

    bool is_ptr() const override { return std::is_pointer_v<T>; }

    std::optional<std::string> to_string() override {
        return stringify(value());
    }

    expr_eval_ptr create_var(std::string const& name) override;

    std::optional<expr_eval_ptr> get_member_var(std::string const& name) override;

    expr_eval_ptr apply_unary_operator(std::string const& type, operator_side side) override;
    expr_eval_ptr apply_binary_operator(std::string const& type, expr_eval_ptr rhs) override;

    std::optional<expr_eval_result> evaluate_var_expression(expression const& expr, size_t part_idx) override;

protected:
    deferred_ptr<T> ptr;
    bool owned = false;
    const addr_helper_id_t addr_helper_id;
};

template <class T>
expr_eval_ptr expr_eval_typed<T>::create_var(std::string const& name) {
    return expr_evaluation::make_lvalue<T>(
        [shared_this = shared_from_this(), name](){
            auto *typed_this = static_cast<expr_eval_typed<T>*>(shared_this.get());
            T* ptr2 = nullptr;
            if (typed_this->owned) { // move
                ptr2 = typed_this->ptr.get();
                typed_this->owned = false; // prevent deletion
            }
            else { // copy
                ptr2 = new T(typed_this->value());
            }
            T& ref2 = *ptr2;
            tunable_factory::create<T>(ref2, name, typed_this->addr_helper_id);
            return lvalue_ptr(ref2);
        }, addr_helper_id);
}

template <class T>
expr_eval_ptr expr_eval_typed<T>::apply_unary_operator(std::string const& type, operator_side side) {
#define _tunable_apply_unary_postfix_op(op, name) \
    if constexpr (has_unary_postfix_operator_##name<T>::value) { \
        if (type == #op) { \
            using _TO = decltype((*(T*)1)op); \
            using TO = std::remove_reference_t<_TO>; \
            auto op_addr_helper_id = addr_helper_id; \
            if constexpr (!std::is_same_v<TO, T>) op_addr_helper_id = addr_helper_factory<TO>::get(); \
            if (is_rvalue()) throw std::runtime_error("can't modify an rvalue"); \
            return expr_evaluation::make_rvalue<TO>( \
                [shared_this = shared_from_this()](){ \
                    return rvalue_ptr((shared_this->template value<T>())op); \
                }, op_addr_helper_id); \
        } \
    }

#define _tunable_apply_unary_op(op, name) \
    if constexpr (has_unary_operator_##name<T>::value) { \
        if (type == #op) { \
            using _TO = decltype(op(*(T*)1)); \
            using TO = std::remove_reference_t<_TO>; \
            auto op_addr_helper_id = addr_helper_id; \
            if constexpr (!std::is_same_v<TO, T>) op_addr_helper_id = addr_helper_factory<TO>::get(); \
            if constexpr (std::is_lvalue_reference_v<_TO>) { \
                if (is_rvalue()) throw std::runtime_error("can't modify an rvalue"); \
                return expr_evaluation::make_lvalue<TO>([shared_this = shared_from_this()](){ \
                        return lvalue_ptr(op(shared_this->template value<T>())); \
                    }, op_addr_helper_id); \
            } \
            else return expr_evaluation::make_rvalue<TO>([shared_this = shared_from_this()](){ \
                        return rvalue_ptr(op((T const&)shared_this->template value<T>())); \
                    }, op_addr_helper_id); \
        } \
    }

    if (side == operator_side::postfix) {
        _tunable_apply_unary_postfix_op(++, post_incr)
        _tunable_apply_unary_postfix_op(--, post_decr)

        throw std::runtime_error("no operator for this operand");
    }

    _tunable_apply_unary_op(+, plus)
    _tunable_apply_unary_op(-, minus)
    _tunable_apply_unary_op(~, bit_neg)
    _tunable_apply_unary_op(++, incr)
    _tunable_apply_unary_op(--, decr)
    _tunable_apply_unary_op(!, neg)

        if constexpr (has_unary_operator_deref<T>::value) {
            using TO = std::remove_reference_t<decltype(*(*(T*)1))>;// todo: same as decltype(*((T)1)) ?
            if (type == "*") {
                return expr_evaluation::make_lvalue<TO>(
                    [shared_this = shared_from_this()](){
                        auto &p = shared_this->template value<T>();
                        if (p == nullptr) throw std::runtime_error("can't dereference null pointer");
                        return lvalue_ptr(*p);
                    },
                    addr_helpers::get(addr_helper_id)->deref());
            }
        }

    if constexpr (has_unary_operator_addr<T>::value) {
        if (type == "&") {
            if (is_rvalue()) throw std::runtime_error("can't apply on an rvalue");
            auto opt_eval = addr_helpers::get(addr_helper_id)->addr_of<T>(shared_from_this());
            if (opt_eval) return std::move(*opt_eval);
        }
    }

#undef _tunable_apply_unary_op
#undef _tunable_apply_unary_postfix_op

    throw std::runtime_error("no operator for this operand");
}

template <class T>
expr_eval_ptr expr_eval_typed<T>::apply_binary_operator(std::string const& type, expr_eval_ptr rhs) {
    if (rhs->is<T>()) {
        auto eval = apply_binary_op<T,T>(type, shared_from_this(), rhs, addr_helper_id);
        if (!eval) throw std::runtime_error("no operator for these operands");
        return std::move(*eval);
    }

    auto eval = binary_operators<T>::call(type, shared_from_this(), rhs);
    if (eval) return std::move(*eval);

    if (type == "=") {
        if (is_rvalue()) throw std::runtime_error("can't apply on an rvalue");
        if (is_ptr() && rhs->is_ptr())
            throw std::runtime_error("implicit casting of pointers of different type");

        // ensure serialization & deserialization makes sense for these operands
        // before the expression gets evaluated
        // todo: do this within rhs
        //if constexpr (is_out_streamable<RHS_TYPE>::value) // todo: something more general?
            //throw std::runtime_error("type does not allow for serialization");
        if constexpr (!is_in_streamable<T>::value) // todo: something more general?
            throw std::runtime_error("type does not allow for deserialization");

        return expr_evaluation::make_lvalue<T>([shared_this = shared_from_this(), rhs](){
            auto str = rhs->to_string(); // serialize
            if (!str) throw std::runtime_error("serialization error");
            // deserialize
            auto& ref = shared_this->template value<T>();
            if (!from_string(ref, *str)) throw std::runtime_error("deserialization error");
            return lvalue_ptr(ref);
        }, addr_helper_id);
    }

    throw std::runtime_error("no operator for these operands");
}

template <class T>
T& expr_evaluation::value() {
    return static_cast<expr_eval_typed<T>*>(this)->value();
}

template <class T>
expr_eval_ptr expr_evaluation::make_lvalue(std::function<T*()> make_ptr, addr_helper_id_t addr_helper_id) {
    return std::make_shared<expr_eval_typed<T>>(std::move(make_ptr), false, false, addr_helper_id);
}

template <class T>
expr_eval_ptr expr_evaluation::make_rvalue(std::function<T*()> make_ptr, addr_helper_id_t addr_helper_id) {
    return std::make_shared<expr_eval_typed<T>>(std::move(make_ptr), true, true, addr_helper_id);
}

template <class T>
expr_eval_ptr expr_evaluation::make_lvalue(std::function<T*()> make_ptr) {
    return make_lvalue(std::move(make_ptr), addr_helper_factory<T>::get());
}

template <class T>
expr_eval_ptr expr_evaluation::make_rvalue(std::function<T*()> make_ptr) {
    return make_rvalue(std::move(make_ptr), addr_helper_factory<T>::get());
}

template <class T>
expr_eval_ptr expr_evaluation::make_rvalue_from_string(std::string const& s) {
    return make_rvalue<T>([s](){
        T* x = new T;
        if (!from_string(*x, s)) throw std::runtime_error("deserialization error");
        return x;
    });
}

// holds a reference to a tunable variable
template <class T>
class tunable : public tunable_base {
public:
    T& ref;
    const addr_helper_id_t addr_helper_id;

    tunable(T &var, std::string const& name, addr_helper_id_t addr_helper_id)
            : ref(var), name(name), addr_helper_id(addr_helper_id) {
        add_to_type(name);
    }
    virtual ~tunable() {
        remove_from_type(name);
    }
private:
    std::string name;

    void add_to_type(std::string const& name);
    void remove_from_type(std::string const& name);
};

// tunable array support
template <class T, size_t N>
class tunable<T[N]> {
public:
    tunable(T* var, std::string const& name, addr_helper_id_t addr_helper_id)
        : ptr(var), t(ptr, name, addr_helper_id) {}
private:
    T* ptr; // needed to get the reference
    tunable<T*> t;
};

template <class T>
void tunable_factory::create(T& ref, std::string const& name, addr_helper_id_t addr_helper_id) {
    tunables.emplace_back(new tunable<T>(ref, name, addr_helper_id));
}

// interface for variable search and assignment
class tunable_type_base {
public:
    virtual ~tunable_type_base() {}
    virtual std::optional<expr_eval_ptr> get_var_eval(std::string const& name) = 0;
};

// holds a mapping from the variable names to their types
class tunable_types {
public:
    static tunable_type_base* find_type_of_var(std::string const &var_name) {
        auto &self = get_instance();
        auto it = self.var_types.find(var_name);
        if (it == self.var_types.end()) return nullptr;
        return it->second;
    }
    static auto const& get_var_types() { return get_instance().var_types; }
private:
    std::map<std::string, tunable_type_base*> var_types;

    tunable_types() {}

    static tunable_types& get_instance() {
        static tunable_types instance;
        return instance;
    }

    static void add_var(std::string const &name, tunable_type_base* type) {
        auto &self = get_instance();
        self.var_types[name] = type;
    }
    static void remove_var(std::string const &name) {
        auto &self = get_instance();
        self.var_types.erase(name);
    }
    template <class>
    friend class tunable_type;
};

// implements variable search and assignment
// holds a mapping from the names to the tunables for all variables of a given type
template <class T>
class tunable_type : public tunable_type_base {
public:
    std::optional<expr_eval_ptr> get_var_eval(std::string const& name) override {
        auto &self = get_instance();
        auto var = self.find_var_typed(name);
        if (!var) return std::nullopt;
        return expr_evaluation::make_lvalue<T>(
            [&self, name](){
                auto var = self.find_var_typed(name);
                _tunable_check(var);
                return lvalue_ptr(var->ref);
            }, var->addr_helper_id);
    }
private:
    std::map<std::string, tunable<T>*> vars;

    tunable_type() {}

    static tunable_type<T>& get_instance() {
        static tunable_type<T> instance;
        return instance;
    }

    static void add_var(tunable<T> *var, std::string const& name) {
        auto &self = get_instance();
        self.vars[name] = var;
        tunable_types::add_var(name, &self);
    }
    static void remove_var(std::string const& name) {
        auto &self = get_instance();
        self.vars.erase(name);
        tunable_types::remove_var(name);
    }
    tunable<T>* find_var_typed(std::string const& name) {
        auto &self = get_instance();
        auto it = self.vars.find(name);
        if (it != self.vars.end()) return it->second;
        return nullptr;
    }

    template <class>
    friend class tunable;
};

template <class T>
void tunable<T>::add_to_type(std::string const& name) {
    tunable_type<T>::add_var(this, name);
}

template <class T>
void tunable<T>::remove_from_type(std::string const& name){
    tunable_type<T>::remove_var(name);
}

// tunable class member base
class member_var_base {
public:
    virtual ~member_var_base() {}
};

// interface for tunable class members
template <class T>
class member_var_base_typed : public member_var_base {
public:
    virtual ~member_var_base_typed() {}
    virtual expr_eval_ptr get_var_eval(T& ref) = 0;
};

// holds all tunable members for a given class
template <class T>
class member_vars {
public:
    static member_var_base_typed<T>* find_member(std::string const& name) {
        auto &self = get_instance();
        auto it = self.members.find(name);
        if (it == self.members.end()) return nullptr;
        return it->second;
    }
private:
    std::map<std::string, member_var_base_typed<T>*> members;

    member_vars() {}

    static member_vars<T>& get_instance() {
        static member_vars<T> instance;
        return instance;
    }

    static void add(member_var_base_typed<T> *var, std::string const& name) {
        auto &self = get_instance();
        self.members[name] = var;
    }
    static void remove(std::string const& name) {
        auto &self = get_instance();
        self.members.erase(name);
    }

    template <class U, class M>
    friend class member_var;
};

// represents a tunable class member
template <class T, class M>
class member_var : public member_var_base_typed<T> {
public:
    virtual ~member_var() {
        member_vars<T>::remove(name);
    }
    expr_eval_ptr get_var_eval(T& ref) override {
         // todo: here probably we'll pass something different than T&
        auto& member_ref = get_ref(ref);
        return expr_evaluation::make_lvalue<M>([&member_ref](){ return lvalue_ptr(member_ref); });
    }
private:
    std::string name;
    M& (*get_ref)(T&) = nullptr;

    member_var(std::string const& name, M& (*get_ref)(T&))
            : name(name), get_ref(get_ref) {
        member_vars<T>::add(this, name);
    }

    friend class member_var_factory;
};

// represents a tunable class array member
template <class T, class M, size_t N>
class member_var<T, M[N]> : public member_var_base_typed<T> {
public:
    virtual ~member_var() {
        member_vars<T>::remove(name);
    }
    expr_eval_ptr get_var_eval(T& ref) override {
        auto& member_ref = get_ref(ref);
        auto arr_ptr = (M* const&)member_ref;
        return expr_evaluation::make_rvalue<M*>([arr_ptr](){
            return rvalue_ptr(arr_ptr);
        });
    }
private:
    std::string name;
    M(& (*get_ref)(T&))[N] = nullptr;

    member_var(std::string const& name, M(& (*get_ref)(T&))[N])
            : name(name), get_ref(get_ref) {
        member_vars<T>::add(this, name);
    }

    friend class member_var_factory;
};

// allows to register class member variables as tunables
class member_var_factory {
public:
    template <class T, class M>
    static void create(std::string const& name, M& (*get_ref)(T&)) {
        members.emplace_back(new member_var(name, get_ref));
    }
private:
    inline static std::vector<member_var_base*> members;
};

// register pairs of types for binary operator evaluation
template <class T1, class T2>
void register_binary_op_types() {
    binary_operators<T1>::template register_type<T2>();
    if constexpr (!std::is_same_v<T1,T2>)
        binary_operators<T2>::template register_type<T1>();
}

// process prefix parts of name split by operators (`.`, `->` and `::`) and brackets
inline std::optional<expr_eval_result> process_var_name_prefixes(expression const& expr, size_t part_idx, std::function<std::optional<expr_eval_result>(std::string const&, size_t)> func) {
    std::string name;
    for (size_t i = part_idx; i < expr.parts.size(); i++) {
        auto &p = expr.parts[i];

        if (std::holds_alternative<expr_variable>(p)) {
            name += std::get<expr_variable>(p).name;
        }
        else if (std::holds_alternative<expr_brackets>(p)) {
            name += std::get<expr_brackets>(p).to_string();
        }
        else if (std::holds_alternative<expr_operator>(p)) {
            auto &op = std::get<expr_operator>(p).type;
            std::vector<std::string> allowed{".", "->", "::"};
            if (name.empty()) allowed.emplace_back("*");
            if (one_of(allowed, op)) {
                name += op;
                continue;
            }
            break;
        }
        else break;

        auto next_idx = i + 1;
        auto ret = func(name, next_idx);
        if (ret.has_value()) return ret;
    }
    return std::nullopt;
}

expr_eval_ptr evaluate_expression(expression const& expr);

template <class T>
std::optional<expr_eval_ptr> expr_eval_typed<T>::get_member_var(std::string const& name) {
    auto var = member_vars<T>::find_member(name);
    if (!var) return std::nullopt;
    auto var_eval = var->get_var_eval(*ptr.get()); // todo: defer
    _tunable_check(var_eval != nullptr);
    return var_eval;
}

inline expr_eval_result evaluate_var_member(expr_eval_ptr var, expression const &expr, size_t part_idx) {
    auto ret = process_var_name_prefixes(expr, part_idx,
        std::function([&](std::string const& prefix, size_t suffix_idx) -> std::optional<expr_eval_result> {
            auto eval = var->get_member_var(prefix);
            if (!eval) return std::nullopt;
            return expr_eval_result{std::move(*eval), suffix_idx};
        })
    );
    if (ret) return std::move(*ret);
    throw expr_eval_exception(expr_eval_error::undefined, expr, part_idx);
}

template <class T>
std::optional<expr_eval_result> evaluate_typed_var_expr(T& ref, expression const& expr, size_t part_idx) {
    return std::nullopt;
}

template <class T>
expr_eval_ptr evaluate_subscript(T& ref, size_t size, expression const& expr) {
    auto idx_eval = evaluate_expression(expr);
    if (!idx_eval) throw expr_eval_exception(expr_eval_error::void_to_value, expr, 0);
    using elem_type = std::remove_reference_t<decltype((*(T*)1)[0])>;
    return expr_evaluation::make_lvalue<elem_type>([&ref, idx_eval, size, &expr](){ // todo: size might change before deferred evaluation ?
        auto str = idx_eval->to_string();
        if (!str) throw expr_eval_exception(expr_eval_error::invalid_syntax, expr, 0);
        size_t idx = -1;
        if (!is_integer(*str) || !from_string(idx, *str))
            throw expr_eval_exception(expr_eval_error::invalid_syntax, expr, 0);
        if (idx >= size) throw expr_eval_exception(expr_eval_error::idx_out_of_bounds, expr, 0);
        return lvalue_ptr(ref[idx]);
    });
}

template <class T>
std::optional<expr_eval_result> evaluate_typed_var_expr(std::vector<T>& ref, expression const& expr, size_t part_idx) {
    auto &part = expr.parts[part_idx];
    if (std::holds_alternative<expr_brackets>(part)) {
        auto &brackets = std::get<expr_brackets>(part);
        if (brackets.type == '[' && brackets.nested) {
            auto eval = evaluate_subscript(ref, ref.size(), *brackets.nested);
            return expr_eval_result{std::move(eval), part_idx + 1};
        }
    }
    else if (std::holds_alternative<expr_operator>(part)) {
        auto &op = std::get<expr_operator>(part).type;
        if (op == ".") {
            auto evaluate_method = [&](expr_variable const& method, expr_brackets const& args) -> std::optional<expr_eval_ptr> {
                if (args.type != '(') return std::nullopt;
                if (args.nested) { // method with arguments
                    if (method.name == "push_back") {
                        auto eval = evaluate_expression(*args.nested);
                        if (!eval) throw expr_eval_exception(expr_eval_error::void_to_value, *args.nested, 0);
                        if (eval->is<T>()) {
                            ref.push_back(eval->value<T>());
                            return expr_evaluation::make_void();
                        }
                        else { // implicit type conversion
                            auto assignment = binary_operators<T>::call("=",
                                    expr_evaluation::make_lvalue<T>([&ref](){
                                        ref.emplace_back();
                                        return lvalue_ptr(ref.back());
                                    }), eval);
                            if (assignment.has_value()) {
                                expr_eval_ptr& assign_eval = *assignment;
                                auto assigned = assign_eval->value<T>(); // evaluate
                                return expr_evaluation::make_void();
                            }
                        }
                        throw expr_eval_exception(expr_eval_error::invalid_syntax, *args.nested, 0);
                    }
                    else if (method.name == "resize") {
                        auto eval = evaluate_expression(*args.nested);
                        if (!eval) throw expr_eval_exception(expr_eval_error::void_to_value, *args.nested, 0);
                        auto s = eval->to_string();
                        if (s && is_integer(*s)) {
                            size_t size;
                            from_string(size, *s);
                            ref.resize(size);
                            return expr_evaluation::make_void();
                        }
                        throw expr_eval_exception(expr_eval_error::invalid_syntax, *args.nested, 0);
                    }
                }
                else { // method without arguments
                    if (method.name == "size") return expr_evaluation::make_rvalue<size_t>([&ref](){ return rvalue_ptr(ref.size()); });
                    else if (method.name == "capacity") return expr_evaluation::make_rvalue<size_t>([&ref](){ return rvalue_ptr(ref.capacity()); });
                    else if (method.name == "empty") return expr_evaluation::make_rvalue<bool>([&ref](){ return rvalue_ptr(ref.empty()); });
                    else if (method.name == "front") return expr_evaluation::make_lvalue<T>([&ref](){ return rvalue_ptr(ref.front()); });
                    else if (method.name == "back") return expr_evaluation::make_lvalue<T>([&ref](){ return rvalue_ptr(ref.back()); });
                    else if (method.name == "pop_back") { ref.pop_back(); return expr_evaluation::make_void(); }
                    else if (method.name == "clear") { ref.clear(); return expr_evaluation::make_void(); }
                }
                return std::nullopt;
            };
            if (part_idx + 2 < expr.parts.size()) {
                auto &method_part = expr.parts[part_idx + 1];
                auto &args_part = expr.parts[part_idx + 2];
                if (std::holds_alternative<expr_variable>(method_part) &&
                    std::holds_alternative<expr_brackets>(args_part)) {
                    auto eval = evaluate_method(std::get<expr_variable>(method_part), std::get<expr_brackets>(args_part));
                    if (eval) return expr_eval_result{std::move(*eval), part_idx + 3};
                }
            }
        }
    }
    return std::nullopt;
}

template <class T>
std::optional<expr_eval_result> evaluate_typed_var_expr(T*& ref, expression const& expr, size_t part_idx) {
    if constexpr (!std::is_pointer_v<T>) {
        auto &part = expr.parts[part_idx];
        if (std::holds_alternative<expr_operator>(part)) {
            auto &op = std::get<expr_operator>(part).type;
            if (op == "->") {
                return evaluate_var_member(expr_evaluation::make_lvalue<T>([ref](){ return ref; /* todo */ }), expr, part_idx + 1);
            }
        }
        else if (std::holds_alternative<expr_brackets>(part)) {
            auto &brackets = std::get<expr_brackets>(part);
            if (brackets.type == '[' && brackets.nested) {
                size_t unknown_size = -1;
                auto eval = evaluate_subscript(ref, unknown_size, *brackets.nested);
                return expr_eval_result{std::move(eval), part_idx + 1};
            }
        }
    }
    return std::nullopt;
}

template <class T>
std::optional<expr_eval_result> expr_eval_typed<T>::evaluate_var_expression(expression const& expr, size_t part_idx) {
    return evaluate_typed_var_expr(*ptr.get(), expr, part_idx); // todo: defer
}

inline expr_eval_result evaluate_var_expression(expr_eval_ptr var, expression const& expr, size_t part_idx) {
    if (part_idx >= expr.parts.size()) return { std::move(var), part_idx };
    {
        auto ret = var->evaluate_var_expression(expr, part_idx);
        if (ret) return std::move(*ret);
    }
    auto &part = expr.parts[part_idx];
    if (std::holds_alternative<expr_operator>(part)) {
        auto &op = std::get<expr_operator>(part).type;
        if (op == ".") {
            return evaluate_var_member(std::move(var), expr, part_idx + 1);
        }
    }
    return { std::move(var), part_idx };
}

inline expr_eval_result evaluate_value_expression(expression const& expr, size_t part_idx) {
    if (part_idx >= expr.parts.size()) throw expr_eval_exception(expr_eval_error::invalid_syntax, expr, part_idx);

    // search through tunables
    auto ret = process_var_name_prefixes(expr, part_idx,
        std::function([&expr](std::string const& prefix, size_t suffix_idx) -> std::optional<expr_eval_result> {
            auto type = tunable_types::find_type_of_var(prefix);
            if (!type) return std::nullopt;
            auto opt_var_eval = type->get_var_eval(prefix);
            _tunable_check(opt_var_eval.has_value());
            auto var_eval = std::move(*opt_var_eval);
            _tunable_check(var_eval != nullptr);
            // evaluate suffix (member variables and specialized operators and methods)
            try {
                while (suffix_idx < expr.parts.size()) {
                    auto ret = evaluate_var_expression(std::move(var_eval), expr, suffix_idx);
                    var_eval = std::move(ret.ptr);
                    if (ret.next_part_idx == suffix_idx) break;
                    suffix_idx = ret.next_part_idx;
                }
            }
            catch (std::exception &e) {
                return std::nullopt;
            }
            return expr_eval_result{std::move(var_eval), suffix_idx};
        })
    );
    if (ret) return std::move(*ret);

    auto &part = expr.parts[part_idx];
    if (std::holds_alternative<expr_variable>(part)) { // we've got an undefined variable
        auto &var = std::get<expr_variable>(part);
        return { std::make_shared<expr_eval_undefined_var>(var.name), part_idx + 1 };
    }
    else if (std::holds_alternative<expr_constant>(part)) {
        auto cnst = std::get<expr_constant>(part);
        auto create_const = [&](){
            try { // create new rvalue
                switch(cnst.type) {
                    case expr_const_type::_int: return expr_evaluation::make_rvalue_from_string<long long>(cnst.value); // todo: these could be created immediately without using deferred callback
                    case expr_const_type::_real: return expr_evaluation::make_rvalue_from_string<double>(cnst.value);
                    case expr_const_type::_bool: return expr_evaluation::make_rvalue_from_string<bool>(cnst.value);
                    case expr_const_type::_char: return expr_evaluation::make_rvalue_from_string<char>(cnst.value);
                    case expr_const_type::_string: return expr_evaluation::make_rvalue_from_string<std::string>(cnst.value);
                    case expr_const_type::_nullptr: return expr_evaluation::make_rvalue<std::nullptr_t>([](){ return rvalue_ptr(nullptr); });
                    case expr_const_type::empty: _tunable_check(false); break;
                }
            }
            catch (std::exception &e) {}
            throw expr_eval_exception(expr_eval_error::invalid_syntax, expr, part_idx);
        };
        return { create_const(), part_idx + 1 };
    }
    throw expr_eval_exception(expr_eval_error::invalid_syntax, expr, part_idx);
}

inline expr_eval_ptr evaluate_expression(expression const& expr) {
    if (expr.parts.empty()) throw expr_eval_exception(expr_eval_error::invalid_syntax, expr, 0);

    // compute operator precedence
    auto precedence = compute_precedence(expr);

    std::vector<expr_eval_ptr> evals; // list of already evaluated subexpressions
    std::vector<int> eval_idxs(expr.parts.size(), -1); // for each part, index in evals the part is associated with

    // evaluate tunable variables/constants & nested expressions
    for (size_t i=0; i<expr.parts.size(); ++i) {
        auto &part = expr.parts[i];
        if (std::holds_alternative<expr_brackets>(part)) {
            auto& brackets = std::get<expr_brackets>(part);
            if (brackets.type == '(' && (i == 0 || std::holds_alternative<expr_operator>(expr.parts[i-1]))) {
                if (!brackets.nested) throw expr_eval_exception(expr_eval_error::invalid_syntax, expr, i);
                evals.emplace_back(evaluate_expression(*brackets.nested));
                eval_idxs[i] = evals.size() - 1;
            }
        }
        else if (std::holds_alternative<expr_variable>(part) ||
                 std::holds_alternative<expr_constant>(part)) {
            auto eval = evaluate_value_expression(expr, i);
            if (eval.next_part_idx < expr.parts.size() && !std::holds_alternative<expr_operator>(expr.parts[eval.next_part_idx]))
                throw expr_eval_exception(expr_eval_error::invalid_syntax, expr, eval.next_part_idx);
            evals.emplace_back(std::move(eval.ptr));
            auto eval_idx = evals.size() - 1;
            for (; i<eval.next_part_idx; ++i) eval_idxs[i] = eval_idx;
            --i;
        }
    }

    // evaluate operators according to precedence
    for (auto &op_ctx : precedence) {
        _tunable_check(op_ctx.part_idx >= 0 && op_ctx.part_idx < eval_idxs.size());
        if (eval_idxs[op_ctx.part_idx] >= 0) continue;
        auto &part = expr.parts[op_ctx.part_idx];
        if (std::holds_alternative<expr_operator>(part)) {
            auto& op = std::get<expr_operator>(part);
            if (op_ctx.side == operator_side::inner) { // binary operator
                _tunable_check(op_ctx.part_idx-1 >= 0);
                _tunable_check(op_ctx.part_idx+1 < eval_idxs.size());
                auto lhs_eval_idx = eval_idxs[op_ctx.part_idx - 1];
                auto rhs_eval_idx = eval_idxs[op_ctx.part_idx + 1];
                if (lhs_eval_idx < 0 || rhs_eval_idx < 0)
                    throw expr_eval_exception(expr_eval_error::invalid_syntax, expr, op_ctx.part_idx);
                _tunable_check(lhs_eval_idx < evals.size());
                _tunable_check(rhs_eval_idx < evals.size());
                auto& lhs_eval = evals[lhs_eval_idx];
                auto& rhs_eval = evals[rhs_eval_idx];
                _tunable_check(lhs_eval != nullptr);
                _tunable_check(rhs_eval != nullptr);
                try {
                    rhs_eval = lhs_eval->apply_binary_operator(op.type, std::move(rhs_eval));
                }
                catch (std::exception &e) {
                    throw expr_eval_exception(e.what(), expr, op_ctx.part_idx);
                }
                lhs_eval = nullptr;
                eval_idxs[op_ctx.part_idx] = rhs_eval_idx;
                for (int i = op_ctx.part_idx - 1; i >= 0 && eval_idxs[i] == lhs_eval_idx; i--)
                    eval_idxs[i] = rhs_eval_idx;
            }
            else { // unary operator
                const int side = (op_ctx.side == operator_side::prefix) ? 1 : -1;
                const auto side_part_idx = op_ctx.part_idx + side;
                _tunable_check(side_part_idx >= 0 && side_part_idx < eval_idxs.size());
                auto side_eval_idx = eval_idxs[side_part_idx];
                if (side_eval_idx < 0) continue; // the operand side was not evaluated
                const auto other_side_part_idx = op_ctx.part_idx - side;
                if (other_side_part_idx >= 0 && other_side_part_idx < eval_idxs.size()
                    && eval_idxs[other_side_part_idx] >= 0) {
                    continue; // the other side is also a value, so this must be a binary operator
                }
                _tunable_check(side_eval_idx < evals.size());
                auto& side_eval = evals[side_eval_idx];
                _tunable_check(side_eval != nullptr);
                if (side_eval->is<undefined_type>())
                    throw expr_eval_exception(expr_eval_error::undefined, expr, op_ctx.part_idx);
                try {
                    side_eval = side_eval->apply_unary_operator(op.type, op_ctx.side);
                }
                catch (std::exception &e) {
                    throw expr_eval_exception(e.what(), expr, op_ctx.part_idx);
                }
                eval_idxs[op_ctx.part_idx] = side_eval_idx;
            }
        }
    }

    // check if all parts were evaluated
    for (size_t i=0; i<expr.parts.size(); ++i) {
        if (eval_idxs[i] < 0)
            throw expr_eval_exception(expr_eval_error::invalid_syntax, expr, i);
    }

    _tunable_check(eval_idxs[0] < evals.size());
    return std::move(evals[eval_idxs[0]]);
}

class initialization {
public:
    static initialization& ensure() {
        static initialization instance;
        return instance;
    }

private:
    initialization() {
        init_operators();
    }

    void init_operators() {
#define _tunable_register_binary_op_types_numeric(i, T) \
        if constexpr(i < 1)  register_binary_op_types<T, char>(); \
        if constexpr(i < 2)  register_binary_op_types<T, signed char>(); \
        if constexpr(i < 3)  register_binary_op_types<T, unsigned char>(); \
        if constexpr(i < 4)  register_binary_op_types<T, short>(); \
        if constexpr(i < 5)  register_binary_op_types<T, unsigned short>(); \
        if constexpr(i < 6)  register_binary_op_types<T, int>(); \
        if constexpr(i < 7)  register_binary_op_types<T, unsigned int>(); \
        if constexpr(i < 8)  register_binary_op_types<T, long>(); \
        if constexpr(i < 9)  register_binary_op_types<T, unsigned long>(); \
        if constexpr(i < 10) register_binary_op_types<T, long long>(); \
        if constexpr(i < 11) register_binary_op_types<T, unsigned long long>(); \
        if constexpr(i < 12) register_binary_op_types<T, float>(); \
        if constexpr(i < 13) register_binary_op_types<T, double>(); \
        if constexpr(i < 14) register_binary_op_types<T, long double>();

        _tunable_register_binary_op_types_numeric(0,  bool)
        _tunable_register_binary_op_types_numeric(1,  char)
        _tunable_register_binary_op_types_numeric(2,  signed char)
        _tunable_register_binary_op_types_numeric(3,  unsigned char)
        _tunable_register_binary_op_types_numeric(4,  short)
        _tunable_register_binary_op_types_numeric(5,  unsigned short)
        _tunable_register_binary_op_types_numeric(6,  int)
        _tunable_register_binary_op_types_numeric(7,  unsigned int)
        _tunable_register_binary_op_types_numeric(8,  long)
        _tunable_register_binary_op_types_numeric(9,  unsigned long)
        _tunable_register_binary_op_types_numeric(10, long long)
        _tunable_register_binary_op_types_numeric(11, unsigned long long)
        _tunable_register_binary_op_types_numeric(12, float)
        _tunable_register_binary_op_types_numeric(13, double)

#undef _tunable_register_binary_op_types_numeric

        register_binary_op_types<std::string, char>();
    }
};

namespace cmd {

inline void print_help(std::ostream &out) {
    const int col = 20;
    out << "Special commands:\n" <<
        std::setw(col) << ";q ;quit ;exit" << " - quit tunable command line\n" <<
        std::setw(col) << ";h ;help" << " - show help\n" <<
        std::setw(col) << ";vars" << " - show all variables\n" <<
        std::setw(col) << ";values" << " - show all variables with values\n";
}

enum class handling_result { unrecognized, empty, processed, exit };

inline handling_result handle_special_command(std::ostream &out, std::string const& s) {
    if (s == "q" || s == "quit" || s == "exit")
        return handling_result::exit;
    if (s == "h" || s == "help") {
        print_help(out);
        return handling_result::processed;
    }
    if (s == "vars") {
        for (auto &[name,_] : tunable_types::get_var_types()) {
            out << name << "\n";
        }
        return handling_result::processed;
    }
    if (s == "values") {
        for (auto &[name,_] : tunable_types::get_var_types()) {
            expression expr;
            expr.parts.emplace_back(expr_variable{.name=name});
            auto value = evaluate_expression(expr);
            if (value) {
                auto val_str = value->to_string();
                if (val_str) {
                    std::string q = value->is<std::string>() ? "\"" : "";
                    out << name << "=" << q << escape(*val_str) << q << "\n";
                }
            }
        }
        return handling_result::processed;
    }
    return handling_result::unrecognized;
}

inline handling_result handle_expression(std::ostream &out, expression const& expr) {
    if (!expr.parts.empty()) {
        auto eval = evaluate_expression(expr);
        if (eval) {
            auto s = eval->to_string();
            if (s) out << *s << "\n";
        }
    }
    return handling_result::processed;
}

inline handling_result handle_expressions(std::ostream &out, std::string const& s) {
    auto expressions = parse_expressions(s);
    for (auto &expr : expressions) {
        auto r = handle_expression(out, expr);
        if (r != handling_result::processed) return r;
    }
    return handling_result::processed;
}

inline handling_result handle_command(std::ostream &out, std::string const& s) {
    if (s == "") return handling_result::empty;
    if (s[0] == ';') return handle_special_command(out, s.substr(1));
    return handle_expressions(out, s);
}

inline bool execute_command(std::ostream &out, std::string const& s) {
    try {
        auto result = cmd::handle_command(out, s);
        if (result == cmd::handling_result::exit) return false;
        else if (result == cmd::handling_result::unrecognized)
            out << "unrecognized command\n";
    }
    catch (parsing_exception &e) {
        out << "Parsing error: " << e.what() << "\n";
    }
    catch (expr_eval_exception &e) {
        out << "Expression evaluation error: " << e.what() << "\n";
    }
    catch (std::exception &e) {
        out << "Exception: " << e.what() << "\n";
    }
    return true;
}

inline void run_interaction_loop() {
    std::cout << "--- TUNABLE BEGIN ---\n";
    cmd::print_help(std::cout);
    while (true) {
        std::cout << "$ ";
        std::string s;
        std::getline(std::cin, s);
        if (!execute_command(std::cout, s)) break;
    }
    std::cout << "--- TUNABLE END ---\n";
}

} // namespace cmd

#undef _tunable_check

} // namespace _tunable_impl

// using __COUNTER__ or __LINE__ instead of variable name
// allows for binding names with special characters (e.g. *v[0]->ptr)
#ifdef __COUNTER__
#define _tunable_uniqid __COUNTER__
#else
#define _tunable_uniqid __LINE__
#endif

#define tunable_op(T1, T2) _tunable_impl::register_binary_op_types<T1,T2>()

#define _tunable_var_type(x) std::remove_reference_t<decltype(x)>

#define _tunable_member(T,M,x) _tunable_impl::member_var_factory::create<T,M>(#x, [](T& t) -> M& { return t.x; })
// tunable_member(Class,x) - register member variable Class::x as tunable
#define tunable_member(Class,x) _tunable_member(Class, _tunable_var_type(Class::x), x)

#define _tunable_var_(T,x,n) _tunable_impl::tunable _tunable_for_##n(x, #x, _tunable_impl::addr_helper_factory<T>::get())
#define _tunable_var(x,n) _tunable_var_(_tunable_var_type(x), x, n)
// tunable_var(x) - capture variable x as tunable
#define tunable_var(x) _tunable_var(x, _tunable_uniqid)

#define _tunable_1(x,...) tunable_var(x)
#define _tunable_2(x,y) tunable_member(x, y)
#define _tunable(x,y,n,...) _tunable_##n(x, y)
// tunable(x) - capture variable x as tunable
// tunable(Class,x) - register member variable Class::x as tunable
#define tunable(x,...) _tunable(x, ##__VA_ARGS__, 2, 1)

inline void tunable_cmd() {
    _tunable_impl::initialization::ensure();
    _tunable_impl::cmd::run_interaction_loop();
}

inline std::string tunable_eval(std::string const& expressions) {
    _tunable_impl::initialization::ensure();
    std::stringstream ss;
    _tunable_impl::cmd::execute_command(ss, expressions);
    return ss.str();
}

#endif