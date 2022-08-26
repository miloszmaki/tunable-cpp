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

namespace { // helpers

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

_tunable_unary_operator_checker(+, plus)
_tunable_unary_operator_checker(-, minus)
_tunable_unary_operator_checker(~, bit_neg)
_tunable_unary_operator_checker(++, incr)
_tunable_unary_operator_checker(--, decr)
_tunable_unary_operator_checker(!, neg)
_tunable_unary_operator_checker(*, deref)
_tunable_unary_operator_checker(&, addr)

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

_tunable_binary_operator_checker(=, assign)
_tunable_binary_operator_checker(+=, add_eq)
_tunable_binary_operator_checker(-=, sub_eq)
_tunable_binary_operator_checker(*=, mul_eq)
_tunable_binary_operator_checker(/=, div_eq)
_tunable_binary_operator_checker(%=, mod_eq)
_tunable_binary_operator_checker(&=, bit_and_eq)
_tunable_binary_operator_checker(|=, bit_or_eq)
_tunable_binary_operator_checker(^=, bit_xor_eq)

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
bool from_string(std::string& ref, std::string const& s) {
    if (is_quoted(s)) ref = unescape(s.substr(1, s.size()-2));
    else ref = s;
    return true;
}

template <>
bool from_string(char& ref, std::string const& s) {
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
bool from_string(bool& ref, std::string const& s) {
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
    return from_string(reinterpret_cast<unsigned long long&>(ref), s);
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
std::optional<std::string> stringify(bool const& ref) {
    return ref ? "true" : "false";
}

template <class T>
std::optional<std::string> stringify(T* const& ref) {
    if (!ref) return "nullptr";
    std::stringstream ss;
    ss << "0x" << std::hex << reinterpret_cast<uintptr_t>(ref);
    return ss.str();
}

} // helpers

namespace { // expression parsing

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
        "==", "!=", "<=", ">=", "<<", ">>", "->", "&&", "||", "::",
        "++", "--", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=",
        ".", ",", "=", "+", "-", "*", "/", "%", "&", "|", "^", "~", "!", "<", ">", "?", ":" };
        // todo: <<= >>=

    static std::string find(char const* s) {
        if (s[0] == 0) return {};
        char c1 = s[0], c2 = s[1];
        for (auto &op : types) {
            if (op[0] != c1) continue;
            if (op.size() == 1 || op[1] == c2) return op;
        }
        return {};
    }
};

struct expr_brackets {
    std::unique_ptr<expression> nested;
    char type; // [ ( {
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

std::string expr_brackets::to_string() const {
    std::string s;
    s += type; // opening bracket
    if (nested) s += nested->to_string();
    s += type + (1 + int(type != '(')); // closing bracket
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

    virtual const char* what() const noexcept { return msg.c_str(); }

private:
    std::string msg;
};

void expression::parse(char const* s, size_t& i) {
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
            if (!one_of("])}", s[i])) throw parsing_exception(parsing_error::unmatched_bracket, s, i0);
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

std::vector<expression> parse_expressions(std::string const& s) {
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

} // expression parsing

// tunable base
class tunable_base {
public:
    virtual ~tunable_base() {}
};

// allows to create new tunables
class tunable_factory {
public:
    template <class T>
    static void create(T& ref, std::string const& name);
private:
    inline static std::vector<tunable_base*> tunables;
};

namespace { // expression evaluation

enum class expr_eval_error { invalid_syntax, undefined, void_to_value, bad_value_assign, invalid_var_name, idx_out_of_bounds };

struct expr_eval_exception : public std::exception {
    expr_eval_exception(expr_eval_error error, expression const& expr, size_t part_idx) {
        static const std::map<expr_eval_error, std::string> errors{
            {expr_eval_error::invalid_syntax, "invalid syntax"},
            {expr_eval_error::undefined, "undefined"},
            {expr_eval_error::void_to_value, "void cannot be converted to value"},
            {expr_eval_error::bad_value_assign, "bad value assignment"},
            {expr_eval_error::invalid_var_name, "invalid variable name"},
            {expr_eval_error::idx_out_of_bounds, "index out of bounds"}
        };

        auto prefix = expr.to_string(0, part_idx);
        auto suffix = expr.to_string(part_idx);
        std::string indent(prefix.size(), ' ');
        std::string marker(expr.to_string(part_idx, part_idx+1).size(), '^');

        msg = errors.at(error) + "\n" +
              prefix + suffix + "\n" +
              indent + marker + "\n";
    }

    virtual const char* what() const noexcept { return msg.c_str(); }

private:
    std::string msg;
};

class expr_evaluation;
using expr_eval_ptr = std::unique_ptr<expr_evaluation>;

struct expr_eval_result {
    expr_eval_ptr ptr;
    size_t next_part_idx;
};

class expr_evaluation {
public:
    expr_evaluation(bool constant) : constant(constant) {}
    virtual ~expr_evaluation() {}

    bool is_const() const { return constant; }
    virtual std::type_index type() const = 0;
    template <class T> bool is() const { return type() == std::type_index(typeid(T)); }
    template <class T> T const& value() const;

    virtual bool is_ptr() const = 0;
    virtual std::optional<std::string> to_string() const = 0;
    virtual expr_eval_ptr create_var(std::string const& name) = 0;
    virtual std::optional<expr_eval_ptr> get_member_var(std::string const& name) const = 0;
    virtual expr_eval_ptr apply_unary_operator(std::string const& type) = 0;
    virtual expr_eval_ptr apply_binary_operator(std::string const& type, expr_eval_ptr rhs) = 0;
    virtual std::optional<expr_eval_result> evaluate_var_expression(expression const& expr, size_t part_idx) = 0;

    template <class T> static expr_eval_ptr make_lvalue(T& ref) { return make_lvalue(ref, false); }
    template <class T> static expr_eval_ptr make_lvalue(T*& ref) { return make_lvalue(ref, true); }

    template <class T> static expr_eval_ptr make_rvalue(T const& ref) { return make_rvalue(ref, false); }
    template <class T> static expr_eval_ptr make_rvalue(T* const& ref) { return make_rvalue(ref, true); }
    template <class T> static expr_eval_ptr make_rvalue_from_string(std::string const& s) {
        T x;
        if (!from_string(x, s)) throw std::runtime_error("deserialization");
        return make_rvalue(x);
    }

    static expr_eval_ptr make_void() { return {}; }

private:
    bool constant = false;

    template <class T> static expr_eval_ptr make_lvalue(T& ref, bool is_ptr);
    template <class T> static expr_eval_ptr make_rvalue(T const& ref, bool is_ptr);
};

// allows to call binary operators on arbitrary types
template <class TL>
class binary_operators {
public:
    template <class TR>
    static void register_type() {
        std::type_index type = typeid(TR);
        auto it = op_maps.find(type);
        if (it != op_maps.end()) return;

        auto &ops = op_maps[type];

#define _tunable_register_binary_op(op, name, lvalue) \
            if constexpr (has_binary_operator_##name<TL,TR>::value) { \
                ops[#op] = [&](TL& lhs, expr_eval_ptr const& rhs, bool lhs_is_const) -> expr_eval_ptr { \
                    if constexpr (lvalue) { \
                        if (lhs_is_const) throw std::runtime_error("can't apply on a constant"); \
                        return expr_evaluation::make_lvalue(lhs op rhs->value<TR>()); \
                    } \
                    else return expr_evaluation::make_rvalue((TL const&)lhs op rhs->value<TR>()); \
                }; \
            }
#define _tunable_register_binary_op_rvalue(op, name) _tunable_register_binary_op(op, name, false)
#define _tunable_register_binary_op_lvalue(op, name) _tunable_register_binary_op(op, name, true)

        _tunable_register_binary_op_rvalue(+, plus)
        _tunable_register_binary_op_rvalue(-, minus)
        _tunable_register_binary_op_rvalue(*, mul)
        _tunable_register_binary_op_rvalue(/, div)
        _tunable_register_binary_op_rvalue(%, mod)
        _tunable_register_binary_op_rvalue(==, eq)
        _tunable_register_binary_op_rvalue(!=, neq)
        _tunable_register_binary_op_rvalue(<, lt)
        _tunable_register_binary_op_rvalue(<=, le)
        _tunable_register_binary_op_rvalue(>, gt)
        _tunable_register_binary_op_rvalue(>=, ge)
        _tunable_register_binary_op_rvalue(&&, and)
        _tunable_register_binary_op_rvalue(||, or)
        _tunable_register_binary_op_rvalue(&, bit_and)
        _tunable_register_binary_op_rvalue(|, bit_or)
        _tunable_register_binary_op_rvalue(^, bit_xor)

        _tunable_register_binary_op_lvalue(=, assign)
        _tunable_register_binary_op_lvalue(+=, add_eq)
        _tunable_register_binary_op_lvalue(-=, sub_eq)
        _tunable_register_binary_op_lvalue(*=, mul_eq)
        _tunable_register_binary_op_lvalue(/=, div_eq)
        _tunable_register_binary_op_lvalue(%=, mod_eq)
        _tunable_register_binary_op_lvalue(&=, bit_and_eq)
        _tunable_register_binary_op_lvalue(|=, bit_or_eq)
        _tunable_register_binary_op_lvalue(^=, bit_xor_eq)

        // todo: "<<", ">>", "<<=", ">>=", "::", ",", "?", ":"

#undef _tunable_register_binary_op_lvalue
#undef _tunable_register_binary_op_rvalue
#undef _tunable_register_binary_op
    }

    static std::optional<expr_eval_ptr> call(std::string const& op_type, TL& lhs, expr_eval_ptr const& rhs, bool lhs_is_const) {
        auto op_map_it = op_maps.find(rhs->type());
        if (op_map_it == op_maps.end()) return std::nullopt;
        auto &ops = op_map_it->second;
        auto op_func_it = ops.find(op_type);
        if (op_func_it == ops.end()) return std::nullopt;
        auto &op = op_func_it->second;
        return op(lhs, rhs, lhs_is_const);
    }

private:
    using op_func = std::function<expr_eval_ptr(TL&, expr_eval_ptr const&, bool)>;
    using op_map = std::map<std::string, op_func>;
    inline static std::map<std::type_index, op_map> op_maps;
};

template <class T>
class expr_eval_typed : public expr_evaluation {
public:
    expr_eval_typed(T* ptr, bool owned, bool constant, bool value_is_ptr) : expr_evaluation(constant), ptr(ptr), owned(owned), value_is_ptr(value_is_ptr) {}

    virtual ~expr_eval_typed() {
        if (owned && ptr) delete ptr;
    }

    virtual std::type_index type() const { return typeid(T); }

    T const& value() const { return *ptr; }

    virtual bool is_ptr() const { return value_is_ptr; }

    virtual std::optional<std::string> to_string() const {
        return stringify(*ptr);
    }

    virtual expr_eval_ptr create_var(std::string const& name) {
        T* ptr2 = nullptr;
        if (owned) { // move
            ptr2 = ptr;
            owned = false; // prevent deletion
        }
        else { // copy
            ptr2 = new T(value());
        }
        T& ref2 = *ptr2;
        tunable_factory::create(ref2, name);
        return expr_evaluation::make_lvalue(ref2);
    }

    virtual std::optional<expr_eval_ptr> get_member_var(std::string const& name) const;

    virtual expr_eval_ptr apply_unary_operator(std::string const& type) {
#define _tunable_apply_unary_op(op, name, lvalue) \
        if (type == #op) { \
            if constexpr (has_unary_operator_##name<T>::value) { \
                if constexpr (lvalue) { \
                    if (is_const()) throw std::runtime_error("can't apply on a constant"); \
                    return expr_evaluation::make_lvalue(op(*ptr)); \
                } \
                else return expr_evaluation::make_rvalue(op(*ptr)); \
            } \
        }
             _tunable_apply_unary_op(+, plus, false)
        else _tunable_apply_unary_op(-, minus, false)
        else _tunable_apply_unary_op(~, bit_neg, false)
        else _tunable_apply_unary_op(++, incr, true)
        else _tunable_apply_unary_op(--, decr, true)
        else _tunable_apply_unary_op(!, neg, false)
        else _tunable_apply_unary_op(*, deref, true)
        // else _tunable_apply_unary_op(&, addr, false) // todo: limit the infinite compilation

#undef _tunable_apply_unary_op
        throw std::runtime_error("invalid operator");
    }

    virtual expr_eval_ptr apply_binary_operator(std::string const& type, expr_eval_ptr rhs) {
        auto eval = binary_operators<T>::call(type, *ptr, rhs, is_const());
        if (eval) return std::move(*eval);

        if (type == "=") {
            if (value_is_ptr && rhs->is_ptr())
                throw std::runtime_error("implicit casting of pointers of different type");
            auto str = rhs->to_string(); // serialize
            if (!str) throw std::runtime_error("serialization error");
            // deserialize
            if (!from_string(*ptr, *str)) throw std::runtime_error("deserialization error");
            return make_lvalue(*ptr);
        }

        throw std::runtime_error("invalid operator");

        // todo: throw expr_eval_exception with proper error, expr and part_idx
    }

    virtual std::optional<expr_eval_result> evaluate_var_expression(expression const& expr, size_t part_idx);

private:
    T* ptr = nullptr;
    bool owned = false, value_is_ptr = false;
};

template <class T>
T const& expr_evaluation::value() const{
    return reinterpret_cast<expr_eval_typed<T> const*>(this)->value();
}

template <class T>
expr_eval_ptr expr_evaluation::make_lvalue(T& ref, bool is_ptr) {
    return std::make_unique<expr_eval_typed<T>>(&ref, false, false, is_ptr);
}

template <class T>
expr_eval_ptr expr_evaluation::make_rvalue(T const& ref, bool is_ptr) {
    return std::make_unique<expr_eval_typed<T>>(new T(ref), true, true, is_ptr);
}

} // expression evaluation

// holds a reference to a tunable variable
template <class T>
class tunable : public tunable_base {
public:
    T& ref;
    tunable(T &var, std::string const& name, bool is_member = false) : ref(var), name(name) {
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

template <class T>
void tunable_factory::create(T& ref, std::string const& name) {
    tunables.emplace_back(new tunable<T>(ref, name));
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
    template <class T>
    friend class tunable_type;
};

// implements variable search and assignment
// holds a mapping from the names to the tunables for all variables of a given type
template <class T>
class tunable_type : public tunable_type_base {
public:
    virtual std::optional<expr_eval_ptr> get_var_eval(std::string const& name) {
        auto &self = get_instance();
        auto var = self.find_var_typed(name);
        if (!var) return std::nullopt;
        return expr_evaluation::make_lvalue(var->ref);
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

    template <class U>
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
    virtual expr_eval_ptr get_var_eval(T& ref) {
        auto& member_ref = get_ref(ref);
        return expr_evaluation::make_lvalue(member_ref);
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

// allows to register class member variables as tunables
class member_var_factory {
public:
    template <class T, class M>
    static void create(std::string const& name, M& (*get_ref)(T&)) {
        members.emplace_back(new member_var<T,M>(name, get_ref));
    }
private:
    inline static std::vector<member_var_base*> members;
};

// register pairs of types for binary operator evaluation
template <class T1, class T2>
void register_binary_op_types() {
    binary_operators<T1>::template register_type<T2>();
    if constexpr (!std::is_same<T1,T2>::value)
        binary_operators<T2>::template register_type<T1>();
}

namespace { // expression evaluation

// process prefix parts of name split by operators (`.`, `->` and `::`) and brackets
std::optional<expr_eval_result> process_var_name_prefixes(expression const& expr, size_t part_idx, std::function<std::optional<expr_eval_result>(std::string const&, size_t)> func) {
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

expr_eval_ptr evaluate_expression(expression const& expr, size_t part_idx);

template <class T>
std::optional<expr_eval_ptr> expr_eval_typed<T>::get_member_var(std::string const& name) const {
    auto var = member_vars<T>::find_member(name);
    if (!var) return std::nullopt;
    auto var_eval = var->get_var_eval(*ptr);
    if (!var_eval) throw std::runtime_error("impossible");
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
    auto idx_eval = evaluate_expression(expr, 0);
    if (!idx_eval) throw expr_eval_exception(expr_eval_error::void_to_value, expr, 0);
    auto str = idx_eval->to_string();
    if (!str) throw expr_eval_exception(expr_eval_error::invalid_syntax, expr, 0);
    size_t idx = -1;
    if (!is_integer(*str) || !from_string(idx, *str)) throw expr_eval_exception(expr_eval_error::invalid_syntax, expr, 0);
    if (idx >= size) throw expr_eval_exception(expr_eval_error::idx_out_of_bounds, expr, 0);
    return expr_evaluation::make_lvalue(ref[idx]);
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
                        auto eval = evaluate_expression(*args.nested, 0);
                        if (!eval) throw expr_eval_exception(expr_eval_error::void_to_value, *args.nested, 0);
                        if (eval->is<T>()) {
                            ref.push_back(eval->value<T>());
                            return expr_evaluation::make_void();
                        }
                        throw expr_eval_exception(expr_eval_error::invalid_syntax, *args.nested, 0);
                    }
                    else if (method.name == "resize") {
                        // todo: support multiple arguments
                        auto eval = evaluate_expression(*args.nested, 0);
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
                    if (method.name == "size") return expr_evaluation::make_rvalue(ref.size());
                    else if (method.name == "capacity") return expr_evaluation::make_rvalue(ref.capacity());
                    else if (method.name == "empty") return expr_evaluation::make_rvalue(ref.empty());
                    else if (method.name == "front") return expr_evaluation::make_lvalue(ref.front());
                    else if (method.name == "back") return expr_evaluation::make_lvalue(ref.back());
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
    auto &part = expr.parts[part_idx];
    if (std::holds_alternative<expr_operator>(part)) {
        auto &op = std::get<expr_operator>(part).type;
        if (op == "->") {
            return evaluate_var_member(expr_evaluation::make_lvalue(*ref), expr, part_idx + 1);
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
    return std::nullopt;
}

template <class T>
std::optional<expr_eval_result> expr_eval_typed<T>::evaluate_var_expression(expression const& expr, size_t part_idx) {
    return evaluate_typed_var_expr(*ptr, expr, part_idx);
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
            if (!opt_var_eval) throw std::runtime_error("impossible");
            auto var_eval = std::move(*opt_var_eval);
            if (!var_eval) throw std::runtime_error("impossible");
            // evaluate suffix (member variables and specialized operators and methods)
            while (suffix_idx < expr.parts.size()) {
                auto ret = evaluate_var_expression(std::move(var_eval), expr, suffix_idx);
                var_eval = std::move(ret.ptr);
                if (ret.next_part_idx == suffix_idx) break;
                suffix_idx = ret.next_part_idx;
            }
            return expr_eval_result{std::move(var_eval), suffix_idx};
        })
    );
    if (ret) return std::move(*ret);

    auto &part = expr.parts[part_idx];
    if (std::holds_alternative<expr_variable>(part)) { // we've got an undefined variable
        auto &var = std::get<expr_variable>(part);
        if (expr.parts.size() == 1) throw expr_eval_exception(expr_eval_error::undefined, expr, part_idx); // nothing more, so return an error

        auto &part2 = expr.parts[part_idx + 1];
        if (std::holds_alternative<expr_operator>(part2)) {
            auto &op = std::get<expr_operator>(part2).type;
            if (op == "=") { // create a new variable
                auto eval = evaluate_expression(expr, part_idx + 2);
                if (!eval) throw expr_eval_exception(expr_eval_error::void_to_value, expr, part_idx + 2);
                return { eval->create_var(var.name), expr.parts.size() };
            }
        }
        throw expr_eval_exception(expr_eval_error::invalid_syntax, expr, part_idx + 1);
    }
    else if (std::holds_alternative<expr_constant>(part)) {
        auto cnst = std::get<expr_constant>(part);
        auto create_const = [&](){
            try { // create new rvalue
                switch(cnst.type) {
                    case expr_const_type::_int: return expr_evaluation::make_rvalue_from_string<long long>(cnst.value);
                    case expr_const_type::_real: return expr_evaluation::make_rvalue_from_string<double>(cnst.value);
                    case expr_const_type::_bool: return expr_evaluation::make_rvalue_from_string<bool>(cnst.value);
                    case expr_const_type::_char: return expr_evaluation::make_rvalue_from_string<char>(cnst.value);
                    case expr_const_type::_string: return expr_evaluation::make_rvalue_from_string<std::string>(cnst.value);
                    case expr_const_type::_nullptr: return expr_evaluation::make_rvalue(nullptr);
                }
            }
            catch (std::exception &e) {}
            throw expr_eval_exception(expr_eval_error::invalid_syntax, expr, part_idx);
        };
        return { create_const(), part_idx + 1 };
    }
    throw expr_eval_exception(expr_eval_error::invalid_syntax, expr, part_idx);
}

inline expr_eval_ptr evaluate_unary_op_expression(expression const& expr, size_t part_idx) {
    if (part_idx >= expr.parts.size()) throw expr_eval_exception(expr_eval_error::invalid_syntax, expr, part_idx);

    auto &part = expr.parts[part_idx];
    if (std::holds_alternative<expr_operator>(part)) {
        auto& op = std::get<expr_operator>(part);
        auto eval = evaluate_expression(expr, part_idx + 1);
        if (!eval) throw expr_eval_exception(expr_eval_error::invalid_syntax, expr, part_idx);
        try {
            return eval->apply_unary_operator(op.type);
        }
        catch (std::exception &e) {
            throw expr_eval_exception(expr_eval_error::invalid_syntax, expr, part_idx);
        }
    }

    throw expr_eval_exception(expr_eval_error::invalid_syntax, expr, part_idx);
}

// todo: process next part (which should be an inner operator or end of expr, otherwise fail)
inline expr_eval_ptr evaluate_binary_op_expression(expr_eval_ptr var, expression const& expr, size_t part_idx) {
    if (part_idx >= expr.parts.size()) return var;

    auto &part = expr.parts[part_idx];
    if (std::holds_alternative<expr_operator>(part)) {
        auto& op = std::get<expr_operator>(part);
        // todo: postfix `++`, `--`
        auto eval = evaluate_expression(expr, part_idx + 1);
        if (!eval) throw expr_eval_exception(expr_eval_error::invalid_syntax, expr, part_idx);
        try {
            return var->apply_binary_operator(op.type, std::move(eval));
        }
        catch (std::exception &e) {
            throw expr_eval_exception(expr_eval_error::invalid_syntax, expr, part_idx);
        }
    }

    throw expr_eval_exception(expr_eval_error::invalid_syntax, expr, part_idx);
}

inline expr_eval_ptr evaluate_expression(expression const& expr, size_t part_idx) {
    if (part_idx >= expr.parts.size()) throw expr_eval_exception(expr_eval_error::invalid_syntax, expr, part_idx);

    auto &part = expr.parts[part_idx];
    if (std::holds_alternative<expr_operator>(part)) {
        return evaluate_unary_op_expression(expr, part_idx);
    }
    else if (std::holds_alternative<expr_brackets>(part)) {
        auto& brackets = std::get<expr_brackets>(part);
        if (brackets.type == '(') {
            auto eval = evaluate_expression(*brackets.nested, 0);
            return evaluate_binary_op_expression(std::move(eval), expr, part_idx + 1);
        }
    }
    else { // variable or constant
        auto eval = evaluate_value_expression(expr, part_idx);
        return evaluate_binary_op_expression(std::move(eval.ptr), expr, eval.next_part_idx);
    }

    throw expr_eval_exception(expr_eval_error::invalid_syntax, expr, part_idx);
}

} // expression evaluation

class interaction_loop
{
public:
    static void run() {
        init();
        std::cout << "--- TUNABLE BEGIN ---\n";
        print_help();
        while (true) {
            std::cout << "$ ";
            std::string s;
            std::getline(std::cin, s);
            try {
                auto result = handle_command(s);
                if (result == cmd_handling_result::exit) break;
                else if (result == cmd_handling_result::unrecognized)
                    std::cout << "unrecognized command\n";
            }
            catch (parsing_exception &e) {
                std::cout << "Parsing error: " << e.what() << "\n";
            }
            catch (expr_eval_exception &e) {
                std::cout << "Expression evaluation error: " << e.what() << "\n";
            }
            catch (std::exception &e) {
                std::cout << "Exception: " << e.what() << "\n";
            }
        }
        std::cout << "--- TUNABLE END ---\n";
    }
private:
    enum class cmd_handling_result { unrecognized, empty, processed, exit };

    static void init() {
#define _tunable_register_binary_op_types_numeric(i, T) \
        if constexpr(i <= 1)  register_binary_op_types<T, char>(); \
        if constexpr(i <= 2)  register_binary_op_types<T, unsigned char>(); \
        if constexpr(i <= 3)  register_binary_op_types<T, short int>(); \
        if constexpr(i <= 4)  register_binary_op_types<T, unsigned short int>(); \
        if constexpr(i <= 5)  register_binary_op_types<T, int>(); \
        if constexpr(i <= 6)  register_binary_op_types<T, unsigned int>(); \
        if constexpr(i <= 7)  register_binary_op_types<T, long int>(); \
        if constexpr(i <= 8)  register_binary_op_types<T, unsigned long int>(); \
        if constexpr(i <= 9)  register_binary_op_types<T, long long>(); \
        if constexpr(i <= 10) register_binary_op_types<T, unsigned long long>(); \
        if constexpr(i <= 11) register_binary_op_types<T, float>(); \
        if constexpr(i <= 12) register_binary_op_types<T, double>(); \
        if constexpr(i <= 13) register_binary_op_types<T, long double>();

        _tunable_register_binary_op_types_numeric(1,  char)
        _tunable_register_binary_op_types_numeric(2,  unsigned char)
        _tunable_register_binary_op_types_numeric(3,  short int)
        _tunable_register_binary_op_types_numeric(4,  unsigned short int)
        _tunable_register_binary_op_types_numeric(5,  int)
        _tunable_register_binary_op_types_numeric(6,  unsigned int)
        _tunable_register_binary_op_types_numeric(7,  long int)
        _tunable_register_binary_op_types_numeric(8,  unsigned long int)
        _tunable_register_binary_op_types_numeric(9,  long long)
        _tunable_register_binary_op_types_numeric(10, unsigned long long)
        _tunable_register_binary_op_types_numeric(11, float)
        _tunable_register_binary_op_types_numeric(12, double)
        _tunable_register_binary_op_types_numeric(13, long double)

#undef _tunable_register_binary_op_types_numeric

        register_binary_op_types<std::string, char>();
    }

    static void print_help() {
        const int col = 20;
        std::cout << "Special commands:\n" <<
            std::setw(col) << ";q ;quit ;exit" << " - quit tunable command line\n" <<
            std::setw(col) << ";h ;help" << " - show help\n" <<
            std::setw(col) << ";vars" << " - show all variables\n" <<
            std::setw(col) << ";values" << " - show all variables with values\n";
    }

    static cmd_handling_result handle_command(std::string const& s) {
        if (s == "") return cmd_handling_result::empty;
        if (s[0] == ';') return handle_special_command(s.substr(1));
        return handle_expressions(s);
    }

    static cmd_handling_result handle_special_command(std::string const& s) {
        if (s == "q" || s == "quit" || s == "exit")
            return cmd_handling_result::exit;
        if (s == "h" || s == "help") {
            print_help();
            return cmd_handling_result::processed;
        }
        if (s == "vars") {
            for (auto &[name,_] : tunable_types::get_var_types()) {
                std::cout << name << "\n";
            }
            return cmd_handling_result::processed;
        }
        if (s == "values") {
            for (auto &[name,_] : tunable_types::get_var_types()) {
                expression expr;
                expr.parts.emplace_back(expr_variable{.name=name});
                auto value = evaluate_expression(expr, 0);
                if (value) {
                    auto val_str = value->to_string();
                    if (val_str) std::cout << name << "=" << *val_str << "\n";
                }
            }
            return cmd_handling_result::processed;
        }
        return cmd_handling_result::unrecognized;
    }

    static cmd_handling_result handle_expressions(std::string const& s) {
        auto expressions = parse_expressions(s);
        for (auto &expr : expressions) {
            auto r = handle_expression(expr);
            if (r != cmd_handling_result::processed) return r;
        }
        return cmd_handling_result::processed;
    }

    static cmd_handling_result handle_expression(expression const& expr) {
        if (expr.parts.empty()) return cmd_handling_result::processed;
        auto eval = evaluate_expression(expr, 0);
        if (eval) {
            auto s = eval->to_string();
            if (s) std::cout << *s << "\n";
        }
        return cmd_handling_result::processed;
    }
};

} // namespace _tunable_impl

// using __COUNTER__ or __LINE__ instead of variable name
// allows for binding names with special characters (e.g. *v[0]->ptr)
#ifdef __COUNTER__
#define _tunable_uniqid __COUNTER__
#else
#define _tunable_uniqid __LINE__
#endif

#define tunable_op(T1, T2) _tunable_impl::register_binary_op_types<T1,T2>()

#define _tunable_var_type(x) std::remove_reference<decltype(x)>::type

#define _tunable_member(T,M,x) _tunable_impl::member_var_factory::create<T,M>(#x, [](T& t) -> M& { return t.x; })
// tunable_member(Class,x) - register member variable Class::x as tunable
#define tunable_member(Class,x) _tunable_member(Class, _tunable_var_type(Class::x), x)

#define _tunable_var_(T,x,n) _tunable_impl::tunable _tunable_for_##n(x, #x); tunable_op(T,T)
#define _tunable_var(x,n) _tunable_var_(_tunable_var_type(x), x, n)
// tunable_var(x) - capture variable x as tunable
#define tunable_var(x) _tunable_var(x, _tunable_uniqid)

#define _tunable_1(x,...) tunable_var(x)
#define _tunable_2(x,y) tunable_member(x, y)
#define _tunable(x,y,n,...) _tunable_##n(x, y)
// tunable(x) - capture variable x as tunable
// tunable(Class,x) - register member variable Class::x as tunable
#define tunable(x,...) _tunable(x, ##__VA_ARGS__, 2, 1)

#define tunablecmd() _tunable_impl::interaction_loop::run()

#endif