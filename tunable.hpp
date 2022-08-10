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
struct is_out_streamable<T, std::void_t<decltype(std::cout << *(T*)0)>> : std::true_type {};

template <typename T, class = void>
struct is_in_streamable : std::false_type {};
template <typename T>
struct is_in_streamable<T, std::void_t<decltype(std::cin >> *(T*)0)>> : std::true_type {};

inline bool is_quoted(std::string const& s) {
    return s.size() >= 2 && s[0]=='"' && s.back() == '"';
}

inline bool is_integer(std::string const& s) {
    return std::regex_match(s, std::regex("[-+]?\\d+"));
}

inline bool is_hex(std::string const& s) {
    return std::regex_match(s, std::regex("0[xX][0-9a-fA-F]+"));
}

inline bool is_bool(std::string const& s) {
    return s == "true" || s == "false";
}

inline bool is_double(std::string const& s) {
    return std::regex_match(s, std::regex("[-+]?(\\d+\\.\\d*|\\.\\d+)"));
}

inline bool is_valid_var_name(std::string const& s) {
    return std::regex_match(s, std::regex("[_a-zA-Z][_a-zA-Z0-9]*"));
}

inline std::string parse_quoted_string(std::string s) {
    s = s.substr(1, s.size()-2);
    static const std::vector<std::pair<std::string,std::string>> escapes{
        {"\\\\\\\\", "\\"}, {"\\\\n", "\n"}, {"\\\\t", "\t"}, {"\\\\\"", "\""}, {"\\\\r", "\r"}};
    for (auto &[c,r] : escapes)
        s = std::regex_replace(s, std::regex(c), r);
    return s;
}

// find first position of c in s (but not within "..."), starting at pos
// use quoted=true to search only within "..."
inline size_t find_not_quoted(std::string const& s, char c, size_t pos = 0, bool quoted = false) {
    for (; pos < s.size(); pos++) {
        if (s[pos] == '"' && (pos == 0 || s[pos-1] != '\\')) quoted = !quoted;
        else if (s[pos] == c && !quoted) return pos;
    }
    return std::string::npos;
}

// split s with delimiter
inline std::vector<std::string> split(std::string const& s, char delimiter) {
    size_t last = 0, next = 0;
    std::vector<std::string> parts;
    while ((next = s.find(delimiter, last)) != std::string::npos) {
        parts.emplace_back(s.substr(last, next-last));
        last = next + 1;
    }
    parts.emplace_back(s.substr(last));
    return parts;
}

// split s by delimiter (which is not within "...")
inline std::vector<std::string> split_not_quoted(std::string const& s, char delimiter) {
    size_t last = 0, next = 0;
    std::vector<std::string> parts;
    while ((next = find_not_quoted(s, delimiter, last)) != std::string::npos) {
        parts.emplace_back(s.substr(last, next-last));
        last = next + 1;
    }
    parts.emplace_back(s.substr(last));
    return parts;
}

// process prefix parts of name split by .
// todo: support [] -> etc.
template <class T>
std::optional<T> process_var_name_prefixes(std::string const& name, std::function<std::optional<T>(std::string const&,std::string const&)> func) {
    // todo: stop on first non-quoted (and non-nested within brackets) =, +, - (but not ->), etc.
    // it would be optimal to jump through any (), but do we even support capturing names with () inside?
    for (size_t i=0; i<name.size(); i++) {
        if (name[i] == '=') { // todo: fix
            auto prefix = name.substr(0, i);
            auto suffix = name.substr(i);
            return func(prefix, suffix);
        }
        if (name[i] == '.' || name[i] == '[' || name[i] == ']' || name[i] == '-' || name[i] == '+') { // todo: handle nested brackets properly
            auto prefix = name.substr(0, i);
            auto suffix = name.substr(i);
            auto ret = func(prefix, suffix);
            if (ret.has_value()) return ret;
        }
    }
    return func(name, "");
}

template <class T>
bool from_string(T& ref, std::string const& s) {
    if constexpr(is_in_streamable<T>::value) {
        std::stringstream ss(s);
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
    if (is_quoted(s)) ref = parse_quoted_string(s);
    else ref = s;
    return true;
}

template <>
bool from_string(size_t& ref, std::string const& s) {
    if (!is_integer(s)) return false;
    std::stringstream ss(s);
    ss >> ref;
    return true;
}

template <>
bool from_string(bool& ref, std::string const& s) {
    if (is_integer(s)) {
        std::stringstream ss(s);
        ss >> ref;
        return true;
    }
    if (s == "true") ref = true;
    else if (s == "false") ref = false;
    else return false;
    return true;
}

template <class T>
bool from_string(T*& ref, std::string const& s) {
    if (s == "nullptr" || s == "NULL") {
        ref = nullptr;
        return true;
    }
    bool hex = is_hex(s);
    if (!hex && !is_integer(s)) return false;
    std::stringstream ss(s);
    if (hex) ss >> std::hex;
    ss >> reinterpret_cast<size_t&>(ref);
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

// holds a reference to a tunable variable
template <class T>
class tunable {
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

enum class var_expr_eval_error { invalid_syntax, undefined, void_to_value, bad_value_assign, invalid_var_name, idx_out_of_bounds };

std::string to_string(var_expr_eval_error e) {
    static const std::map<var_expr_eval_error, std::string> errors{
        {var_expr_eval_error::invalid_syntax, "invalid syntax"},
        {var_expr_eval_error::undefined, "undefined"},
        {var_expr_eval_error::void_to_value, "void cannot be converted to value"},
        {var_expr_eval_error::bad_value_assign, "bad value assignment"},
        {var_expr_eval_error::invalid_var_name, "invalid variable name"},
        {var_expr_eval_error::idx_out_of_bounds, "index out of bounds"}
    };
    return errors.at(e);
}

class var_expr_eval {
public:
    var_expr_eval(bool is_ptr) : value_is_ptr(is_ptr) {}
    virtual ~var_expr_eval() {}

    template <class T> bool is() const { return type() == std::type_index(typeid(T)); }

    template <class T> T const& value() const;

    virtual std::optional<std::string> to_string() const = 0;
    virtual std::unique_ptr<var_expr_eval> create_var(std::string const& name) = 0;

    template <class T> static std::unique_ptr<var_expr_eval> make_lvalue(T& ref) { return make_lvalue(ref, false); }
    template <class T> static std::unique_ptr<var_expr_eval> make_lvalue(T*& ref) { return make_lvalue(ref, true); }

    template <class T> static std::unique_ptr<var_expr_eval> make_rvalue(T const& ref) { return make_rvalue(ref, false); }
    template <class T> static std::unique_ptr<var_expr_eval> make_rvalue(T* const& ref) { return make_rvalue(ref, true); }

    static std::unique_ptr<var_expr_eval> make_void() { return {}; }

    template <class T> bool assign_to(T& ref) const { return assign_to(ref, false); }
    template <class T> bool assign_to(T*& ref) const { return assign_to(ref, true); }

private:
    bool value_is_ptr = false;

    virtual std::type_index type() const = 0;

    template <class T> static std::unique_ptr<var_expr_eval> make_lvalue(T& ref, bool is_ptr);
    template <class T> static std::unique_ptr<var_expr_eval> make_rvalue(T const& ref, bool is_ptr);

    template <class T>
    bool assign_to(T& ref, bool ref_is_ptr) const {
        if (is<T>()) {
            // if types match assign directly
            ref = value<T>(); // todo: move if ptr owned?
        }
        else {
            if (ref_is_ptr && value_is_ptr) return false; // forbid implicit casting of pointers of different type
            auto str = to_string(); // serialize
            if (!str) return false;
             // deserialize
            if (!from_string(ref, *str)) return false;
        }
        return true;
    }
};

template <class T>
class var_expr_eval_typed : public var_expr_eval {
public:
    var_expr_eval_typed(T* ptr, bool owned, bool value_is_ptr) : var_expr_eval(value_is_ptr), ptr(ptr), owned(owned) {}

    virtual ~var_expr_eval_typed() {
        if (owned && ptr) delete ptr;
    }

    T const& value() const { return *ptr; }

    virtual std::optional<std::string> to_string() const {
        return stringify(*ptr);
    }

    virtual std::unique_ptr<var_expr_eval> create_var(std::string const& name) {
        T* ptr2 = nullptr;
        if (owned) { // move
            ptr2 = ptr;
            owned = false; // prevent deletion
        }
        else { // copy
            ptr2 = new T(value());
        }
        T& ref2 = *ptr2;
        new tunable<T>(ref2, name);
        return var_expr_eval::make_lvalue(ref2);
    }

private:
    T* ptr = nullptr;
    bool owned = false;

    virtual std::type_index type() const { return typeid(T); }
};

template <class T>
T const& var_expr_eval::value() const{
    return reinterpret_cast<var_expr_eval_typed<T> const*>(this)->value();
}

template <class T>
std::unique_ptr<var_expr_eval> var_expr_eval::make_lvalue(T& ref, bool is_ptr) {
    return std::make_unique<var_expr_eval_typed<T>>(&ref, false, is_ptr);
}

template <class T>
std::unique_ptr<var_expr_eval> var_expr_eval::make_rvalue(T const& ref, bool is_ptr) {
    return std::make_unique<var_expr_eval_typed<T>>(new T(ref), true, is_ptr);
}

using var_expr_eval_ptr = std::unique_ptr<var_expr_eval>;

using var_expr_eval_result = std::variant<var_expr_eval_ptr, var_expr_eval_error>;

template <class T>
var_expr_eval_result evaluate_var_expression(T& ref, std::string const& suffix);

// interface for variable search and assignment
class tunable_type_base {
public:
    virtual ~tunable_type_base() {}
    virtual var_expr_eval_result evaluate_var_expr(std::string const& name, std::string const& suffix) = 0;
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
    virtual var_expr_eval_result evaluate_var_expr(std::string const& name, std::string const& suffix) {
        auto &self = get_instance();
        auto var = self.find_var_typed(name);
        if (!var) return var_expr_eval_error::undefined;
        return evaluate_var_expression(var->ref, suffix);
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
    virtual var_expr_eval_result evaluate_expression(T& ref, std::string const& suffix) = 0;
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
    virtual var_expr_eval_result evaluate_expression(T& ref, std::string const& suffix) {
        auto& member_ref = get_ref(ref);
        return evaluate_var_expression(member_ref, suffix);
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


var_expr_eval_result evaluate_expression(std::string const& expr, bool for_assignment = false);

size_t find_closing_bracket(std::string const& s, char close_bracket, size_t i = 0) {
    if (i >= s.size()) return std::string::npos;
    char open_bracket = s[i++];
    for (size_t o=1; i<s.size(); i++) {
        if (s[i] == open_bracket) ++o;
        else if (s[i] == close_bracket) {
            if (o == 0) return std::string::npos;
            if (--o == 0) break;
        }
    }
    if (i == s.size()) return std::string::npos;
    return i;
}

std::pair<size_t, size_t> find_brackets(std::string const& s, char open_bracket, char close_bracket, size_t i = 0) {
    auto r = std::make_pair(std::string::npos, std::string::npos);
    r.first = s.find(open_bracket, i);
    r.second = find_closing_bracket(s, close_bracket, r.first);
    return r;
}

template <class T>
var_expr_eval_result evaluate_var_assignment(T& ref, std::string const &suffix) {
    auto eval = evaluate_expression(suffix, true);
    if (!std::holds_alternative<var_expr_eval_ptr>(eval)) return eval;
    auto& ptr = std::get<var_expr_eval_ptr>(eval);
    if (!ptr || !ptr->assign_to(ref)) return var_expr_eval_error::bad_value_assign;
    return var_expr_eval::make_lvalue(ref);
}

template <class T>
var_expr_eval_result evaluate_var_member(T& ref, std::string const &suffix) {
    auto ret = process_var_name_prefixes(suffix,
        std::function([&](std::string const& prefix, std::string const& suffix) -> std::optional<var_expr_eval_result> {
            auto var = member_vars<T>::find_member(prefix);
            if (!var) return std::nullopt;
            auto ret = var->evaluate_expression(ref, suffix);
            if (std::holds_alternative<var_expr_eval_ptr>(ret)) return ret;
            return std::nullopt;
        })
    );
    if (ret) return std::move(*ret);
    return var_expr_eval_error::undefined;
}

template <class T>
std::optional<var_expr_eval_result> evaluate_typed_var_expr(T& ref, std::string const& suffix) {
    return std::nullopt;
}

template <class T>
var_expr_eval_result evaluate_subscript(T& ref, size_t size, std::string const& suffix) {
    int i = find_closing_bracket(suffix, ']');
    if (i <= 1) return var_expr_eval_error::invalid_syntax;
    auto idx_eval = evaluate_expression(suffix.substr(1, i-1));
    if (!std::holds_alternative<var_expr_eval_ptr>(idx_eval)) return idx_eval;
    auto& idx_ptr = std::get<var_expr_eval_ptr>(idx_eval);
    if (!idx_ptr) return var_expr_eval_error::void_to_value;
    auto str = idx_ptr->to_string();
    if (!str) return var_expr_eval_error::invalid_syntax;
    size_t idx = -1;
    if (!from_string(idx, *str)) return var_expr_eval_error::invalid_syntax;
    if (idx >= size) return var_expr_eval_error::idx_out_of_bounds;
    return evaluate_var_expression(ref[idx], suffix.substr(i+1));
}

template <class T>
std::optional<var_expr_eval_result> evaluate_typed_var_expr(std::vector<T>& ref, std::string const& suffix) {
    if (suffix[0] == '[') {
        return evaluate_subscript(ref, ref.size(), suffix);
    }
    if (suffix[0] == '.') {
        auto [b1, b2] = find_brackets(suffix, '(', ')', 1);
        if (b2 != std::string::npos) {
            auto name = suffix.substr(1, b1-1);
            auto args = suffix.substr(b1+1, b2-b1-1);
            // methods with arguments
            if (name == "push_back") {
                auto eval = evaluate_expression(args);
                if (!std::holds_alternative<var_expr_eval_ptr>(eval)) return eval;
                auto& ptr = std::get<var_expr_eval_ptr>(eval);
                if (!ptr) return var_expr_eval_error::void_to_value;
                if (ptr->is<T>()) {
                    ref.push_back(ptr->value<T>());
                    return var_expr_eval::make_void();
                }
                return var_expr_eval_error::invalid_syntax;
            }
            else if (name == "resize") {
                auto eval = evaluate_expression(args);
                if (!std::holds_alternative<var_expr_eval_ptr>(eval)) return eval;
                auto& ptr = std::get<var_expr_eval_ptr>(eval);
                if (!ptr) return var_expr_eval_error::void_to_value;
                // todo: support multiple arguments
                auto s = ptr->to_string();
                if (s && is_integer(*s)) {
                    ref.resize(std::stoll(*s));
                    return var_expr_eval::make_void();
                }
                return var_expr_eval_error::invalid_syntax;
            }
            // methods without arguments
            if (args != "") return var_expr_eval_error::invalid_syntax;
            if (name == "size") {
                // todo: here we should call evaluate_var_expression instead
                // to enable further processing of suffix
                // but tell it that it's non-assignable (rvalue)
                return var_expr_eval::make_rvalue(ref.size());
            }
            else if (name == "capacity") return var_expr_eval::make_rvalue(ref.capacity());
            else if (name == "empty") return var_expr_eval::make_rvalue(ref.empty());
            else if (name == "front") return evaluate_var_expression(ref.front(), suffix.substr(b2+1));
            else if (name == "back") return evaluate_var_expression(ref.back(), suffix.substr(b2+1));
            else if (name == "pop_back") {
                ref.pop_back();
                return var_expr_eval::make_void();
            }
            else if (name == "clear") {
                ref.clear();
                return var_expr_eval::make_void();
            }
        }
    }
    if (suffix[0] == '=') {
        return evaluate_var_assignment(ref, suffix.substr(1));
    }
    return var_expr_eval_error::invalid_syntax;
}

template <class T>
std::optional<var_expr_eval_result> evaluate_typed_var_expr(T*& ref, std::string const& suffix) {
    if (suffix[0] == '[') {
        size_t unknown_size = -1;
        return evaluate_subscript(ref, unknown_size, suffix);
    }
    if (suffix.substr(0, 2) == "->") {
        return evaluate_var_member(*ref, suffix.substr(2));
    }
    if (suffix[0] == '=') {
        return evaluate_var_assignment(ref, suffix.substr(1));
    }
    return var_expr_eval_error::invalid_syntax;
}

template <class T>
var_expr_eval_result evaluate_var_expression(T& ref, std::string const& suffix) {
    if (suffix.empty()) {
        // todo: create and return rvalue if ref not assignable
        return var_expr_eval::make_lvalue(ref);
    }
    {
        auto ret = evaluate_typed_var_expr(ref, suffix);
        if (ret) return std::move(*ret);
    }
    if (suffix[0] == '.') {
        return evaluate_var_member(ref, suffix.substr(1));
    }
    if (suffix[0] == '=') {
        return evaluate_var_assignment(ref, suffix.substr(1));
    }
    // todo: more expressions
    return var_expr_eval_error::invalid_syntax;
}

var_expr_eval_result evaluate_expression(std::string const& expr, bool for_assignment) {
    if (expr.empty()) return var_expr_eval_error::invalid_syntax;
    auto ret = process_var_name_prefixes(expr,
        std::function([](std::string const& prefix, std::string const& suffix) -> std::optional<var_expr_eval_result> {
            auto type = tunable_types::find_type_of_var(prefix);
            if (!type) return std::nullopt;
            auto ret = type->evaluate_var_expr(prefix, suffix);
            if (std::holds_alternative<var_expr_eval_ptr>(ret)) return ret;
            return std::nullopt;
        })
    );
    if (ret) return std::move(*ret);

    auto eq = find_not_quoted(expr, '=');
    if (eq != std::string::npos) { // create a new variable
        auto name = expr.substr(0, eq);
        if (name.empty()) return var_expr_eval_error::invalid_syntax;
        if (!is_valid_var_name(name)) return var_expr_eval_error::invalid_var_name;
        auto suffix = expr.substr(eq+1);
        auto eval = evaluate_expression(suffix, false /* primitive types only */);
        if (std::holds_alternative<var_expr_eval_ptr>(eval)) {
            auto& ptr = std::get<var_expr_eval_ptr>(eval);
            if (!ptr) return var_expr_eval_error::void_to_value;
            return ptr->create_var(name);
        }
        return eval;
    }

    // todo: more expressions

    try { // create new rvalue
        if (is_quoted(expr)) return var_expr_eval::make_rvalue(parse_quoted_string(expr));
        else if (is_double(expr)) return var_expr_eval::make_rvalue(std::stod(expr));
        else if (is_integer(expr)) return var_expr_eval::make_rvalue(std::stoll(expr));
        else if (is_bool(expr)) return var_expr_eval::make_rvalue(expr == "true");
        else if (for_assignment) return var_expr_eval::make_rvalue(expr);
    }
    catch (std::exception &e) {
        std::cout << "Exception: parse failed (" << e.what() << ")\n";
        return var_expr_eval_error::invalid_syntax;
    }
    if (is_valid_var_name(expr)) { // todo: also check for things like *(v[i]->p).x
        return var_expr_eval_error::undefined;
    }
    return var_expr_eval_error::invalid_syntax;
}

class interaction_loop
{
public:
    static void run() {
        std::cout << "--- TUNABLE BEGIN ---\n";
        print_help();
        while (true) {
            std::cout << "$ ";
            std::string s;
            std::getline(std::cin, s);
            auto result = handle_command(s);
            if (result == cmd_handling_result::exit) break;
            else if (result == cmd_handling_result::unrecognized)
                std::cout << "unrecognized command\n";
        }
        std::cout << "--- TUNABLE END ---\n";
    }
private:
    enum class cmd_handling_result { unrecognized, empty, processed, error, exit };

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
                auto value = evaluate_expression(name);
                if (std::holds_alternative<var_expr_eval_ptr>(value)) {
                    auto& ptr = std::get<var_expr_eval_ptr>(value);
                    if (ptr) {
                        auto val_str = ptr->to_string();
                        if (val_str) std::cout << name << "=" << *val_str << "\n";
                    }
                }
            }
            return cmd_handling_result::processed;
        }
        return cmd_handling_result::unrecognized;
    }

    static cmd_handling_result handle_expressions(std::string const& s) {
        auto expressions = split_not_quoted(s, ';');
        for (auto &expr : expressions) {
            auto r = handle_expression(expr);
            if (r != cmd_handling_result::processed) return r;
        }
        return cmd_handling_result::processed;
    }

    static cmd_handling_result handle_expression(std::string const& expr) {
        if (expr.empty()) return cmd_handling_result::processed;
        auto eval = evaluate_expression(expr);

        if (std::holds_alternative<var_expr_eval_ptr>(eval)) {
            auto& ptr = std::get<var_expr_eval_ptr>(eval);
            if (ptr) {
                auto s = ptr->to_string();
                if (s) std::cout << *s << "\n";
            }
            return cmd_handling_result::processed;
        }
        else if (std::holds_alternative<var_expr_eval_error>(eval)) {
            auto& error = std::get<var_expr_eval_error>(eval);
            std::cout << to_string(error) << "\n";
        }
        return cmd_handling_result::error;
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

#define _tunable_var_type(x) std::remove_reference<decltype(x)>::type

#define _tunable_member(T,M,x) _tunable_impl::member_var_factory::create<T,M>(#x, [](T& t) -> M& { return t.x; })
// tunable_member(Class,x) - register member variable Class::x as tunable
#define tunable_member(Class,x) _tunable_member(Class, _tunable_var_type(Class::x), x)

#define _tunable_var_(x,n) _tunable_impl::tunable _tunable_for_##n(x, #x)
#define _tunable_var(x,n) _tunable_var_(x, n)
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