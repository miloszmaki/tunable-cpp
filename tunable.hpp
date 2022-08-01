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

namespace _tunable_impl {

// helpers

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

enum class assign_var_result { ok, var_not_found, bad_value };

template<class T>
assign_var_result var_from_string(T& ref, std::string const& var_suffix, std::string const& value);

struct var_to_string_result {
    bool found = false;
    std::optional<std::string> str;
    void const* var_ptr = nullptr;
    std::optional<std::type_index> var_type;
};
template<class T>
var_to_string_result var_to_string(T const& ref, std::string const& var_suffix);

// process prefix parts of name split by .
// todo: support [] -> etc.
template <class T>
std::optional<T> process_var_name_prefixes(std::string const& name, std::function<std::optional<T>(std::string const&,std::string const&)> func) {
    for (size_t i=0; i<name.size(); i++) {
        if (name[i] == '.') {
            auto prefix = name.substr(0, i);
            auto suffix = name.substr(i);
            auto ret = func(prefix, suffix);
            if (ret.has_value()) return ret;
        }
    }
    return func(name, "");
}

template<class T>
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
std::optional<std::string> to_string(T const& ref) {
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
std::optional<std::string> to_string(bool const& ref) {
    return ref ? "true" : "false";
}

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

// interface for variable search and assignment
class tunable_type_base {
public:
    virtual ~tunable_type_base() {}
    virtual var_to_string_result find_var_to_string(std::string const& name, std::string const& suffix) const = 0;
    virtual assign_var_result assign_var(std::string const& name, std::string const& suffix, std::string const& value) = 0;
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
    virtual var_to_string_result find_var_to_string(std::string const& name, std::string const& suffix) const {
        auto &self = get_instance();
        auto var = self.find_var_typed(name);
        if (!var) return {};
        return var_to_string(var->ref, suffix);
    }
    virtual assign_var_result assign_var(std::string const& name, std::string const& suffix, std::string const& value) {
        auto &self = get_instance();
        auto var = self.find_var_typed(name);
        if (!var) return assign_var_result::var_not_found;
        return var_from_string(var->ref, suffix, value);
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
    virtual var_to_string_result to_string(T const& ref, std::string const& var_suffix) const = 0;
    virtual assign_var_result from_string(T& ref, std::string const& var_suffix, std::string const& value) = 0;
};

// holds all tunable members for a given class
template<class T>
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

    template<class U, class M>
    friend class member_var;
};

// represents a tunable class member
template <class T, class M>
class member_var : public member_var_base_typed<T> {
public:
    virtual ~member_var() {
        member_vars<T>::remove(name);
    }
    var_to_string_result to_string(T const& ref, std::string const& var_suffix) const {
        auto& member_ref = get_cref(ref);
        return var_to_string(member_ref, var_suffix);
    }
    virtual assign_var_result from_string(T& ref, std::string const& var_suffix, std::string const& value) {
        auto& member_ref = get_ref(ref);
        return var_from_string(member_ref, var_suffix, value);
    }
private:
    std::string name;
    M& (*get_ref)(T&) = nullptr;
    M const& (*get_cref)(T const&) = nullptr;

    member_var(std::string const& name, M& (*get_ref)(T&), M const& (*get_cref)(T const&))
            : name(name), get_ref(get_ref), get_cref(get_cref) {
        member_vars<T>::add(this, name);
    }

    friend class member_var_factory;
};

// allows to register class member variables as tunables
class member_var_factory {
public:
    template <class T, class M>
    static void create(std::string const& name, M& (*get_ref)(T&), M const& (*get_cref)(T const&)) {
        members.emplace_back(new member_var<T,M>(name, get_ref, get_cref));
    }
private:
    inline static std::vector<member_var_base*> members;
};

var_to_string_result evaluate_expression(std::string const& expr) {
    if (expr.empty()) return {};
    auto ret = process_var_name_prefixes(expr,
        std::function([](std::string const& prefix, std::string const& suffix) -> std::optional<var_to_string_result> {
            auto type = tunable_types::find_type_of_var(prefix);
            if (!type) return {};
            return type->find_var_to_string(prefix, suffix);
        })
    );
    if (ret) return *ret;
    return { .str = expr };
}

template<class T>
assign_var_result var_from_string(T& ref, std::string const& var_suffix, std::string const& value) {
    if (var_suffix.empty()) {
        auto eval = evaluate_expression(value);
        if (eval.var_ptr && *eval.var_type == std::type_index(typeid(ref))) {
            // if types match assign directly
            ref = *(T const*)eval.var_ptr;
            return assign_var_result::ok;
        }
        // deserialize
        if (eval.str && from_string(ref, *eval.str)) return assign_var_result::ok;
        return assign_var_result::bad_value;
    }
    // todo: specialization for [] -> etc.
    if (var_suffix[0] == '.') {
        auto ret = process_var_name_prefixes(var_suffix.substr(1),
            std::function([&](std::string const& prefix, std::string const& suffix) -> std::optional<assign_var_result> {
                auto var = member_vars<T>::find_member(prefix);
                if (!var) return std::nullopt;
                auto ret = var->from_string(ref, suffix, value);
                if (ret != assign_var_result::var_not_found) return ret;
                return std::nullopt;
            })
        );
        if (ret) return *ret;
        return assign_var_result::var_not_found;
    }
    return assign_var_result::var_not_found;
}

template<class T>
var_to_string_result var_to_string(T const& ref, std::string const& var_suffix) {
    if (var_suffix.empty()) {
        return {true, to_string(ref), (void const*)&ref, typeid(ref) };
    }
    // todo: specialization for [] -> etc.
    if (var_suffix[0] == '.') {
        auto ret = process_var_name_prefixes(var_suffix.substr(1),
            std::function([&](std::string const& prefix, std::string const& suffix) -> std::optional<var_to_string_result> {
                auto var = member_vars<T>::find_member(prefix);
                if (!var) return std::nullopt;
                auto ret = var->to_string(ref, suffix);
                if (ret.found) return ret;
                return std::nullopt;
            })
        );
        if (ret) return *ret;
        return {};
    }
    return {};
}

// allows to print, assign and create variables
class tunable_manager {
public:
    static void print_var(std::string const &name) {
        auto eval = evaluate_expression(name);
        if (eval.str) {
            auto s = *eval.str;
            if (eval.found || is_quoted(s) || is_integer(s) || is_double(s) || is_bool(s)) {
                std::cout << s << "\n";
                return;
            }
        }
        if (is_valid_var_name(name)) std::cout << "undefined\n";
        else std::cout << "invalid expression\n";
    }
    static void assign_var(std::string const &name, std::string const &value) {
        auto ret = process_var_name_prefixes(name,
            std::function([&](std::string const& prefix, std::string const& suffix) -> std::optional<assign_var_result> {
                auto type = tunable_types::find_type_of_var(prefix);
                if (!type) return std::nullopt;
                return type->assign_var(prefix, suffix, value);
            })
        );
        if (ret) {
            if (*ret != assign_var_result::ok) std::cout << "failed to assign the variable " << name << "\n";
            if (*ret != assign_var_result::var_not_found) return;
        }
        create_var(name, value);
    }
    static void create_var(std::string const &name, std::string const &value) {
        if (name.empty() || value.empty()) return;
        if (!is_valid_var_name(name)) {
            std::cout << "invalid variable name\n";
            return;
        }
        try {
            if (is_quoted(value)) {
                auto x = new std::string(parse_quoted_string(value));
                create_tunable(*x, name);
            }
            else if (is_double(value)) {
                auto x = new double(std::stod(value));
                create_tunable(*x, name);
            }
            else if (is_integer(value)) {
                auto x = new long long(std::stod(value));
                create_tunable(*x, name);
            }
            else if (is_bool(value)) {
                auto x = new bool(value == "true");
                create_tunable(*x, name);
            }
            else {
                std::cout << "invalid value\n";
            }
        }
        catch (std::exception &e) {
            std::cout << "Exception: parse failed (" << e.what() << ")\n";
        }
    }
private:
    // create a tunable variable of a given type
    template <class T>
    static void create_tunable(T &x, std::string const& s) {
        new tunable<T>(x, s);
    }
};

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
            else if (result == cmd_handling_result::invalid_syntax)
                std::cout << "invalid syntax\n";
        }
        std::cout << "--- TUNABLE END ---\n";
    }
private:
    enum class cmd_handling_result { unrecognized, empty, processed, invalid_syntax, exit };

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
            for (auto &[s,_] : tunable_types::get_var_types()) {
                std::cout << s << "\n";
            }
            return cmd_handling_result::processed;
        }
        if (s == "values") {
            for (auto &[s,_] : tunable_types::get_var_types()) {
                auto value = evaluate_expression(s);
                if (value.str) std::cout << s << "=" << *value.str << "\n";
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

    static cmd_handling_result handle_expression(std::string const& s) {
        if (s.empty()) return cmd_handling_result::processed;
        auto eq = find_not_quoted(s, '=');
        if (eq == std::string::npos) {
            tunable_manager::print_var(s);
            return cmd_handling_result::processed;
        }
        else { // x = v
            auto x = s.substr(0, eq);
            auto v = s.substr(eq+1);
            if (find_not_quoted(v, '=') != std::string::npos) {
                return cmd_handling_result::invalid_syntax;
            }
            if (!x.empty() && !v.empty()) {
                tunable_manager::assign_var(x, v);
                return cmd_handling_result::processed;
            }
            return cmd_handling_result::invalid_syntax;
        }
        // todo: more expressions
        return cmd_handling_result::unrecognized;
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

#define _tunable_member(T,M,x) _tunable_impl::member_var_factory::create<T,M>(#x, \
    [](T& t) -> M& { return t.x; }, [](T const& t) -> M const& { return t.x; })
// tunable_member(Class,x) - register member variable Class::x as tunable
#define tunable_member(Class,x) _tunable_member(Class, _tunable_var_type(Class::x), x)

#define _tunable_var_(T,x,n) _tunable_impl::tunable _tunable_for_##n(x, #x)
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