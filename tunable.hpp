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

std::string parse_quoted_string(std::string s) {
    s = s.substr(1, s.size()-2);
    static const std::vector<std::pair<std::string,std::string>> escapes{
        {"\\\\\\\\", "\\"}, {"\\\\n", "\n"}, {"\\\\t", "\t"}, {"\\\\\"", "\""}, {"\\\\r", "\r"}};
    for (auto &[c,r] : escapes)
        s = std::regex_replace(s, std::regex(c), r);
    return s;
}

// interface for tunable
struct tunable_base {
    virtual ~tunable_base() {}
    virtual std::optional<std::string> to_string() const = 0;
    virtual bool from_string(std::string const& s) = 0;
    virtual bool is_class_member() const = 0;
};

// holds a reference to a tunable variable
// allows for conversion to/from string
template <class T>
class tunable : public tunable_base {
public:
    T& ref;
    tunable(T &var, std::string const& name, bool is_member = false) : ref(var), name(name), is_member(is_member) {
        add_to_type(name);
    }
    virtual ~tunable() {
        remove_from_type(name);
    }
    virtual std::optional<std::string> to_string() const {
        if constexpr (is_out_streamable<T>::value) {
            std::stringstream ss;
            ss << ref;
            return ss.str();
        }
        else {
            std::cout << "missing << overload for " << name << " (type=" << typeid(T).name() << ")\n";
            return std::nullopt;
        }
    }
    virtual bool from_string(std::string const& s) {
        if constexpr(is_in_streamable<T>::value) {
            std::stringstream ss(s);
            ss >> ref;
            return true;
        }
        else {
            std::cout << "missing >> overload for " << name << " (type=" << typeid(T).name() << ")\n";
            return false;
        }
    }
    virtual bool is_class_member() const { return is_member; }
private:
    std::string name;
    bool is_member;
    void add_to_type(std::string const& name);
    void remove_from_type(std::string const& name);
};

template <>
bool tunable<std::string>::from_string(std::string const& s) {
    if (is_quoted(s)) ref = parse_quoted_string(s);
    else ref = s;
    return true;
}

template <>
std::optional<std::string> tunable<bool>::to_string() const {
    return ref ? "true" : "false";
}

template <>
bool tunable<bool>::from_string(std::string const& s) {
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

enum class assign_var_result { ok, not_found, bad_value };

// interface for variable search and assignment
class tunable_type_base {
public:
    virtual ~tunable_type_base() {}
    virtual tunable_base* find_var(std::string const &name) const = 0;
    virtual assign_var_result assign_var(std::string const &name, std::string const &value) = 0;
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
    virtual tunable_base* find_var(std::string const &name) const {
        auto &self = get_instance();
        return self.find_var_typed(name);
    }
    virtual assign_var_result assign_var(std::string const &name, std::string const &value) {
        auto &self = get_instance();
        auto var = self.find_var_typed(name);
        if (!var) return assign_var_result::not_found;
        // search for variable of the same type
        { auto var2 = self.find_var_typed(value);
        if (var2) {
            var->ref = var2->ref;
            return assign_var_result::ok;
        } }
        // search for other variables
        auto type2 = tunable_types::find_type_of_var(value);
        if (type2) {
            auto var2 = type2->find_var(value);
            if (var2) {
                // convert
                auto var2_s = var2->to_string();
                if (!var2_s) return assign_var_result::bad_value;
                return var->from_string(*var2_s) ? assign_var_result::ok : assign_var_result::bad_value;
            }
        }
        // parse value
        return var->from_string(value) ? assign_var_result::ok : assign_var_result::bad_value;
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
private:
    virtual void register_tunables(std::vector<std::unique_ptr<tunable_base>>& registered,
                                   T& obj, std::string const& obj_name) = 0;

    template<class U>
    friend class member_vars;
};

// allows to register members of an object as tunables
// holds all tunable members for a given class
template<class T>
class member_vars {
public:
    static void register_tunables(std::vector<std::unique_ptr<tunable_base>>& registered,
                                  T& obj, std::string const& name) {
        auto &self = get_instance();
        for (auto &m : self.members) {
            m->register_tunables(registered, obj, name);
        }
    }
    static std::vector<std::unique_ptr<tunable_base>> register_tunables(T& obj, std::string const& name) {
        std::vector<std::unique_ptr<tunable_base>> registered;
        register_tunables(registered, obj, name);
        return registered;
    }
private:
    std::set<member_var_base_typed<T>*> members;

    member_vars() {}

    static member_vars<T>& get_instance() {
        static member_vars<T> instance;
        return instance;
    }

    static void add(member_var_base_typed<T> *var) {
        auto &self = get_instance();
        self.members.insert(var);
    }
    static void remove(member_var_base_typed<T> *var) {
        auto &self = get_instance();
        self.members.erase(var);
    }

    template<class U, class M>
    friend class member_var;
};

// represents a tunable class member
// allows to register the member of an object as tunable (and its own members recursively)
template <class T, class M>
class member_var : public member_var_base_typed<T> {
public:
    virtual ~member_var() {
        member_vars<T>::remove(this);
    }
private:
    std::string name;
    M& (*get_ref)(T&) = nullptr;

    virtual void register_tunables(std::vector<std::unique_ptr<tunable_base>>& registered,
                                   T& obj, std::string const& obj_name) {
        auto full_name = obj_name + "." + name;
        auto &this_member = get_ref(obj);
        // register this member as a tunable
        registered.emplace_back(new tunable<M>(this_member, full_name, true));
        // register members recursively
        member_vars<M>::register_tunables(registered, this_member, full_name);
    }

    member_var(std::string const& name, M& (*get_ref)(T&)) : name(name), get_ref(get_ref) {
        member_vars<T>::add(this);
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

// allows to print, assign and create variables
class tunable_manager {
public:
    static void print_var(std::string const &name) {
        if (name.empty()) return;
        auto type = tunable_types::find_type_of_var(name);
        if (type) {
            auto var = type->find_var(name);
            if (var) {
                auto s = var->to_string();
                if (s) std::cout << *s << "\n";
                return;
            }
        }
        if (isdigit(name[0])) std::cout << name << "\n";
        else if (is_quoted(name)) std::cout << name.substr(1, name.size()-2) << "\n";
        else std::cout << "undefined\n";
    }
    static void assign_var(std::string const &name, std::string const &value) {
        auto type = tunable_types::find_type_of_var(name);
        if (type) {
            auto r = type->assign_var(name, value);
            if (r != assign_var_result::ok) std::cout << "failed to assign the variable " << name << "\n";
            if (r != assign_var_result::not_found) return;
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
        return handle_expression(s);
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
            for (auto &[s,v] : tunable_types::get_var_types()) {
                auto var = v->find_var(s);
                if (var && !var->is_class_member()) {
                    auto value = var->to_string();
                    if (value) std::cout << s << "=" << *value << "\n";
                }
            }
            return cmd_handling_result::processed;
        }
        return cmd_handling_result::unrecognized;
    }

    static cmd_handling_result handle_expression(std::string const& s) {
        auto eq = s.find("=");
        if (eq == std::string::npos) {
            tunable_manager::print_var(s);
            return cmd_handling_result::processed;
        }
        else { // x = v
            auto x = s.substr(0, eq);
            auto v = s.substr(eq+1);
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

#define _tunable_member(T,M,x) _tunable_impl::member_var_factory::create<T,M>(#x, [](T& t) -> M& { return t.x; })
// tunable_member(Class,x) - register member variable Class::x as tunable
#define tunable_member(Class,x) _tunable_member(Class, _tunable_var_type(Class::x), x)

#define _tunable_var_(T,x,n) _tunable_impl::tunable _tunable_for_##n(x, #x); \
    auto _tunable_members_of_##n = _tunable_impl::member_vars<T>::register_tunables(x, #x)
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