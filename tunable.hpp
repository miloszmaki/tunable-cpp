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
#include <map>
#include <iostream>
#include <sstream>
#include <optional>

namespace _tunable_impl {

template <typename T, class = void>
struct is_out_streamable : std::false_type {};
template <typename T>
struct is_out_streamable<T, std::void_t<decltype(std::cout << *(T*)0)>> : std::true_type {};

template <typename T, class = void>
struct is_in_streamable : std::false_type {};
template <typename T>
struct is_in_streamable<T, std::void_t<decltype(std::cin >> *(T*)0)>> : std::true_type {};

// holds a reference to a tunable variable
// allows for conversion to/from string
template <class T>
class tunable {
public:
    T& ref;
    tunable(T &var, std::string const& name) : ref(var), name(name) {
        add_to_type(name);
    }
    virtual ~tunable() {
        remove_from_type(name);
    }
    std::optional<std::string> to_string() const {
        static_assert(is_out_streamable<T>::value, "Please overload the std::ostream << operator for this type");
        std::stringstream ss;
        ss << ref;
        return ss.str();
    }
    void from_string(std::string const& s) {
        static_assert(is_in_streamable<T>::value, "Please overload the std::istream >> operator for this type");
        std::stringstream ss(s);
        ss >> ref;
    }
private:
    std::string name;
    void add_to_type(std::string const& name);
    void remove_from_type(std::string const& name);
};

template <>
void tunable<std::string>::from_string(std::string const& s) {
    if (s.size() >= 2 && s[0]=='"' && s.back() == '"') ref = s.substr(1, s.size()-2);
    else ref = s;
    // todo: parse special chars such as \n
}

template <>
std::optional<std::string> tunable<bool>::to_string() const {
    return ref ? "true" : "false";
}

template <>
void tunable<bool>::from_string(std::string const& s) {
    if (s == "true") ref = true;
    else if (s == "false") ref = false;
    else {
        std::stringstream ss(s);
        ss >> ref;
    }
}

// interface for variable search and assignment
class tunable_type_base {
public:
    virtual ~tunable_type_base() {}
    virtual std::optional<std::string> find_var_to_string(std::string const &name) const = 0;
    virtual bool assign_var(std::string const &name, std::string const &value) = 0;
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
    virtual std::optional<std::string> find_var_to_string(std::string const &name) const {
        auto &self = get_instance();
        auto var = self.find_var(name);
        if (!var) return std::nullopt;
        return var->to_string();
    }
    virtual bool assign_var(std::string const &name, std::string const &value) {
        auto &self = get_instance();
        auto var = self.find_var(name);
        if (!var) return false;
        // search for variable of the same type
        auto var2 = self.find_var(value);
        if (var2) {
            var->ref = var2->ref;
            return true;
        }
        // search for other variables
        auto type2 = tunable_types::find_type_of_var(value);
        if (type2) {
            // serialize
            auto s_var2 = type2->find_var_to_string(value);
            if (s_var2) {
                // deserialize
                var->from_string(*s_var2);
                return true;
            }
        }
        // parse value
        var->from_string(value);
        return true;
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
    tunable<T>* find_var(std::string const& name) {
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

// allows to print, assign and create variables
class tunable_manager {
public:
    static void print_var(std::string const &name) {
        auto type = tunable_types::find_type_of_var(name);
        if (type) {
            auto s = type->find_var_to_string(name);
            if (s) {
                std::cout << *s << "\n";
                return;
            }
        }
        std::cout << "undefined\n";
    }
    static void assign_var(std::string const &name, std::string const &value) {
        auto type = tunable_types::find_type_of_var(name);
        if (type) {
            if (type->assign_var(name, value)) return;
        }
        create_var(name, value);
    }
    static void create_var(std::string const &name, std::string const &value) {
        if (value.empty()) return;
        // todo: regex
        if (value[0] == '"') {
            auto x = new std::string(value.substr(1, value.size()-2));
            create_tunable(*x, name);
        }
        else try {
            if (value.find('.') != std::string::npos) {
                auto x = new double(std::stod(value));
                create_tunable(*x, name);
            }
            else {
                auto x = new long long(std::stod(value));
                create_tunable(*x, name);
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

// interaction loop
void interact() {
    std::cout << "--- TUNABLE BEGIN ---    (CTRL+D to quit)\n";
    while (true) {
        std::cout << "$ ";
        std::string s;
        std::getline(std::cin, s);
        if (s == "") break;
        auto eq = s.find("=");
        if (eq == std::string::npos) {
            tunable_manager::print_var(s);
        }
        else { // x = v
            auto x = s.substr(0, eq);
            auto v = s.substr(eq+1);
            if (!v.empty()) tunable_manager::assign_var(x, v);
        }
        // todo: more expressions
    }
    std::cout << "\n--- TUNABLE END ---\n";
}

} // namespace _tunable_impl

// #define tunable(x) _tunable_impl::tunable _tunable_for_##x(x, #x)
// using __COUNTER__ or __LINE__ allows for binding names with special characters (e.g. *v[0]->ptr)
#define _tunable_(x, n) _tunable_impl::tunable _tunable_for_##n(x, #x)
#define _tunable(x, n) _tunable_(x, n)
#ifdef __COUNTER__
#define tunable(x) _tunable(x, __COUNTER__)
#else
#define tunable(x) _tunable(x, __LINE__)
#endif

#define tunablecmd() _tunable_impl::interact()

#endif