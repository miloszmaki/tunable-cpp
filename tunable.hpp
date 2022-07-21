#ifndef tunable

#include <string>
#include <vector>
#include <map>
#include <iostream>
#include <sstream>
#include <optional>

namespace _tunable_impl {

template <typename T, class = void>
struct is_out_streamable : std::false_type { };

template <typename T>
struct is_out_streamable<T, std::void_t<decltype(std::cout << *(T*)0)>>
  : std::true_type { };

template <typename T, class = void>
struct is_in_streamable : std::false_type { };

template <typename T>
struct is_in_streamable<T, std::void_t<decltype(std::cin >> *(T*)0)>>
  : std::true_type { };

template <class T>
class tunable_ref {
public:
    T* x = nullptr;
    tunable_ref() {}
    tunable_ref(T &x) : x(&x) {}

    std::optional<std::string> to_string() const {
        if (!x) return std::nullopt;
        static_assert(is_out_streamable<T>::value, "Please overload the std::ostream << operator for this type");
        std::stringstream ss;
        ss << *x;
        return ss.str();
    }

    void from_string(std::string const& s) {
        if (!x) return;
        static_assert(is_in_streamable<T>::value, "Please overload the std::istream >> operator for this type");
        std::stringstream ss(s);
        ss >> *x;
    }

    operator bool() const { return x != nullptr; }
};

template <>
void tunable_ref<std::string>::from_string(std::string const& s) {
    if (s.size() >= 2 && s[0]=='"' && s.back() == '"') *x = s.substr(1, s.size()-2);
    else *x = s;
    // todo: parse special chars such as \n
}

template <>
std::optional<std::string> tunable_ref<bool>::to_string() const {
    if (!x) return std::nullopt;
    return *x ? "true" : "false";
}

template <>
void tunable_ref<bool>::from_string(std::string const& s) {
    if (s == "true") *x = true;
    else if (s == "false") *x = false;
    else {
        std::stringstream ss(s);
        ss >> *x;
    }
}

class tunable_type_base {
public:
    static void interact() {
        std::cout << "--- TUNABLE BEGIN ---    (CTRL+D to quit)\n";
        while (true) {
            std::cout << "$ ";
            std::string s;
            std::getline(std::cin, s);
            if (s == "") break;
            auto eq = s.find("=");
            if (eq == std::string::npos) {
                print_var(s);
            }
            else { // x = v
                auto x = s.substr(0, eq);
                auto v = s.substr(eq+1);
                if (!v.empty()) assign_var(x, v);
            }
            // todo: more expressions
        }
        std::cout << "\n--- TUNABLE END ---\n";
    }
    virtual std::optional<std::string> find_to_string(std::string const &x) const = 0;
private:
    static void print_var(std::string const &x) {
        auto it = var_types.find(x);
        if (it != var_types.end()) {
            auto s = it->second->find_to_string(x);
            if (s) {
                std::cout << *s << "\n";
                return;
            }
        }
        std::cout << "undefined\n";
    }
    static void assign_var(std::string const &x, std::string const &v) {
        auto it = var_types.find(x);
        if (it != var_types.end()) {
            if (it->second->_assign_var(x, v)) return;
        }
        create_var(x, v);
    }
    static void create_var(std::string const &name, std::string const &val);
protected:
    static inline std::vector<tunable_type_base*> all;
    static inline std::map<std::string, tunable_type_base*> var_types;
    virtual bool _assign_var(std::string const &x, std::string const &v) = 0;
};

template <class T>
class tunable_type : public tunable_type_base
{
public:
    static void add(T &x, std::string const& s) {
        create();
        m[s] = x;
        var_types[s] = instance;
    }
    static void remove(std::string const& s) {
        m.erase(s);
        var_types.erase(s);
    }
protected:
    virtual std::optional<std::string> find_to_string(std::string const &x) const {
        auto v = search(x);
        if (!v) return std::nullopt;
        return v.to_string();
    }
    virtual bool _assign_var(std::string const &x, std::string const &s) {
        auto v = search(x);
        if (!v) return false;
        // search for variable of the same type
        auto v2 = search(s);
        if (v2) {
            *v.x = *v2.x;
            return true;
        }
        // search for other variables
        auto it = var_types.find(s);
        if (it != var_types.end()) {
            // serialize
            auto s2 = it->second->find_to_string(s);
            if (s2) {
                // deserialize
                v.from_string(*s2);
                return true;
            }
        }
        // parse
        v.from_string(s);
        return true;
    }
private:
    static inline std::map<std::string, tunable_ref<T>> m;

    tunable_type() {}
    static inline tunable_type<T>* instance = nullptr;

    static void create() {
        if (instance) return;
        instance = new tunable_type();
        all.emplace_back(instance);
    }

    static tunable_ref<T> search(std::string const& x) {
        auto it = m.find(x);
        if (it != m.end()) return it->second;
        return {};
    }
};

template <class T>
class tunable
{
public:
    tunable (T &x, std::string const& s) : s(s) {
        tunable_type<T>::add(x, s);
    }
    virtual ~tunable() {
        tunable_type<T>::remove(s);
    }
    static void create(T &x, std::string const& s) {
        all.emplace_back(new tunable<T>(x, s));
    }
private:
    std::string s;
    static inline std::vector<tunable<T>*> all;
};

template <class T>
void create_tunable(T &x, std::string const& s) {
    tunable<T>::create(x, s);
}

void tunable_type_base::create_var(std::string const &name, std::string const &val) {
    if (val.empty()) return;
    // todo: regex
    if (val[0] == '"') {
        auto x = new std::string(val.substr(1, val.size()-2));
        create_tunable(*x, name);
    }
    else try {
        if (val.find('.') != std::string::npos) {
            auto x = new double(std::stod(val));
            create_tunable(*x, name);
        }
        else {
            auto x = new long long(std::stod(val));
            create_tunable(*x, name);
        }
    }
    catch (std::exception &e) {
        std::cout << "Exception: parse failed (" << e.what() << ")\n";
    }
}

} // namespace _tunable_impl

// #define tunable(x) _tunable_impl::tunable _tunable_for_##x(x, #x)
// using __COUNTER__ or __LINE__ allows for binding names with `.`
#define _tunable_(x, n) _tunable_impl::tunable _tunable_for_##n(x, #x)
#define _tunable(x, n) _tunable_(x, n)
#ifdef __COUNTER__
#define tunable(x) _tunable(x, __COUNTER__)
#else
#define tunable(x) _tunable(x, __LINE__)
#endif

#define tunablecmd() _tunable_impl::tunable_type_base::interact()

#endif