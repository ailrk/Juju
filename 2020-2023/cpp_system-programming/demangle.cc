#include <cstdint>
#include <cstdlib>
#include <cxxabi.h>
#include <iostream>
#include <string>
#include <typeinfo>

// we can use cxxabi.h's __cxa_demangle to demangle a mangled type name.

#define RED "\033[1;31m";
#define RESET "\033[0m"

template <typename T> std::string type_name() {
    int status;
    std::string name = typeid(T).name();
    auto demangled_name =
        abi::__cxa_demangle(name.c_str(), nullptr, nullptr, &status);
    if (status == 0) {
        name = demangled_name;
        std::free(demangled_name);
    }
    return name;
}

template <typename T1, typename T2> void are_equal() {
    std::cout << type_name<T1>() << std::endl;
    std::cout << type_name<T2>() << std::endl;

    if (sizeof(T1) == sizeof(T2)) {
        std::cout << " - size are both " << sizeof(T1) << std::endl;
    } else {
        std::cout << " - size: ";
        std::cout << sizeof(T1) << " != " << sizeof(T2) << RESET "\n";
    }

    if (type_name<T1>() == type_name<T2>()) {
        std::cout << "same type name" << std::endl;
    }
}

int main(void) {

    std::cout << type_name<int>() << std::endl;
    std::cout << typeid(int).name() << std::endl;

    std::cout << type_name<std::string>() << std::endl;
    std::cout << typeid(std::string).name() << std::endl;

    are_equal<int, int>();
    are_equal<double, long>();
    are_equal<double, int>();

    return 0;
}
