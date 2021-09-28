#include "tml.h"
#include <iostream>

declare(x);
declare(y);
declare(z);

using less = ord<int_tag>::less;
using times = num<int_tag>::times;
using minus = num<int_tag>::minus;
using plus = num<int_tag>::plus;

template <typename N>
struct fact                          //
    : if_<apply<less, N, int_<1>>,   //
          int_<1>,                   //
          apply<times,               //
                fact<                //
                    apply<minus,     //
                          N,         //
                          int_<1>>>, //
                N>                   //
          > {};

using func =             //
    lambda<x, y>::begin< //
        let<z, int_<2>,
            in<fact<                     //
                apply<plus,              //
                      apply<plus, x, y>, //
                      apply<times, z, z> //
                      >>>>>;

int main(void) {
    std::cout << "peepee poopoo: "
              << func::type::apply<int_<3>>::apply<int_<3>>::type::value
              << std::endl;
    return 0;
}
