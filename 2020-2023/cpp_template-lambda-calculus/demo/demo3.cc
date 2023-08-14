#include "tml.h"
#include <iostream>

declare(x);
declare(y);
declare(z);
declare(fact);

using less = ord<int_tag>::less;
using times = num<int_tag>::times;
using minus = num<int_tag>::minus;
using plus = num<int_tag>::plus;

///! use s expression style to fully apply all parameters
///! e.g apply<func, 1, 2, 3>
///! to paritally apply a function, use the apply member of a lambda.
///! e.g lambda<x, y>::begin<x>::apply<1>::type

///! clashing test
using func =                           //
    let<x, int_<2>,                    //
        in<let<y, int_<3>,             //
               in<lambda<x, y>::begin< //
                   apply<plus, x, y>>::apply<x>::apply<y>>>>>;
static_assert(func::type::value == 5);

///! currying test
using curryed = lambda<x, y>::begin<apply<plus, x, y>>::apply<int_<1>>::type;
using fullyapply = curryed::apply<int_<2>>::type;
static_assert(fullyapply::value == 3);

///! recursive let test

using lettest0 = letrec<x, int_<0>, in<x>>;
static_assert(sizeof(lettest0) != 0);

using lettest1 =
    letrec<fact,
           lambda<fact, lambda<x>::begin< //
                            if_<eql<x, int_<1>>, int_<1>,
                                apply<fact, apply<minus, x, int_<1>>>>>>,
           in<apply<fact, int_<5>>>>;

int main(void) {
    std::cout << "clashing: " << func::type::value << std::endl;
    std::cout << "currying: " << fullyapply::value << std::endl;
    return 0;
}
