#include "tml.h"
#include <iostream>

declare(x);
declare(y);
declare(z);

using v = let<x, int_<10>, in<x>>;

using v1 =            //
 let<x, int_<10>,     //
  in<let<y, int_<20>, //
   in<y>>>>;

using v2 = //
 let<x, int_<10>,
  in<let<y, int_<30>,                   //
   in<let<z, int_<20>,                  //
    in<apply<num<int_tag>::plus, x, y>> //
    >>>>>;

template <typename N>
struct fact                                      //
    : if_<apply<ord<int_tag>::less, N, int_<1>>, //
       int_<1>,                                  //
       apply<num<int_tag>::times,                //
        fact<                                    //
         apply<num<int_tag>::minus,              //
          N,                                     //
          int_<1>>>,                             //
        N>                                       //
       > {};

#ifdef Debug
static_assert(and_<bool_<true>, bool_<true>>::type::value);
static_assert(or_<bool_<false>, bool_<true>>::type::value);
#endif

#ifdef Debug
static_assert(eql<int_<10>, int_<10>>::type::value);
static_assert(eql<int_tag, int_tag>::type::value);
static_assert(is_integral<int_tag>::value);
#endif

#ifdef Debug
static_assert(eq<int_tag>::not_equal::apply<int_<20>, int_<10>>::type::value);
static_assert(eq<int_tag>::equal::apply<bool_<true>, bool_<true>>::type::value);
static_assert(eq<maybe_tag>::equal::apply<nothing, nothing>::type::value);
static_assert(
 !eq<maybe_tag>::equal::apply<just<int_<1>>, just<int_<2>>>::type::value);
#endif

#ifdef Debug
static_assert(
 ord<int_tag>::greater_equal::apply<int_<10>, int_<10>>::type::value);
static_assert(
 ord<int_tag>::greater_equal::apply<int_<10>, int_<5>>::type::value);
static_assert(ord<int_tag>::greater::apply<int_<10>, int_<5>>::type::value);
#endif

#ifdef Debug
static_assert(eql<int_<30>,                                          //
 unquote<quote<num<int_tag>::plus::apply<int_<10>, int_<20>>>>::type //
 >::type::value);

static_assert(std::is_same_v<unquote<quote<int_<10>>>::type, int_<10>>);
#endif

int main(void) {
    std::cout << "v: " << v::type::value << std::endl;
    std::cout << "v1: " << v1::type::value << std::endl;
    std::cout << "v2: " << v2::type::value << std::endl;
    std::cout << "fact " << fact<int_<1>>::type::value << std::endl;
    std::cout << "fact " << fact<int_<2>>::type::value << std::endl;
    std::cout << "fact " << fact<int_<3>>::type::value << std::endl;
    std::cout << "fact " << fact<int_<4>>::type::value << std::endl;
    std::cout << "fact " << fact<int_<5>>::type::value << std::endl;
    std::cout << "fact " << fact<int_<6>>::type::value << std::endl;
    std::cout << "fact " << fact<int_<7>>::type::value << std::endl;
    std::cout << "fact " << fact<int_<8>>::type::value << std::endl;
    std::cout << "fact " << fact<int_<9>>::type::value << std::endl;
    std::cout << "fact " << fact<int_<10>>::type::value << std::endl;

    return 0;
}
