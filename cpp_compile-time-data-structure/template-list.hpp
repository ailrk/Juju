#include <iostream>

template <typename T> struct Return { using type = T; };

// Type level list.
// params:
//   @...Ts: list of types for constructing the list.
// operations:
//   @type:   List<typename...>,
//     return the list type itself.
//   @size:   size_t
//     return the size of elements in ...Ts.
//   @append: typename... -> List<typename...>
//     append the list with another varadict type list.
//   @apply:  template <typename> typename F -> F<Ts...>
//     apply type operator F on ...Ts
template <typename... Ts> struct List {
    // varadic ...Ts includes on or more, and it's already recursive by itself.
    // there is no need to define a ADT style list.

    using type = List<Ts...>;
    static constexpr size_t size = sizeof...(Ts);

    template <typename... Us> using append = List<Ts..., Us...>;

    // apply F with Ts...
    template <template <typename...> typename F> using apply = F<Ts...>;
};

template <typename... Ts> using List_t = typename List<Ts...>::type;

using Nil = List<>;

template <typename T> using IsNil = typename std::is_same<T, Nil>::type;

// Get the head of a list
// @param:
//   typename:  List<typename...>
// @operations:
//   type: typename,  get the head of the list.
template <typename> struct Head {};
template <> struct Head<List<>> { using type = Nil; };
template <typename T, typename... Ts> struct Head<List<T, Ts...>> {
    using type = T;
};

// Get the tail of a list
// @param:
//   typename:  List<typename...>
// @operations:
//   type: List<typename...>,  get the tail of the list.
template <typename...> struct Tail {};
template <> struct Tail<List<>> { using type = Nil; };
template <typename T, typename... Ts> struct Tail<List<T, Ts...>> {
    using type = List<Ts...>;
};

// Concat two lists.
// @params:
//   List<typename...>: first list
//   List<typename...>: second list
// @operations:
//   type: List<typename...>, the concated list.
template <typename, typename> struct Concat {};
template <typename... Ts, typename... Us>
struct Concat<List<Ts...>, List<Us...>> {
    using type = typename List<Ts...>::template append<Us...>;
};

// Map over a type level list.
// @params:
//  F:  template <typename> typename,  function
//  XS: List<typename...>
// @operations:
//  type: Get the list;
template <template <typename> typename F, typename XS> struct Map {};
template <template <typename> typename F, typename... Ts>
struct Map<F, List<Ts...>> : List<typename F<Ts>::type...> {};

template <template <typename> typename Pred, typename XS,
          typename Base = List<>>
// default type parameter can introduce local binding.
struct Filter : Base {};

template <template <typename> typename Pred, typename Base, typename T,
          typename... Ts>
struct Filter<Pred, List<T, Ts...>, Base> {
    using type = std::conditional_t<
        Pred<T>::value,
        Filter<Pred, typename Base::template concat<T>::type, Ts...>,
        Filter<Pred, Base, Ts...>>;
};

// Left fold on a type level list.
// @params:
//   Op: template <typename, typename> typename,
//     A binary operation, takes acc as the first argument and a element in the
//     list as the second argument. Op :: a -> b -> a
//   Acc: typename
//     Accumulator. It cab be a single value for base case.
//   XS: List<typename...>
//     A type level list.
// @operations;
//   type: Acc::type
template <template <typename, typename> typename Op, typename Acc, typename XS>
struct FoldeL : Return<Acc> {};
template <template <typename, typename> typename Op, typename Acc, typename T,
          typename... Ts>
struct FoldeL<Op, Acc, List<T, Ts...>>
    : FoldeL<Op, typename Op<Acc, T>::type, List<Ts...>> {};

// Check if element is in the type level list.
// @params:
//   E:                     element to test.
//   XS: List<typename...>  list to test.
//
// @operations:
//   value: bool,  the test result.
template <typename...> struct Elem;
template <typename E> struct Elem<E> : std::false_type {};
template <typename E, typename... Ts>
struct Elem<E, List<Ts...>>
    : std::bool_constant<(std::is_same_v<Ts, E> || ...)> {};

// Return a List<typename ...> with no duplicated elements.
// @params:
//    Input: List<typename...>
// @operations:
//    type: List<typename...>
template <typename Input> class Unique {
    // use private type alias for local declaration.
  private:
    template <typename Acc, typename E>
    struct Add : std::conditional_t<Elem<E, Acc>::value, Acc,
                                    typename Acc::template concat<E>> {};

  public:
    using type = typename FoldeL<Add, List<>, Input>::type;
};

#define typeof(TYPE) static_assert(sizeof(TYPE) == -1)

// using t = Tail<List<int>>;
// typeof(t::type);
