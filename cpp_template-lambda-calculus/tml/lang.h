// Tempalte meta langauge
// Copyright © 2021 ailrk

// Permission is hereby granted, free of charge, to any person obtaining
// a copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
// OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
// IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
// TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
// OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

/*
 * This file implements basic language facilities like let binding and
 * functions.
 * */

#pragma once
#include "base.h"
#include "types.h"

/*
 * let binding
 * let is implemented by substituting var<T> with it's corresponding
 * quote. On evaluation we evaluate the quote.
 * */

template <typename A, typename E, typename In> struct let_impl_1;

///! let_impl recursively evaluate to the simplest form.
template <typename A, typename E, typename In> struct let_impl;

///! boxed values should remain unchanged.
template <typename A, typename E, typename V>
struct let_impl<A, E, box<V>> : box<V> {};

template <typename A, typename E, typename V>
struct let_impl<A, E, quote<V>> : quote<V> {};

template <typename A, typename E, typename In> struct let_impl {
    using type = In;
};

template <typename A, typename E, template <typename...> typename F,
          typename... Ts>
struct let_impl<A, E, F<Ts...>> {
    using type = F<typename let_impl_1<A, E, Ts>::type...>;
};

///! this case should be specialized before F<Ts...>, otherwise it's
///! ambigous.
template <typename A, typename E, typename In>
struct let_impl_1 : let_impl<A, E, In> {};

///! should be specialized before quote<V>
template <typename A, typename E, typename V>
struct let_impl_1<A, E, unquote<quote<V>>> : let_impl_1<A, E, V> {};

template <typename A, typename E> struct let_impl_1<A, E, A> {
    using type = E;
};

///! unrap E and In.
template <typename A, typename E, typename In> struct strict_let;

///! quotes will evalute to themselves, so only quotes can be instantiated.
template <typename A, typename E, typename In>
struct strict_let<A, quote<E>, quote<In>> //
    : quote<typename let_impl_1<A, E, In>::type> {};

///! force evaluate quotes.
template <typename A, typename E, typename In>
struct let_quoted : strict_let<           //
                        typename A::type, //
                        typename E::type, //
                        typename In::type> {};

template <typename In> struct in : In {};

///! quote expressions to avoid accidental evaluation.
template <typename A, typename E, typename In> struct let;

template <typename A, typename E, typename In>
struct let<A, E, in<In>>
    : unquote<typename let_quoted<A, quote<E>, quote<In>>::type> {};

/*
 * lambda is represented as a pair of parameter list and quoted body expression.
 * */

// TODO
template <typename Body, typename... Ts> struct lambda_impl_1;
template <typename Body, typename...> struct lambda_impl;

///! same as let, forward boxed values.
///! note because lambda is a value, we need to make sure it's a metafunction
///! value. So wrap it inside indentity.
template <typename Body, typename... Ts>
struct lambda_impl<quote<Body>, Ts...>
    : identity<lambda_impl<quote<Body>, Ts...>> {
    using apply = quote<Body>;
};

template <typename Body, typename... Ts>
struct lambda_impl<box<Body>, Ts...>
    : identity<lambda_impl<quote<Body>, Ts...>> {
    template <typename U> using apply = box<Body>;
};

///! base case
template <typename Body> struct lambda_impl<Body> : Body {};

///! recursively substitute paramter.
template <typename Body, typename T, typename... Ts>
struct lambda_impl<Body, T, Ts...> : identity<lambda_impl<Body, T, Ts...>> {
    template <typename U>
    using apply = lambda_impl_1<let<T, U, in<Body>>, Ts...>;
};

template <typename Body, typename... Ts>
struct lambda_impl_1 : lambda_impl<Body, Ts...> {};

///! forward unquote
template <typename Body, typename... Ts>
struct lambda_impl_1<unquote<quote<Body>>, Ts...> : lambda_impl_1<Body, Ts...> {
};

template <typename Body, typename... Ts> struct strict_lambda;

template <typename Body, typename... Ts>
struct strict_lambda<quote<Body>, Ts...>
    : quote<typename lambda_impl_1<Body, Ts...>::type> {};

///! body of the lambda should be quoted.
template <typename Body, typename... Ts>
struct lambda_quoted : strict_lambda<           //
                           typename Body::type, //
                           typename Ts::type...> {};

template <typename... Ts> struct lambda {
    template <typename Body>
    using begin = unquote<typename lambda_quoted<quote<Body>, Ts...>::type>;
};

/*
 * recursive let
 * */

///! we want to bound A in the expression E itself.
template <typename A, typename E, typename In> struct letrec;

// NOTE: let is not defined for letrec for some reasons.
template <typename A, typename E, typename In>
struct letrec : let<A, letrec<A, E, E>, In> {};

/*
 * pattern matching
 */

/*
 * case
 */

/*
 * helper
 * */
// template <typename T>
// struct reflexive : ::apply<typename eq<typename tag<T>::type>::equal, T, T>
// {};

template <typename T>
struct reflexive : let<tml_v0_, typename eq<typename tag<T>::type>::equal, //
                       in<apply<tml_v0_, T, T>>>                           //
{};

/*
 * test
 * */

namespace {
static_assert(reflexive<int_<10>>::type::value);
} // namespace

// first class angle bracket expression
