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
 * This module defines some universal utilities like quotes and variables.
 * */

#pragma once

/*
 * utils
 * */
template <typename T> struct identity { using type = T; };
template <typename T> struct tag { using type = typename T::tag; };

/*
 * TODO
 * apply is specializef for both top level metafunction and lambda.
 * it supports currying, so for partial applicaition it should return
 * a new partially applied lambda.
 * */
template <typename F, typename... Ts> struct apply {
    using type = typename F::template apply<Ts...>::type;
};

///! box a value not compatible for meta funtion.
template <typename T> struct box { using type = box; };
template <typename T> struct unbox;
template <typename T> struct unbox<box<T>> { using type = T; };

/*
 * lazy eval
 * */
template <typename T> struct already_lazy;
template <typename Expr> struct lazy : Expr {};
template <typename T> struct lazy<already_lazy<T>> { using type = T; };
template <template <typename...> typename F, typename... Ts>
struct lazy<F<Ts...>> : F<typename lazy<Ts>::type...> {};

/*
 * conditional
 * */
template <typename C, typename T, typename F> struct if_ {
  private:
    template <bool C_, typename T_, typename F_> struct if_impl;
    template <typename T_, typename F_> struct if_impl<true, T_, F_> : T {};
    template <typename T_, typename F_> struct if_impl<false, T_, F_> : F {};

  public:
    using type = typename if_impl<C::type::value, T, F>::type;
};
