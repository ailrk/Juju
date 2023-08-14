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

#pragma once
#include "base.h"

/*
 * values
 * value is defined by it's constructor and a tag to indicate it's type.
 * all values are boxed. Boxed value wrap types that are not prepared for
 * metafunction, which expect a type with ::type.
 * */
///! int type
struct int_tag {}; // this is really the type.
template <int N> struct int_ : identity<int_<N>> {
    using tag = int_tag;
    static const int value = N;
};

///! char type
struct char_tag {}; // this is really the type.
template <char N> struct char_ : identity<char_<N>> {
    using tag = char_tag;
    static const char value = N;
};

///! bool type
struct bool_tag {}; // this is really the type.
template <bool N> struct bool_ : identity<bool_<N>> {
    using tag = bool_tag;
    static const bool value = N;
};

///! maybe type
struct maybe_tag {};
template <typename T> struct just : identity<just<T>> {
    using tag = maybe_tag;
};
struct nothing : identity<nothing> {
    using tag = maybe_tag;
};

///! list type
struct list_tag {};
template <typename H, typename T> struct cons : identity<cons<H, T>> {};
struct nil : identity<nil> {};

///! logic not
template <typename T> struct not_;
template <> struct not_<bool_<true>> : bool_<false> {};
template <> struct not_<bool_<false>> : bool_<true> {};
///! logic and
template <typename... Ts>
struct and_ : bool_<(Ts::type::value && ... && true)> {};
///! logic or
template <typename... Ts>
struct or_ : bool_<(Ts::type::value || ... || false)> {};

///! from from just
template <typename T> struct from_just;
template <typename T> struct from_just<just<T>> : just<T>::type {};

template <typename L> struct head;
template <typename H, typename T> struct head<cons<H, T>> : H {};

template <typename L> struct tail;
template <typename H, typename T> struct tail<cons<H, T>> : T {};

/*
 * type predicates
 * */

///! equal
template <typename A, typename B> struct eql : bool_<false> {};
template <typename A> struct eql<A, A> : bool_<true> {};

///! is integral type
template <typename A>
struct is_integral : or_<eql<A, int_tag>,  //
                         eql<A, bool_tag>, //
                         eql<A, char_tag>> {};

/*
 * typeclasses
 * */

///! class num
template <typename T> struct num;
template <> struct num<int_tag> {
    struct plus : identity<plus> {
        template <typename A, typename B>
        using apply = int_<A::type::value + B::type::value>;
    };
    struct minus : identity<minus> {
        template <typename A, typename B>
        using apply = int_<A::type::value - B::type::value>;
    };
    struct times : identity<times> {
        template <typename A, typename B>
        using apply = int_<A::type::value * B::type::value>;
    };
};

///! class: eq
///!   equal<A, B> -> bool_tag
///!   not_equal<A, B> -> bool_tag
template <typename T> struct eq;
template <typename T> struct eq_defaults {
    struct not_equal : identity<not_equal> {
        template <typename A, typename B>
        struct apply
            : not_<typename ::apply<typename eq<T>::equal, A, B>::type> {};
    };
};

template <> struct eq<int_tag> : eq_defaults<int_tag> {
  private:
    template <typename A, typename B> struct equal_to {
        using type = bool_<A::type::value == B::type::value>;
    };

  public:
    struct equal : identity<equal> {
        template <typename A, typename B> struct apply : equal_to<A, B> {};
    };
};

template <typename A, typename B> struct equal_just_impl {
    using type = bool_<false>;
};
template <> struct equal_just_impl<nothing, nothing> {
    using type = bool_<true>;
};
template <typename A> struct equal_just_impl<just<A>, just<A>> {
    using type = bool_<true>;
};

template <> struct eq<maybe_tag> : eq_defaults<maybe_tag> {
  public:
    struct equal : identity<equal> {
        template <typename A, typename B>
        struct apply : equal_just_impl<A, B>::type {};
    };
};

///! class ord
///!    less<A, B> -> bool_tag
///!    less_equal<A, B> -> bool_tag
///!    greater<A, B> -> bool_tag
///!    greater_equal<A, B> -> bool_tag
template <typename T> struct ord;
template <typename T> struct ord_defaults {
    struct less : identity<less> {
        template <typename A, typename B>
        using apply =
            and_<typename ::apply<typename ord<T>::less_equal, A, B>::type, //
                 typename not_<typename ::apply<typename eq<T>::equal, A,
                                                B>::type>::type //
                 >;
    };

    struct greater : identity<greater> {
        template <typename A, typename B>
        using apply =
            not_<typename ::apply<typename ord<T>::less_equal, A, B>::type>;
    };

    struct greater_equal : identity<greater_equal> {
        template <typename A, typename B>
        using apply = not_<typename ::apply<typename ord<T>::less, A, B>::type>;
    };
};

template <> struct ord<int_tag> : ord_defaults<int_tag> {
    struct less_equal : identity<less_equal> {
        template <typename A, typename B>
        using apply = bool_<(A::type::value <= B::type::value)>;
    };
};

template <> struct ord<bool_tag> : ord_defaults<bool_tag> {
    struct less_equal : identity<less_equal> {
        template <typename A, typename B>
        using apply = bool_<(A::type::value <= B::type::value)>;
    };
};

/*
 * quotes and variable
 * If a metafunction value is quoted, it will never be evaluated by itself.
 * the only way to evaluate it is to unquote it, or pattern match to pull the
 * value out.
 * */
struct quote_tag {};
struct unquote_tag {};
template <typename T> struct quote : identity<quote<T>> {
    using tag = quote_tag;
};
template <typename T> struct unquote;
template <typename T> struct unquote<quote<T>> : T { using tag = unquote_tag; };

struct var_tag {};
template <typename Id> struct var : identity<var<Id>> { using tag = var_tag; };

template <> struct eq<var_tag> : eq_defaults<var_tag> {
    struct equal : identity<equal> {
        template <typename A, typename B>
        using apply = eql<typename A::type, typename B::type>;
    };
};

#define declare(n)                                                             \
    struct n##__tml__internal_defined_var_ {};                                 \
    using n = var<n##__tml__internal_defined_var_>

#define intern(n) declare(n)

// internal use.
intern(tml_v0_);
intern(tml_v1_);
intern(tml_v2_);
intern(tml_v3_);
intern(tml_v4_);
intern(tml_v5_);
intern(tml_v6_);
intern(tml_v7_);
intern(tml_v8_);
intern(tml_v9_);
