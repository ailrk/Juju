
// secdXD
// Copyright © 2021 ailrk
//
// Permission is hereby granted, free of charge, to any person obtaining
// a copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
// OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
// IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
// TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
// OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

#pragma once
#include <any>
#include <concepts>
#include <iostream>
#include <llvm/IR/Value.h>
#include <memory>
#include <optional>
#include <span>
#include <sstream>
#include <vector>

namespace secd {

enum SECDExprType { SECDCONST_ID, SECDINST_ID, SECDPROG_ID, SECDUNKNOWN_ID };
enum IntType { Int8, Int32, Int64, UInt8, UInt16, UInt32 };
enum Inst {
    LDV,
    CLOSURE,
    LET,
    ENDLET,
    APPLY,
    RETURN,
    ACCESS,
};

char const *inst_to_string(int t) {
    switch (t) {
    case LDV:
        return "LDV";
    case CLOSURE:
        return "CLOSURE";
    case LET:
        return "LET";
    case ENDLET:
        return "ENDLET";
    case APPLY:
        return "APPLY";
    case RETURN:
        return "RETURN";
    case ACCESS:
        return "ACCESS";
    default:
        return "";
    }
}

// value semantics wrapper for polymorphic pointer.
// Expr is a exitential type for any node type T.

// get_typeid is used to bring the erased type back at runtime.

class Expr {
  public:
    // two layeres of base class. BaseI is for an ectual node type to subtype
    // from. Base is used internally.
    struct BaseI { // base interface
        virtual ~BaseI() = default;
        virtual std::string to_string() const = 0;
        virtual SECDExprType get_typeid() const { return SECDUNKNOWN_ID; }
    };

  private:
    struct Base : BaseI {
        virtual std::unique_ptr<Base> clone() const = 0;
    };

    template <typename T> struct Wrapper final : public Base {
        T obj_;
        Wrapper(T obj)
            : obj_(std::move(obj)) {
            static_assert(std::is_copy_constructible_v<T>,
                          "no copy constructor for SECDExpr");
        }

        // export same methods.
        std::unique_ptr<Base> clone() const override {
            return std::make_unique<Wrapper<T>>(obj_);
        }
        std::string to_string() const override { return obj_.to_string(); }
        SECDExprType get_typeid() const override { return obj_.get_typeid(); }
    };

    std::unique_ptr<Base> ptr_;

  public:
    // constructor a pointer to wrapper, which is subtype of Base.
    template <typename T>
    Expr(T obj)
        : ptr_(std::make_unique<Wrapper<T>>(std::move(obj))) {}
    Expr(const Expr &other)
        : ptr_(other->clone()) {}

    Base *operator->() const { return ptr_.get(); }
    Base &operator*() const { return *ptr_; }

    std::string to_string() const { return ptr_->to_string(); }
    SECDExprType get_typeid() const { return ptr_->get_typeid(); }
    std::unique_ptr<Base> clone() const { return ptr_->clone(); }

    friend bool operator==(Expr const &u, Expr const &v);
    friend bool operator!=(Expr const &u, Expr const &v) { return !(u == v); }
    friend std::ostream &operator<<(std::ostream &os, Expr const &u) {
        os << u.to_string();
        return os;
    }

    Expr &operator=(const Expr &other) {
        ptr_ = other->clone();
        return *this;
    }

    // we want to bring back whatever holds by secdexpr.ptr_.get(). Because we
    // erase all types with a Wrapper, when casting back we need to cast back to
    // the corresponding wrapper type first, then access the obj_ field.

    // #define BASE_TO_BASEI(n) static_cast<SECDExpr::BaseI *>(n.ptr_.get())
    template <typename T> std::remove_cvref_t<T> &get() const {
        auto baseI_p = static_cast<Expr::BaseI *>(ptr_.get());
        using plain_t = std::remove_cvref_t<std::decay_t<T>>;
        using wrapper_ptr_t = Expr::Wrapper<plain_t> *;
        return static_cast<wrapper_ptr_t>(baseI_p)->obj_;
    }
};

// Now we can define new nodes as simple pod that simply implement the
// IBase interface.

struct Const {
    int type;
    int64_t value;
    std::string to_string() const { return std::to_string(value); }
    SECDExprType get_typeid() const { return SECDCONST_ID; }
#ifdef Debug
    std::string debug_info = "no info";
#endif
};

struct Instr {
    int inst;
    std::string to_string() const { return inst_to_string(inst); }
    SECDExprType get_typeid() const { return SECDINST_ID; }
};

// Represent a nested program.
struct Program : public std::vector<Expr> {

    using std::vector<Expr>::vector;
    std::string to_string() const {
        std::stringstream ss;
        ss << '[' << ' ';
        for (auto &n : *this) {
            ss << n.to_string() << ' ';
        }
        ss << ']';
        return ss.str();
    }
    SECDExprType get_typeid() const { return SECDPROG_ID; }
};

} // namespace secd
