#include <algorithm>
#include <array>
#include <assert.h>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <memory>
#include <optional>
#include <type_traits>

//////////////////////////////////////////////////////////////////////////////
// Non destructive Move (C++):
//    Move from object still usable, just in a invalid state.
// Destructive Move (Rust):
//    1. We simply do a member wise copy
//    2. Move from object just gone.
//

// For example, In c++ lots of moves are implemneted with swap, simply swap
// the element between two objects. Why do you want to swap? Isn't it
// expensive to create a new temporary?
// Because c++ has exception and you have to deal with throwable Constructors
// and destructors.

// One reason for having non destructive move is that C++ can have self
// referential structs that refers to it's own field. In this case, once
// we move, we need to ajust the pointer to make things work.
// If we just destory the old object then we no longer have the access to
// original fields.
//
// e.g to move a linked list.

struct SelfReferential {
    std::array<char, 1024> data;
    char *cursor = nullptr;
    SelfReferential()
        : data{}
        , cursor{ &data[0] } {}

    // Note here cursor points to it's own field.
    // If we just do a member wise copy, as a true destructive move, we can't
    // really access other's member.
    SelfReferential(SelfReferential &&other)
        : data{ other.data }
        , cursor{ &data[0] + (other.cursor - &(other.data[0])) } {}

    // ...

    // Imagine this is a destructive move with bitwise copy.
    // after move we have data == other.data, cursor == other.cursor.
    // but cursor depends on the position of data.
    // case 1: if data is a ptr, it's ok
    // case 2: data is relocated, then the old cursor will be nolonger valid.
    //         in this case we need to adjust the pointer.
};

// Another example of self referential
struct Parent {
    int count;
};
struct Child {
    Parent &parent;
};
struct Combined {
    Parent parent;
    Child chld;
};

// force it t allocate in the stack, and place the move from mmebers into it
// it 's not quite right because we still have move from object available
// if t is passed by lvalue reference.
template <typename T> std::remove_cvref_t<T> memberwise_move(T &&t) noexcept {
    using T_ = std::remove_cvref_t<T>;
    T_ *ptr = (T_ *)alloca(sizeof(T_));
    std::memcpy(ptr, &t, sizeof(T_));
    return *ptr;
}

// what happend here?
// - initially parent and child have there own address, child referer to
//   parent's address
//
// - combined has a new location that might be far away from both child
//   and parent
// - parent is moved to a offset start from combined,
// - child also moved to a offset start from combined.
// - child still holds a reference to the old parent, which now is gone.
// => we get a dangling reference.
//
// To fix this, we need to adjust the reference in child. But to do that
// the move is no longer trivial, we need to have access to the old parent
// reference in child.
void lifetime_and_self_reference() {

    // lifetime
    {                          // 0
        Parent parent{ 24 };   // 1
        Child child{ parent }; // 2
        Combined combined{ memberwise_move(parent),
                           memberwise_move(child) }; // 3
    }                                                // 4

    // parent has lifetime [1-3)
    // child has lifetime [2-3)
    // combined has lifetime [3-4)
    // meaning:
    //  at [3], child is invalid, and parent is also invalid.
    //  but child still holds a reference to the invalid parent, bad.
    //
    // we need to either adjust pointer or forbid this case.

    // or maybe
    {
        Combined combined{ Parent{ 12 }, Child{ combined.parent } };

        Combined combinded1 = memberwise_move(combined); // NO!
    }
}

////////////////////
// PS: After move the move from object will still be a valid state, and when
// it's lifetime ends the destructor wil be called.
// If we swap content, then swapped content will be destroyed.
// If we leave bunch of dangling ptr in the move from object, there might be
// double free.
// So if we don't want to swap, we need to handle double free cases manually.

// Also in case of std::move(foo), destructor of foo is not called because we
// just cast foo into a rvalue reference and work with it. Rvaleu reference
// allow us to modify the state, and make no assumption about the state
// after. But the lifetime of foo still depends on where it's introduced, so
// no destructor will be called.

// Better to think about copy/move as pattern matching on references.
// if it's a int&&, the move construct will be called. What's the behavior
// of move entirely depends on how the move constructor is defined.

//////////////////////////////////////////////////////////////////////////////
// problem: Throwable move operations

template <typename T> struct Vec {
  private:
    T *data_;
    size_t size_;
    size_t capacity_;

  public:
    ~Vec() {
        for (int i = 0; i < size_; ++i) {
            delete data_[i];
        }
    }

    Vec()
        : data_(new T[0])
        , size_(0)
        , capacity_(64) {}

    // a simple example of how to handle types with different semantics.
    // given a type, we need to think about:
    // 1. is the type trivially copyable?
    //    - if is, we probably can go ahead with memcpy
    // 2. is it movable?
    //    - if is, we probably can go ahead with memcpy.
    // 3. does it throw?
    //    - if is, we need to deal with the throwing case.
    Vec(Vec const &other)
        : size_(other.size_)
        , capacity_(other.capacity_)
        , data_(new T[capacity_]) {
        if constexpr (std::is_trivially_copy_constructible_v<T>) {
            std::uninitialized_copy(std::begin(other.data_),
                                    std::end(other.data_), std::begin(data_));
        } else if constexpr (std::is_nothrow_copy_assignable_v<T> &&
                             std::is_nothrow_copy_assignable_v<T>) {
            for (int i = 0; i < size_; ++i) {
                *data_[i] = *other.data_[i];
            }
        } else if constexpr (std::is_copy_constructible_v<T> &&
                             std::is_copy_assignable_v<T>) {
            int i = 0;
            try {
                for (; i < size_; ++i) {
                    *data_[i] = *other.data_[i];
                }
            } catch (...) {
                for (int j = 0; j < i; ++j) {
                    delete data_[j];
                }
            }
        }
    }

    Vec &operator=(Vec const &other) {
        auto tmp = Vec{ other };
        std::swap(*this, tmp);
    }

    // Simulate destructive move when you only have non destructive move.
    // What should ptr be after moved? Nothing but nullptr otherwise it will
    // be very weired.
    //
    // move simply takes the pointer of data from other, and invalidate the old
    // ptr. Nothing really get moved, and it's a farily trivial operation.
    // this move is exception safe, we guarantee no throw.
    //
    // It's really nice to mark it as noexcept because now this Vec type can
    //  itself be deteceted and handle differently.
    Vec(Vec &&other) noexcept
        : size_(other.size_)
        , capacity_(other.capacity_) {
        data_ = other.data_;
        other.data_ = nullptr;
    }

    Vec &operator=(Vec &&other) noexcept {
        auto tmp = std::move_if_noexcept(other);
        std::swap(this, tmp);
    }
};

//////////////////////////////////////////////////////////////////////////////
// be careful with pointer semantics
// unique ptr. We don't want it to be copyable.
//
// When writing move, there are two entities involves:
//  1. a move from object
//  2. a move to object
//
// After move, the state of a move from object should be in a valid but
// unspecified state, and move to object should be the new move from.
//
// The move from state is the special case that you want to handle.
//
// Here, the intention of a unique ptr is to have an uniqeuly owned not null
// ptr, but the move introduce a move from state that the move from object
// will contain a nullptr.
// 67

template <typename T> struct OwningPtr {
    T *ptr_;

    template <typename... Args>
    explicit OwningPtr(Args &&...args)
        : ptr_(new T{ std::forward<Args>(args)... }) {}

    // avoid double free.
    ~OwningPtr() {
        if (ptr_)
            delete ptr_;
    }

    OwningPtr(const OwningPtr &) = delete;
    OwningPtr &operator=(const OwningPtr &) = delete;

    // move?
    OwningPtr(OwningPtr &&other)
        : ptr_(other.ptr_) {
        other.ptr_ = nullptr;
    }
    OwningPtr &operator=(OwningPtr &&other) {
        ptr_ = other.ptr_;
        other.ptr_ = nullptr;
    }

    // the object can be in moved from state, and in that state these two
    // operations are invalid.
    T &operator*() { return *ptr_; }
    T *operator->() { return ptr_; }
};

static_assert(std::is_move_constructible_v<OwningPtr<int>>);

// is there a way that we can forbid ppl to use an object in a moved from state?

//////////////////////////////////////////////////////////////////////////////
// think about the life time of an object as a state machine, in different
// states different oerations are valid.

//////////////////////////////////////////////////////////////////////////////
// The tricky part of handling destructive move is to avoid pointers in move
// from state and move to object share the same heap memory. Once we
// adjust that case, usually there will be no other cases that make the move
// throw.
//
// So destructive move generally are nothrow.

template <typename T> struct List {

    // a node owns it's content.
    struct Node {
        std::unique_ptr<T> content_;
        T *next;

        static std::unique_ptr<Node> make_sentinel() {
            return std::make_unique<Node>({ nullptr, nullptr });
        }

        bool is_sentinel() const {
            return content_ == nullptr && next == nullptr;
        }
    };

    static_assert(std::is_copy_constructible_v<Node>);

    std::unique_ptr<Node> node;

    List()
        : node(std::move(Node::make_sentinel())) {}
    List(List const &list) {
        // TODO
    }
};
