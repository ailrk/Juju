#include <exception>
#include <iostream>
#include <memory>

// an any is a wrapper for void pointer with runtime type infomation attach to
// it.

struct bad_any_cast : std::exception {
    const char *what() const noexcept { return "bad"; }
};

struct any {
    void *data_;                        // holds the data itself.
    std::type_info const (*getType_)(); // get rtti
    void *(*clone_)(void *otherData);   // copy data owned.
    void *(*destory_)(void *data);      // destruct data owned.

    // construct explicitly to avoid type coersion. This ensures the value we
    // store is the value we pass.
    template <typename T>
    explicit any(T &&value) // a paramatric polymorphic parameter. T is erased.
        : data_{ new T{ std::forward<T>(value) } }
        , getType_{ []() -> std::type_info const & { return typeid(T); } }
        , clone_{ [](void *otherData) -> void * {
            return new T(*static_cast<T *>(otherData));
        } }
        , destory_{ [](void *data_) { delete static_cast<T *>(data_); } } {}

    any(any const &other)
        : data_(other.clone_(other.data_))
        , getType_(other.getType_)
        , clone_(other.clone_)
        , destory_(other.destory_) {}

    ~any() { destory_(data_); }
};

template <typename T> T &any_case(any &a) {
    if (typeid(T) == a.getType_()) { // boils down to runtype comparing string.
        return *static_cast<T *>(a.data_);
    } else {
        throw bad_any_cast{};
    }
}

// PS: std::function is also a type of type erasure.
