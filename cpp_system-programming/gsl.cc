// Guideline support library
// - pointer ownership
// - expectation management
// - no pointer arithmetic

#define GSL_THROW_ON_CONTRACT_VIOLATION
#include <gsl/gsl>
#include <gsl/gsl_assert>
#include <iostream>

int main(void) {

    {
        // owned ptr to control pointer ownership.

        auto init = [](int *p) { *p = 0; };
        gsl::owner<int *> p = new int;
        init(p);
        delete p;
    }

    {
        // a null will throw an exception
        auto relay = [](gsl::not_null<int *> p) { return p; };
        auto p1 = std::make_unique<int>();
        auto p2 = relay(gsl::not_null(p1.get()));
    }

    {
        // avoid ptr arithmetics with span or string view.

        int array[5] = { 1, 2, 3, 4, 5 };
        auto span = gsl::span(array);

        for (auto const &e : span) {
            std::clog << e << std::endl;
        }

        try {
            std::clog << span[5] << std::endl;

        } catch (std::exception &e) {
            std::cout << "exception: " << e.what() << std::endl;
        }
    }

    return 0;
}
