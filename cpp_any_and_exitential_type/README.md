## C++ any, callback, and exitential wrapper.

For any langauges with a runtime erased type system, exitential type will be very helpful tool in the toolbox.

The easist way to erase type is to hold a value with void pointer, maybe bundle it with a tag so we can do runtime dispatch. Template can be used to erase type too. A tempalted constructor essentially hide the parameter's type and provides a universal interface to the outside world.
