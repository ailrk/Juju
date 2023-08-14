add_executable(demo1 demo1.cc)
target_link_libraries(demo1 PRIVATE tml)
  target_precompile_headers(demo1 PRIVATE ../tml/tml.h)

add_executable(demo2 demo2.cc)
target_link_libraries(demo2 PRIVATE tml)
  target_precompile_headers(demo1 PRIVATE ../tml/tml.h)

add_executable(demo3 demo3.cc)
target_link_libraries(demo3 PRIVATE tml)
  target_precompile_headers(demo1 PRIVATE ../tml/tml.h)

set(CMAKE_CXX_COMPILER "clang++")
message("compiler is ${CMAKE_CXX_COMPILER}")

if (NOT "${CMAKE_CXX_COMPILER}" MATCHES ".*clang\\+\\+")
  message(FATAL_ERROR "Compiling with fuzzer support requires clang++")
endif()

set(TEST_COMPILER_FLAG "")
set(TEST_COMPILER_FLAG "${TEST_COMPILER_FLAG} -Wall")
set(TEST_COMPILER_FLAG "${TEST_COMPILER_FLAG} -Wextra")
set(TEST_COMPILER_FLAG "${TEST_COMPILER_FLAG} -pedantic")
set(TEST_COMPILER_FLAG "${TEST_COMPILER_FLAG} -Werror")
set_target_properties(
  demo1 demo2 demo3
  PROPERTIES
  COMPILE_FLAGS "${TEST_COMPILER_FLAG}")

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

