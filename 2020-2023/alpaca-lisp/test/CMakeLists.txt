enable_testing()

# TEST AS EXCUTABLE
if ("${CMAKE_BUILD_TYPE}" STREQUAL "Debug")
  set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fsanitize=address")
endif()

message("compiler is ${CMAKE_CXX_COMPILER}")

# FLAGs
set(TEST_COMPILER_FLAG "")
set(TEST_COMPILER_FLAG "${TEST_COMPILER_FLAG} -Wall")
set(TEST_COMPILER_FLAG "${TEST_COMPILER_FLAG} -Wextra")
# set(TEST_COMPILER_FLAG "${TEST_COMPILER_FLAG} -pedantic")
# set(TEST_COMPILER_FLAG "${TEST_COMPILER_FLAG} -Werror")

# for ninja output
option(FORCE_COLORED_OUPUT "Always produce ANSI-colored output (gcc/clang)"
  TRUE)
if (${FORCE_COLORED_OUPUT})
  if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU")
    add_compile_options(-fdiagnostics-color=always)
  elseif("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
    add_compile_options(-fcolor-diagnostics)
  endif()
endif()

# define test
function(alpaca_lisp_test test_file)
  get_filename_component(test_target_name "${test_file}" NAME_WE)

  add_executable("${test_target_name}" "")
  target_sources("${test_target_name}"
    PRIVATE
    "${test_file}")

  target_include_directories("${test_target_name}"
    PRIVATE
    $<TARGET_PROPERTY:alpaca_lisp,INTERFACE_INCLUDE_DIRECTORIES>)

  target_link_libraries("${test_target_name}" alpaca_lisp)

  target_compile_definitions("${test_target_name}"
    PRIVATE
    # CHECK_UNIMPLEMENTED=1
    )

  # DEBUG FLAGS
  set_target_properties("${test_target_name}"
    PROPERTIES
    COMPILE_FLAGS "${TEST_COMPILER_FLAG}")
endfunction(alpaca_lisp_test)

alpaca_lisp_test("basic/test1.cpp")
alpaca_lisp_test("basic/test2.cpp")
