cmake_minimum_required(VERSION 3.5)

set(PROJECT_VERSION 1.0)
project(
  alpaca_lisp
  VERSION ${PROJECT_VERSION}
  DESCRIPTION "Lisp for alpacas"
  LANGUAGES CXX
)

message("[alpaca_lisp] CMAKE_BUILD_TYPE is ${CMAKE_BUILD_TYPE}")

# header only lib
include(CMakePackageConfigHelpers)

# options
option(SECDXD_BUILD_TEST "Build unit tests")
option(SECDXD_BUILD_FUZZER "Build fuzzer tests.")
option(SECDXD_BUILD_BECHMARK "Build benchmarks")
option(SECDXD_BUILD_DEMO "Build demo programs")

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON CACHE INTERNAL "")

set(PUBLIC_INCLUDE_DIR "include")

add_library(
  alpaca_lisp
  "include/parse.h"

  "src/parse.cpp"
)


target_include_directories(
  alpaca_lisp
  PUBLIC
  $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/include>
)

# Compile
if(SECDXD_BUILD_FUZZER AND SECDXD_BUILD_TEST)
  message(
    FATAL_ERROR
    "Can't build fuzzer and test together. Please choose one."
  )
endif()

if(SECDXD_BUILD_TEST)
  message("[alpaca_lisp]: Building unit tests")
  add_subdirectory(test)
endif()

# # test targets
# if(SECDXD_BUILD_FUZZER)
#   message("[alpaca_lisp]: Building fuzz tests")
#   add_subdirectory(fuzzer)
# endif()

# if(SECDXD_BUILD_DEMO)
#   message("[alpaca_lisp]: Building demo")
#   add_subdirectory(demo)
# endif()
