cmake_minimum_required(VERSION 3.5)

project(tml VERSION 1.0
            DESCRIPTION "template meta langauge"
            LANGUAGES CXX)

message("CMAKE_BUILD_TYPE is ${CMAKE_BUILD_TYPE}")

# options
# option(TML_BUILD_FZZER "Build fuzzer tests.")
# option(TML_BUILD_TEST "Build unit tests")
option(TML_BUILD_DEMO "Build demo programs")

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# header only lib
include(CMakePackageConfigHelpers)

add_library(tml INTERFACE)
# add_library(tml::tml ALIAS tml)

target_include_directories(tml INTERFACE
  $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>
  $<BUILD_INTERFACE:${CMAKE_CURRENT_LIST_DIR}/tml>)

set_target_properties(tml PROPERTIES
  C_VISIBILITY_PRESET       hidden
  CXX_VISIBILITY_PRESET     hidden
  VISIBILITY_INLINES_HIDDEN YES
  )

if (TML_BUILD_FUZZER AND TML_BUILD_TEST)
  message(FATAL_ERROR
    "Can't build fuzzer and test together. Please choose one.")
endif()

# test targets
if(TML_BUILD_FUZZER)
  message("tml: Building fuzz tests")
  add_subdirectory(fuzzer)
endif()

if (TML_BUILD_TEST)
  message("tml: Building unit tests")
  add_subdirectory(test)
endif()

if (TML_BUILD_DEMO)
  message("tml: Building demo")
  add_subdirectory(demo)
endif()
