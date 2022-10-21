cmake_minimum_required(VERSION 3.10)

project(tunable-examples)

add_executable(example1 examples/example1.cpp)
add_executable(example2 examples/example2.cpp)
add_executable(example3 examples/example3.cpp)
add_executable(example4 examples/example4.cpp)

include_directories(".")

set (CMAKE_CXX_STANDARD 17)
