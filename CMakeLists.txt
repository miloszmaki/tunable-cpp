cmake_minimum_required(VERSION 3.10)

project(tunable-cpp)

add_executable(test1 tests/test1.cpp)

add_executable(example1 examples/example1.cpp)
add_executable(example2 examples/example2.cpp)
add_executable(example3 examples/example3.cpp)
add_executable(example4 examples/example4.cpp)

include_directories(".")

target_compile_features(test1 PRIVATE cxx_std_17)

target_compile_features(example1 PRIVATE cxx_std_17)
target_compile_features(example2 PRIVATE cxx_std_17)
target_compile_features(example3 PRIVATE cxx_std_17)
target_compile_features(example4 PRIVATE cxx_std_17)