cmake_minimum_required(VERSION 3.8)
project(Supermarket)

set(CMAKE_CXX_STANDARD 17)
include_directories(third_party)

enable_testing()
add_subdirectory(model)
add_subdirectory(test)
