cmake_minimum_required(VERSION 3.8)
project(Supermarket.tests)
set(CMAKE_CXX_STANDARD 17)
set(SOURCE_FILES
        main.cpp
        SuperMarketTest.cpp
        FakeCatalog.cpp
        FakeCatalog.h)
add_executable(${PROJECT_NAME} ${SOURCE_FILES} )
find_library(Supermarket.model model ../model)
target_link_libraries(${PROJECT_NAME}  LINK_PUBLIC Supermarket.model)
add_test(NAME Supermarket.tests COMMAND Supermarket.tests)
