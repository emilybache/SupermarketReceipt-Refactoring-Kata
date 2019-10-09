To compile and run your tests, you you will need:
  - make, cmake and a C++ compiler (like gcc) is installed on your system and is in the PATH
  - The GTest framework in the directory gtest.


More Verbose Instructions
-------------------------

Create a clone of both SupermarketReceipt-Refactoring-Kata and googletest in a directory we'll call ${ROOT_INSTALL_DIR}:

    cd ${ROOT_INSTALL_DIR}
    git clone https://github.com/emilybache/SupermarketReceipt-Refactoring-Kata
    git clone https://github.com/google/googletest

Make googletest by running make in subfolder googletest/googletest/make:

    cd googletest/googletest/make
    make

Create a softlink in the SupermarketReceipt-Refactoring-Kata clone pointing at the googletest code:

    cd ${ROOT_INSTALL_DIR}/SupermarketReceipt-Refactoring-Kata/cpp
    ln -s ${ROOT_INSTALL_DIR}/googletest/googletest gtest

Make the SupermarketReceipt-Refactoring-Kata:

    mkdir build
    cd build
    cmake -G "Unix Makefiles" -DGTEST_LIBRARY=../gtest/lib/libgtest.a -DGTEST_MAIN_LIRARY=../gtest/lib/libgtest_main.a -DGTEST_INCLUDE_DIR=../gtest/include .. 
    make

Then you should be able to run the tests:

    ./Supermarket

If you have been successful, then you should see a passing test, SuperMarketTest.foo that has no assertion for the moment.
