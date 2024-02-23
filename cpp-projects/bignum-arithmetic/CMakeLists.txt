cmake_minimum_required(VERSION 3.21)
project(LongArithmetics)

set(CMAKE_CXX_STANDARD 17)

add_executable(LongArithmetics main.cpp LN.h return_codes.h stack.h stack.cpp LN.cpp parser.h parser.cpp)
