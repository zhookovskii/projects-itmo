cmake_minimum_required(VERSION 3.21)
project(lab2 C)

set(CMAKE_C_STANDARD 11)

add_executable(lab2 main.c)
target_link_libraries(lab2 D:zlib-1.2.12/cmake-build-release/libzlib.dll)
