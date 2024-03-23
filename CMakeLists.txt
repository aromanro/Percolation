cmake_minimum_required(VERSION 3.10)

enable_language(Fortran)
get_filename_component(Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)
project(Percolation)

set(default_build_type "Release")
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE "Release" CACHE STRING "Choose Release or Debug" FORCE)
endif()

set(SOURCES Calc.f90
            Grid.f90
            Stats.F90
            Percolation.f90)

add_executable(Percolation ${SOURCES})
