cmake_minimum_required (VERSION 3.0)
#Creating the project
project(dtu_we_controller.dll LANGUAGES Fortran)


# Adding all source files in ./source directory
#file(GLOB_RECURSE SRC  *.f90)

#set(LIB_TYPE STATIC)


include(${CMAKE_CURRENT_SOURCE_DIR}/utils/ci/CMakeListsFortranLibrary.txt)
