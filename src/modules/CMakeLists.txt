# Set the library name
project(modules LANGUAGES Fortran)

set(MODSRC_DIR ${CMAKE_CURRENT_SOURCE_DIR})

# Add a share library
# set(CMAKE_Fortran_MODULE_DIRECTORY ${MODSRC_DIR}/libs)
add_library(common_module STATIC ${MODSRC_DIR}/global_constants.f90
                                            global_variables.f90
											misc_mod.f90
											user_defined_types.f90)

# Adding all source files in ./source directory
# file(GLOB_RECURSE SRC  *.f90)

# set(LIB_TYPE STATIC)


# include(${CMAKE_CURRENT_SOURCE_DIR}/utils/ci/CMakeListsFortranLibrary.txt)
