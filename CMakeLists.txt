cmake_minimum_required (VERSION 3.0)
# Creating the project
project(DTUWEC LANGUAGES Fortran)

# message(${CMAKE_SOURCE_DIR})

set(ROOT_DIR ${CMAKE_CURRENT_SOURCE_DIR})
set(MODSRC_DIR ${CMAKE_CURRENT_SOURCE_DIR}/src/modules)
set(IPCSRC_DIR ${CMAKE_CURRENT_SOURCE_DIR}/src/individual_pitch_controller)
set(DTUWEC_SRC_DIR ${CMAKE_CURRENT_SOURCE_DIR}/src/dtu_we_controller)
set(BLADED_SRC_DIR ${CMAKE_CURRENT_SOURCE_DIR}/src/dtu_we_controller_bladed)
set(FLAP_SRC_DIR   ${CMAKE_CURRENT_SOURCE_DIR}/src/flap_controller_individual_aep_u_f)
set(BUILD_TYPE SHARED)
set(LIB static)


message("Configuring DTU WEC source files: " ${DTUWEC_SRC_DIR})
add_subdirectory(${DTUWEC_SRC_DIR})

message("Configuring Bladed Interface source files: " ${BLADED_SRC_DIR})
add_subdirectory(${BLADED_SRC_DIR})

message("Configuring Individual Pitch Controller source files: " ${IPCSRC_DIR})
add_subdirectory(${IPCSRC_DIR})

message("Configuring flap controller source files: " ${FLAP_SRC_DIR})
add_subdirectory(${FLAP_SRC_DIR})

# Adding all source files in ./source directory
# file(GLOB_RECURSE SRC  *.f90)

# set(LIB_TYPE STATIC)

# include(${CMAKE_CURRENT_SOURCE_DIR}/utils/ci/CMakeListsFortranLibrary.txt)
