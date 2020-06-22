# Set the project name
# project(individual_pitch_controller LANGUAGES Fortran)
project(cyclic_pitch_controller LANGUAGES Fortran)

message("Configuring Sub-project: " ${PROJECT_NAME})
message("Root Directory of this Sub-project: " ${ROOT_DIR})

# set source code
set(IPCSRC_DIR ${CMAKE_CURRENT_SOURCE_DIR})


file(GLOB_RECURSE MODSRC "${ROOT_DIR}/src/modules/global_constants.f90"
                         "${ROOT_DIR}/src/modules/global_variables.f90"
					     "${ROOT_DIR}/src/modules/misc_mod.f90"
					     "${ROOT_DIR}/src/modules/user_defined_types.f90"
	)

#set(SRC ${IPCSRC_DIR}/individual_pitch_controller.f90 
#                      individual_pitch_controller_fcns.f90					  
#	)
	
set(SRC ${IPCSRC_DIR}/cyclic_pitch_controller.f90 
                      cyclic_pitch_controller_fcns.f90					  
	)
list(APPEND SRC ${MODSRC})

include(${ROOT_DIR}/utils/cmake/CMakeLists.txt)

message("Including the utils: " "${ROOT_DIR}/utils/cmake/CMakeLists.txt")

