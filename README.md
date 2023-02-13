# DTU Wind Energy Controller modified for power tracking and co-simulation

This is a branch of the Basic DTU Wind Energy Controller:
https://gitlab.windenergy.dtu.dk/OpenLAC/BasicDTUController

The main modifications are:
- Adaptation for linking with a supercontroller featuring a MPI bridge (https://github.com/ValentinChb/SC_MPIClient) exchanging the controller data array (avrSWAP) to a parallel simulation environment, which may be used to connect and sync with a farm controller (or even bypass the entire turbine controller) in a distinct environment (e.g. Simulink) in FAST.Farm simulations
- Improved derating functionality providing tracking of power commands sent by the farm controller (the original implementation only supports fixed open-loop derating from input file)

# Installation
- Make sure BLAS and LAPACK dependencies are in PATH
- Binaries for Windows 64 bits with and without static link for use in supercontroller (.a or .dll) are provided. Recompiling (recommended) should be done using CMake and a fortran compiler. The MinGW/GNU suite has been used in this project. Intel fortran compiler and linux environments have not been tested.
- The utils submodule is not included in this repository. It is recommended to clone the two repositories separately instead of using git clone --recurse-submodules or similar. Make sure a valid version is defined by using the git tag command appropriately. Then, update The cmake config file CMakeLists.txt in utils/cmake by copying and renaming the file CMakeList_utils.txt from this repository (it implements the necessary adaptations in fortran compiler and linker flags).

