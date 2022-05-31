# DTU Wind Energy Controller modified for power tracking and co-simulation

This is a branch of the Basic DTU Wind Energy Controller:
https://gitlab.windenergy.dtu.dk/OpenLAC/BasicDTUController

The main modifications are:
- An MPI bridge exchanging the controller data array (avrSWAP) to a parallel simulation environment, which may be used to connect and sync with a farm controller (or even bypass the entire turbine controller) in a distinct environment (e.g. Simulink) in FAST.farm simulations
- Improved derating functionality providing tracking of power commands sent by the farm controller (the original implementation only supports fixed open-loop derating from input file)

# Compiling
- In Cmake, set the boolean variable CMAKE_LINK2MPI to true if you want to use the MPI bridge (set to false if you plan to use the controller in OpenFAST only, or in FAST.farm's with inbuilt supercontroller)
- If you want to import functionality from the ROSCO controller, set the variable CMAKE_LINK2ROSCO to true. In the current implementation, the wind speed observer from ROSCO will be used.
- Version control with utils submodule: It is recommended to clone the two repositories separately instead of using git clone --recurse-submodules or similar. Make sure a valid version is defined by using the git tag command appropriately.

