# DTU Wind Enenergy Controller modified for power tracking and co-simulation

This is a branch of the Basic DTU Wind Energy Controller:
https://gitlab.windenergy.dtu.dk/OpenLAC/BasicDTUController

The main modifications are:
- An MPI bridge exchanging the controller data array (avrSWAP) to a parallel simulation environment. This bridge may be used to connect to a farm controller or bypass the turbine controller
- Improved derating functionality providing tracking of power commands sent by the supercontroller (the original implementation only supports fixed open-loop derating from input file)

