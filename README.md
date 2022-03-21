# DTU Wind Energy Controller modified for power tracking and co-simulation

This is a branch of the Basic DTU Wind Energy Controller:
https://gitlab.windenergy.dtu.dk/OpenLAC/BasicDTUController

The main modifications are:
- An MPI bridge exchanging the controller data array (avrSWAP) to a parallel simulation environment, which may be used to connect and sync with a farm controller (or even bypass the entire turbine controller) in a distinct environment (e.g. Simulink)
- Improved derating functionality providing tracking of power commands sent by the farm controller (the original implementation only supports fixed open-loop derating from input file)

