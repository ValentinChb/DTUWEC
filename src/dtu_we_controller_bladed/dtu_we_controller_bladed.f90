module dtu_we_controller_bladed

contains
!**************************************************************************************************
subroutine DTUWEC_DISCON (avrSWAP, aviFAIL, avcINFILE, avcOUTNAME, avcMSG, control_dir_in) bind(c,name='DTUWEC_DISCON')  ! VC edit: renamed and added input control_dir

    use, intrinsic :: ISO_C_Binding
    use misc_mod
    use dtu_we_controller 

    implicit none

    !DEC$ IF .NOT. DEFINED(__LINUX__)
    !DEC$ ATTRIBUTES DLLEXPORT :: DTUWEC_DISCON
    !GCC$ ATTRIBUTES DLLEXPORT :: DTUWEC_DISCON
    !DEC$ END IF

    ! New fields in avrSWAP (currently unused by ServoDyn - see Retrieve_avrSWAP in BladedInterface.f90):
    ! 85: turbine number
    ! 86: length of control directory path
    ! 87: estimated wind speed
    ! 88: derated reference rotor speed
    
    
    ! Passed in Variables from simulation codes (OpenFAST or Bladed):
    real(c_float),   intent(inout)    :: avrSWAP    (*)   ! The swap array, used to send data to, and receive data from, the DLL controller.
    integer(c_int),  intent(inout)    :: aviFAIL          ! A flag used to indicate the success of this DLL call set as follows:
                                                            !        = 0 if the DLL call was successful.
                                                            !        > 0 if the DLL call was successful but cMessage should be issued as a warning messsage.
                                                            !        < 0 if the DLL call was unsuccessful or for any other reason the simulation
                                                            !            is to be stopped at this point with cMessage as the error message.

    character(Kind=c_char), intent(in   )  :: avcINFILE (nint(avrSWAP(50)))   ! An array of 1-byte CHARACTERs giving the name of the parameter input file, 'DISCON.IN'.

    character(Kind=c_char), intent(inout)  :: avcMSG    (nint(avrSWAP(49)))   ! An array of 1-byte CHARACTERS giving the message contained in cMessage,
                                                                                ! which will be displayed by the calling program if aviFAIL <> 0.

    character(Kind=c_char), intent(inout)  :: avcOUTNAME(nint(avrSWAP(51)))     ! An array of 1-byte CHARACTERS giving the simulation run name
                                                                                ! including path without extension.

    ! VC edit
    character(Kind=c_char), intent(in   ), optional  :: control_dir_in(nint(avrSWAP(86)))           ! local directory path (instead of the original ./control), unused if directly called from OpenFAST and not through supercontroller
    
    ! Define local Variables
    real(c_double) array1(100), array2(100)
    real(c_double),save :: initpitch = 0.0 ! VC edit: initial pitch angle
    integer(4) :: i, iostat 
    integer(4), save :: callno = 0 ! VC edit: added save attribute (don't get what this variable was for if not incrementing the call number?)
    integer(c_int) :: iStatus

    CHARACTER(LEN=SIZE(avcINFILE))     :: cInFile               ! CHARACTER string giving the name of the parameter input file, 'DISCON.IN'
    CHARACTER(LEN=SIZE(avcMSG))        :: cMessage              ! CHARACTER string giving a message that will be displayed by the calling program
                                                                ! if aviFAIL <> 0.
    CHARACTER(LEN=SIZE(avcOUTNAME))    :: cOutName              ! CHARACTER string giving the simulation run name without extension.

    ! VC edit
    real(c_float)                      :: Vobs
    logical, parameter                 :: powerramp=.false.
    logical                            :: SC_flag


    iStatus = nint(avrSWAP(1))

    Vobs = avrSWAP(87)
    
    if (callno == 0 .and. iStatus == 0) then
        ! Convert c character arrays to Fortran CHARACTER strings:
        cInFile = transfer(avcINFILE(1:len(cInFile)),cInFile)
        i = index(cInFile, C_NULL_CHAR) - 1       ! Find the c NULL character at the end of cInFile, if it has then remove it
        if (i>0) cInFile = cInFile(1:i)

        ! VC edit : Get control input directory and turbine number and broadcast to global variables

        pCtrlInputFile=>null()
        if(.not. associated(pCtrlInputFile)) then
            allocate(pCtrlInputFile)
        endif

        pCtrlInputFile%name = trim(cInFile) 

#if SC
        control_dir=""
        control_dir(1:nint(avrSWAP(86))-1) = transfer(control_dir_in(1:nint(avrSWAP(86))-1),control_dir(1:nint(avrSWAP(86))-1))
        iturb=nint(avrSWAP(85))
        SC_flag=.true.
#else
        i=max(index(pCtrlInputFile%name,"/",back=.true.),index(pCtrlInputFile%name,"\",back=.true.))
        control_dir = pCtrlInputFile%name(1:i-1) ! path to root directory of the specified input file
        ! control_dir=".\control" ! Original hard-coded location
        iturb=1
        SC_flag=.false.
#endif      

        ! Get a free file unit id for additional control parameter input file
        call GetFreeFileUnitDllCall(pCtrlInputFile%fileID) 
        
        ! Open the control parameter input file for needed by Bladed/FAST

        if(fileExistsDllCall(trim(adjustl(pCtrlInputFile%name)))) then
            open(unit=pCtrlInputFile%fileID,file=trim(adjustl(pCtrlInputFile%name)),action='read')
            if(verbose .or. iturb==1) write(6,'(A)') ' Reading controller input parameters from file: '//trim(adjustl(pCtrlInputFile%name)) ! VC edit: print only if asked
        else
            write(6,*) ' ERROR: FAST/BLADED interface Control input file: '//trim(adjustl(pCtrlInputFile%name))//' does not exist.'
            stop
        endif

        ! Call a function which reads the DISCON input file and convert is to 
        ! type2_dll input array1 style controller init data block

        call type2_dll_input(pCtrlInputFile,array1)
        if (EstSpeed_flag) array1(36) = 0.0 ! VC edit: if estimated wind speed is used instead of measured, it should not be filtered.
        call init_regulation_advanced(array1, array2)

        if(verbose .or. iturb==1) write(*,*) 'Controller input parameters read successfully!' ! VC edit: print only if asked
        if(verbose) write(*,*) 'Current time: ',dble(avrSWAP( 2)) ! VC edit: print only if asked

        ! Controller initialization is done and set callno to 1 ! VC edit: commented this out (obsolete)
        ! callno = 1 
        ! array1 = 0.0_mk 

        ! VC edit: get initial pitch and initialise command to this value
        initpitch = avrSWAP(4)
        avrSWAP(42:45) = initpitch 

        ! VC edit: set estimated wind speed and reference generator speed as outputs for logging
        avrSWAP(62) = 2
        avrSWAP(65) = 2
        avrSWAP(63) = 87

    endif

    ! VC edit: Export logging channel name and unit at first and last call (according to Bladed UM)
    if (iStatus<=0) then 
        cOutName = "EstWndSpd:L/T;RefGenSpd:A/T"
        avcOUTNAME = transfer(cOutName(1:len(cOutName)-1)//C_NULL_CHAR, avcOUTNAME)
    endif

    !------------------------------ VC edit ---------------------------------------
    
    if(callno==0) then
        Deratevar%dr0 = Deratevar%dr ! Obtained from input file, derived from available power (if updated)
    elseif (Deratevar%strat>0) then
        Deratevar%dr = max(min(avrSWAP(13)/PeRated,1.0),0.0)
    endif

    if(powerramp) avrSWAP(13) = max(PeRated-ABS((FLOOR(avrSWAP(2)/100.0)+1)*PeRated/10.0-PeRated),PeRated/10.0)  ! Power ramp
    !-------------------------------------------------------------------------------

    array1 = 0.0_mk
    ! If iStatus = 0 means OpenFAST/Bladed running at time = 0.00,
    ! The DTUWEC starts to run at the second time step, eg., time = delta_t
    ! Therefore, the iStatus should be bigger than 0
    if (( iStatus > 0 ) .and. ( aviFAIL >= 0 ) )  then

        array1( 1) = dble(nint(avrSWAP( 2)*1000.0_mk)/1000.0_mk)   !    1: general time
        array1( 2) = dble(avrSWAP(20))             !    2: For Bladed/OpenFAST: convert Generator speed to rotor speed; For HAWC2: constraint bearing1 shaft_rot 1 only 2 ! VC edit: no gear conversion here, it's done in update_regulation
        ! array1( 2) = dble(avrSWAP(21))            !    2: constraint bearing1 shaft_rot 1 only 2 
        array1( 3) = dble(avrSWAP( 4))             !    3: constraint bearing2 pitch1 1 only 1
        array1( 4) = dble(avrSWAP(33))             !    4: constraint bearing2 pitch2 1 only 1
        array1( 5) = dble(avrSWAP(34))             !    5: constraint bearing2 pitch3 1 only 1
        ! if (callno==1) array1( 3:5) = initpitch  ! VC edit: repeat initial pitch angle at second time step to make it available in update_regulation
        array1( 7) = 0.0_mk                        !  6-8: wind free_wind 1 0.0 0.0 hub height
        array1( 6) = dble(merge(Vobs,avrSWAP(27),EstSpeed_flag))             !  VC edit: use estimated wind speed if actual; VC edit: swap 6 and 7, the x component should well come first??? that has no strong impact in update_regulation as total horizontal speed is used
        array1( 8) = 0.0_mk
        array1( 9) = dble(avrSWAP(15))             !    9: elec. power
        array1(10) = int(0)                        !   10: grid flag  ; [1=no grid,0=grid]
        array1(11) = dble(avrSWAP(53))             !   11: Tower top x-acceleration
        array1(12) = dble(avrSWAP(54))             !   12: Tower top y-acceleration
        array1(13) = 0.0_mk                        !   13: reserved variable for blade 1 pitch angle used by IPC
        array1(14) = 0.0_mk                        !   14: reserved variable for blade 2 pitch angle used by IPC
        array1(15) = 0.0_mk                        !   15: reserved variable for blade 3 pitch angle used by IPC
        
        call update_regulation(array1, array2)

        avrSWAP(35) = 1.0e0          ! Generator contactor status: 1=main (high speed) variable-speed generator
        avrSWAP(36) = array2(25)     ! Shaft brake status: 0=off
        avrSWAP(41) = 0.0e0          ! Demanded yaw actuator torque
        avrSWAP(42) = array2( 2)   ! Use the command angles of all blades if using individual pitch
        avrSWAP(43) = array2( 3)   ! "
        avrSWAP(44) = array2( 4)   ! "
        avrSWAP(45) = array2( 2)   ! Use the command angle of blade 1 if using collective pitch
        avrSWAP(46) = 0.0e0          ! Demanded pitch rate (Collective pitch)
        avrSWAP(47) = array2( 1)     ! Demanded generator torque 
        ! avrSWAP(48) = 0.0e0          ! Demanded nacelle yaw rate ! VC edit: that should not be overridden by the controller
        avrSWAP(55) = 0.0e0          ! Pitch override: 0=yes
        avrSWAP(56) = 0.0e0          ! Torque override: 0=yes

        ! VC edit: set estimated wind speed and reference generator speed as outputs for logging
        ! avrSWAP(65) = 0.0e0          ! Number of variables returned for logging 
        avrSWAP(62) = 2
        avrSWAP(65) = 2
        avrSWAP(63) = 87

        avrSWAP(72) = 0.0e0          ! Generator start-up resistance
        avrSWAP(79) = 0.0e0          ! Request for loads: 0=none
        avrSWAP(80) = 0.0e0          ! Variable slip current status
        avrSWAP(81) = 0.0e0          ! Variable slip current demand

        avrSWAP(88) = array2(36)     ! VC edit: Reference generator speed

    endif 
    
    ! VC edit: update effective wind speed
    if(WindEstvar%J /= 0.0) then 
        avrSWAP(87) = array2(44) 
    else
        avrSWAP(87) = avrSWAP(27)
    endif

    callno=callno+1 ! VC edit: had incrementation been omitted originally?
    
    return

end subroutine DTUWEC_DISCON


end module dtu_we_controller_bladed
