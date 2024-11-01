module turbine_controller_mod
   ! ===============================================================================
   ! Module containing the main subroutines for the wind turbine regulation.
   ! ===============================================================================
   ! use user_defined_types
   ! use global_variables
   use dtu_we_controller_fcns
   use logging

   implicit none
!**************************************************************************************************
contains
!**************************************************************************************************
subroutine turbine_controller(CtrlStatus, GridFlag, GenSpeed, PitchVect, wsp, Pe, TTAccVect, &
                              GenTorqueRef, PitchColRef, dump_array)
   !
   ! Main subroutine of the wind turbine controller.
   ! It calls the system monitoring and selects the controller status (normal operation, start-up,
   ! and shut-down).
   !
   integer , intent(inout) :: CtrlStatus ! Integer indicating the status of the controller.&
                                         !  (0: Normal operation, <0: Start-up., >0: Shutdown)
   integer , intent(inout) :: GridFlag   ! Integer indicating the status of the grid.
   real(mk), intent(inout) :: dump_array(50) ! Array for output.
   real(mk), intent(in)    :: GenSpeed     ! Measured generator speed [rad/s].
   real(mk), intent(in)    :: PitchVect(3) ! Measured pitch angles [rad].
   real(mk), intent(in)    :: wsp          ! Measured wind speed [m/s].
   real(mk), intent(in)    :: Pe           ! Measured electrical power [W].
   real(mk), intent(in)    :: TTAccVect(2) ! Measured tower top acceleration. Longitudinal and &
                                           !  lateral components [m/s**2].
   real(mk), intent(out)   :: GenTorqueRef ! Generator torque reference [Nm].
   real(mk), intent(out)   :: PitchColRef  ! Reference collective pitch [rad].
   real(mk) TTAcc
   !***********************************************************************************************
   ! Wind turbine monitoring
   !***********************************************************************************************
   TTAcc = dsqrt(TTAccVect(1)**2 + TTAccVect(2)**2)
   call monitoring(CtrlStatus, GridFlag, GenSpeed, TTAcc, PitchVect, PitchColRefOld, dump_array)
   !***********************************************************************************************
   ! Control Status CtrlStatus = 0
   ! VC edit: embedded derating in normal_operation
   if (CtrlStatus .eq. 0) then
      call normal_operation(GenSpeed, PitchVect, wsp, Pe, TTAccVect(2), GenTorqueRef, PitchColRef,&
                            dump_array)
   endif

   ! !-----------------------------------------------------------------------------------------------
   ! ! Normal operation
   ! !-----------------------------------------------------------------------------------------------
   
   ! if (CtrlStatus .eq. 0 .and. Deratevar%strat .eq. 0) then

   !    call normal_operation(GenSpeed, PitchVect, wsp, Pe, TTAccVect(2), GenTorqueRef, PitchColRef,&
   !                          dump_array)

   !  !-----------------------------------------------------------------------------------------------
   !  ! Derate operation
   !  !-----------------------------------------------------------------------------------------------
   ! elseif (CtrlStatus .eq. 0 .and. Deratevar%strat .gt. 0) then
   !     call derate_operation(GenSpeed, PitchVect, wsp, Pe, TTAccVect(2), GenTorqueRef, PitchColRef,&
   !                           dump_array)
   ! endif

   !***********************************************************************************************
   ! Cut-in procedure for control status CtrlStatus = -1
   !***********************************************************************************************
   if (CtrlStatus .lt. 0) then
      if (CtrlStatus .eq. -2) then ! First pitch out
         call shut_down(CtrlStatus, GenSpeed, PitchVect, wsp, GenTorqueRef, PitchColRef, &
                        dump_array)
      else
         call start_up(CtrlStatus, GenSpeed, PitchVect, wsp, GenTorqueRef, PitchColRef, dump_array)
      endif
   endif
   !***********************************************************************************************
   ! Cut-out procedure for control status CtrlStatus > 0
   !***********************************************************************************************
   if (CtrlStatus .gt. 0) then
      call shut_down(CtrlStatus, GenSpeed, PitchVect, wsp, GenTorqueRef, PitchColRef, dump_array)
   endif
   return
end subroutine turbine_controller
!**************************************************************************************************
subroutine normal_operation(GenSpeed, PitchVect, wsp, Pe, TTfa_acc, GenTorqueRef, PitchColRef, dump_array)
   !
   ! Controller for normal operation.
   !
   real(mk), intent(in)    :: PitchVect(3) ! Measured pitch angles [rad].
   real(mk), intent(in)    :: GenSpeed     ! Measured generator speed [rad/s].
   real(mk), intent(in)    :: wsp          ! Measured wind speed [m/s].
   real(mk), intent(in)    :: Pe           ! Measured electrical power [W].
   real(mk), intent(in)    :: TTfa_acc     ! Measured tower top longitudinal acceleration.
   real(mk), intent(out)   :: GenTorqueRef   ! Generator torque reference [Nm].
   real(mk), intent(out)   :: PitchColRef    ! Reference collective pitch [rad].
   real(mk), intent(inout) :: dump_array(50) ! Array for output.
   real(mk) WSPfilt
   real(mk) GenSpeedFilt, dGenSpeed_dtFilt,PeFilt
   real(mk) PitchMean, PitchMeanFilt, PitchMin
   real(mk) GenSpeedRef_full
   real(mk) Qdamp_ref, theta_dam_ref, P_filt
   real(mk) x, y(2)
   real(mk) estAeroTorq, estLambda, estREWS ! Rotor effective wind speed estimator
   !***********************************************************************************************
   ! Inputs and their filtering
   !***********************************************************************************************
   ! Mean pitch angle
   PitchMean = (PitchVect(1) + PitchVect(2) + PitchVect(3)) / 3.0_mk
   ! Low-pass filtering of the rotor speed
   y = lowpass2orderfilt(deltat, stepno, omega2ordervar, GenSpeed)
   GenSpeedFilt = y(1)
   dGenSpeed_dtFilt = y(2)
   ! Low pass filtered power
   y = lowpass2orderfilt(deltat, stepno, power2ordervar, Pe)
   PeFilt=y(1)
   ! Low-pass filtering of the mean pitch angle for gain scheduling
   PitchMeanFilt = lowpass1orderfilt(deltat, stepno, pitchfirstordervar, PitchMean)
   PitchMeanFilt = min(PitchMeanFilt, 30.0_mk*degrad)
   ! Low-pass filtering of the nacelle wind speed
   WSPfilt = lowpass1orderfilt(deltat, stepno, wspfirstordervar, wsp)
   ! Minimum pitch angle may vary with filtered wind speed
   PitchMin = GetOptiPitch(WSPfilt)
   !***********************************************************************************************
   ! Limit reference speed for storm control
   !***********************************************************************************************
   if (Vcutout .gt. Vstorm) then
      GenSpeedRef_full = GenSpeedRefMax - max(0.0_mk, &
                         (WSPfilt - Vstorm)/(Vcutout - Vstorm)*(GenSpeedRefMax - GenSpeedRefMin))
   else
      GenSpeedRef_full = GenSpeedRefMax
   endif
   GenSpeedRef_full = max(min(GenSpeedRef_full, GenSpeedRefMax), GenSpeedRefMin)
   !***********************************************************************************************
   ! Update reference speed and torque for downregulation if actual (VC edit)
   !***********************************************************************************************
   if (Deratevar%strat.ne.0) then
      call derate()
   endif
   !***********************************************************************************************
   ! PID regulation of generator torque
   !***********************************************************************************************
   call torquecontroller(GenSpeed, GenSpeedFilt, dGenSpeed_dtFilt, PitchMean, WSPfilt, PitchMin, &
                         GenSpeedRef_full, Pe, GenTorqueRef, dump_array)
   !***********************************************************************************************
   ! Active DT damping based on filtered rotor speed
   !***********************************************************************************************
   call drivetraindamper(GenSpeed, Qdamp_ref, dump_array)
   if (newtimestep) TimerGenCutin = TimerGenCutin + deltat
   x = switch_spline(TimerGenCutin, CutinVar%delay, 2.0_mk*CutinVar%delay)
   GenTorqueRef = min(max(GenTorqueRef + Qdamp_ref*x, 0.0_mk), GenTorqueMax)
   !***********************************************************************************************
   ! PID regulation of collective pitch angle
   !***********************************************************************************************
   call pitchcontroller(GenSpeedFilt, dGenSpeed_dtFilt, PitchMeanFilt, PeFilt, PitchMin, &
                        GenSpeedRef_full, PitchColRef, dump_array)
   !***********************************************************************************************
   ! Active Tower damping based on filtered tower top aceleration
   !***********************************************************************************************
   P_filt = lowpass1orderfilt(deltat, stepno, TTfa_PWRfirstordervar, GenTorqueRef*GenSpeedFilt)
   call towerdamper(TTfa_acc, theta_dam_ref, dump_array)
   x = switch_spline(P_filt, TTfa_PWR_lower*PeRated, TTfa_PWR_upper*PeRated)
   PitchColRef = min(max(PitchColRef + theta_dam_ref*x, PID_pit_var%outmin), PID_pit_var%outmax)
   ! call log_info('pitch_twr_damper-ref: ',theta_dam_ref)
   !
   !***********************************************************************************************
   ! Calculate the estimated aerodynamic torque and effective wind speed
   !***********************************************************************************************
   if (WindEstvar%J > 0.0_mk) then
    call windEstimator(GenTorqueRef, GenSpeed, PitchMean, WindEstvar, Cpdata, deltat,estAeroTorq,estLambda,estREWS,WSPfilt )
    ! Write into dump array 
    dump_array(29) = estAeroTorq 
    dump_array(39) = estLambda ! [-]
    dump_array(40) = estREWS 
   !  print*,estLambda, estREWS
   endif
    
   ! Write into dump array
   dump_array(1) = GenTorqueRef*GenSpeed
   dump_array(2) = WSPfilt
   dump_array(3) = GenSpeedFilt
   dump_array(20) = PitchMeanFilt
 
   return

   contains

   subroutine derate()
      ! Fixed parameter variables
      logical, save        :: firstStep   = .true.
      logical, parameter   :: filt_derate = .false.
      real(mk), save       :: Rated_wsp   = 11.5_mk ! Use a default value in case not specified as input
      real(mk), save       :: KoptDerating = 1.0_mk, Kopt_normal = 1.0_mk
      real(mk), save       :: GenSpeedRefMax_normal ! Max generator speed in normal operation
      real(mk), save       :: Cpmax ! Max power coefficient in input downregulation data
      real(mk), save       :: TSRmin, drrelmin ! Min TSR and derating factor in input downregulation data
      ! Operating point variables
      real(mk)             :: GenSpeedDerate ! Rotor speed setpoint to be tracked
      real(mk)             :: Cpderate, lambdaDerate ! Desired power coefficient and TSR 
      real(mk), save       :: PitchMinDerate ! Pitch angle yielding Cpderate and lambdaderate based on performance curves
      ! Local variables
      logical              :: update_derate
      integer              :: i
      real(mk)             :: drabs, drrel ! Filtered derating factor
      real(mk)             :: GenSpeedfiltDerate, WSPfiltDerate, PitchfiltDerate ! Slowly-varying operating point used to update fixed pitch for partial load derating strategy
      real(mk)             :: GenSpeedOptLimit2 ! Generator speed upper limit yielding lambdaDerate, ideal (unsaturated) value of GenSpeedDerate
      real(mk)             :: Pea ! Available power
      ! Debug variables
      character(1)         :: str
      logical              :: debugFlag = .false.

      !***********************************************************************************************
      ! Inputs and their filtering
      !***********************************************************************************************
      if(filt_derate) then 
         if(firstStep) then
            Deratevar%dr2ordervar%f0   = 0.1 ! 10s period. that will leed to an update every 2s. Make sure this does not lead to inconsistent downsampling of rotor dynamics/effective wind speed.
            Deratevar%dr2ordervar%zeta = sqrt(2.0) 
         endif
         y = lowpass2orderfilt(deltat, stepno, Deratevar%dr2ordervar, Deratevar%dr)
         drabs = y(1)
      else
         drabs = Deratevar%dr
      endif

      if (firstStep) then
         Deratevar%Nsteps_update = nint(1/Deratevar%dr2ordervar%f0/deltat/4) ! update frequency is set well under (4 times) cut-off frequency to avoid aliasing
         GenSpeedRefMax_normal   = GenSpeedRefMax
      endif
      update_derate = .not. filt_derate .or. mod(stepno-1,Deratevar%Nsteps_update)==0 ! update every call or Nsteps_update (includes init)
      
      if (Deratevar%strat.ge.4) then
         if (firstStep) then
            Kopt_normal = Kopt ! save the generator constant for normal optimal Cp tracking
            PitchMinDerate=PitchMin
            Cpmax = maxval(downRegulationData%Cp(1:downRegulationData%NumLines,1))
            TSRmin = minval(downRegulationData%Lambda(1:downRegulationData%NumLines))
            drrelmin = minval(downRegulationData%dCp(1:downRegulationData%NumLines))
            ! Keep the rated wind speed during derating
            if(RatedWindSpeed .gt. 0.0) then
               Rated_wsp = RatedWindSpeed
            endif
         endif

         if(filt_derate) then ! use much slower dynamics
            if (firstStep) then
               Deratevar%omega2ordervar%f0   = 0.005_mk ! 200s period
               Deratevar%omega2ordervar%zeta = sqrt(2.0_mk)    
               Deratevar%wspfirstordervar%tau = 1.0_mk/Deratevar%omega2ordervar%f0*2.0_mk*pi/GenSpeedRefMax
               Deratevar%Pitch2ordervar = Deratevar%omega2ordervar
            endif
            y = lowpass2orderfilt(deltat, stepno, Deratevar%omega2ordervar, GenSpeed)
            GenSpeedfiltDerate = y(1)
            y = lowpass1orderfilt(deltat, stepno, Deratevar%wspfirstordervar, wsp)
            WSPfiltDerate = y(1)
            Pea = min(Cpmax*0.5*rho*pi*R**2*WSPfiltDerate**3,PeRated)
            y = lowpass2orderfilt(deltat, stepno, Deratevar%Pitch2ordervar, PitchColRefOld)
            PitchfiltDerate = y(1)
         else ! use original filtering
            GenSpeedfiltDerate = GenSpeedfilt
            WSPFiltDerate = WSPfilt
            PitchfiltDerate = PitchMeanfilt
         endif
      endif

      !****************************************************************************************************
      ! Apply derating strategy to update reference generator speed and torque 
      !****************************************************************************************************      
      if (update_derate) then

         select case (Deratevar%strat)
         ! Derating strategies:
         !1. full-load operation, pitch control tracking constant rotor speed corresponding to normal operation for initial (fixed) power setpoint
         !2. full-load operation, pitch control tracking rated rotor speed
         !3. Same as 1. but using time-varying power setpoint to get ref rotor speed
         !4. partial-load operation, adjusting pitch angle and derated tracking factor K to provide desired power at a TSR that minimizes thrust coefficient
         !5. partial-load operation, generalization of 4., adjusting pitch angle and derated tracking factor K to provide desired power at a selected TSR (for instance normal operation-equivalent)
         case(1) 
            GenSpeedDerate = ((Deratevar%dr0*PeRated)/Kopt)**(1.0_mk/3.0_mk) ! Derated Rotor Speed: Remark: this is not completely right ! VC edit: fixed to initial value (mode=1: constant speed)
         case(2)
            GenSpeedDerate = GenSpeedRefMax
         case(3)
            GenSpeedDerate = ((drabs*PeRated)/Kopt)**(1.0_mk/3.0_mk)
         case(4:5)
            drrel = drabs*PeRated/Pea ! dCp relates to derating factor wrt available power, not rated power
            if(Deratevar%strat==4 .and. drrel >= drrelmin) then  ! minimum Ct only if within interpolation bounds
               ! Calculate the tip-speed-ratio and pitch angle through the dCp-Lambda and dCp-beta relation table
               do i = 1,(downRegulationData%NumLines - 1)
                     if (drrel<=downRegulationData%dCp(i) .and. drrel >= downRegulationData%dCp(i+1)) then
                        lambdaAtMinCt = interpolate(drrel, downRegulationData%dCp(i),downRegulationData%dCp(i+1), downRegulationData%Lambda(i), downRegulationData%Lambda(i+1))
                        pitchAtMinCt = interpolate(drrel, downRegulationData%dCp(i),downRegulationData%dCp(i+1), downRegulationData%Pitch(i), downRegulationData%Pitch(i+1))
                        pitchAtMinCt = pitchAtMinCt*degrad  ! convert to rads
                        minCt = interpolate(drrel, downRegulationData%dCp(i),downRegulationData%dCp(i+1), downRegulationData%Ct(i,1), downRegulationData%Ct(i+1,1))
                     endif
               enddo
               ! get the Cp at the derating level
               CpAtMinCt = Cpmax*drrel

               ! Output operating point yielding minimum Ct at desired Cp
               lambdaDerate = lambdaAtMinCt
               Cpderate = CpAtMinCt
               PitchMinDerate = pitchAtMinCt

            else ! if Deratevar%strat==5 or outside interpolation bounds (high wind speeds, low power setpoints)
               ! Operating point yielding desired TSR at desired Cp
               lambdaDerate = TSR_opt*Rated_wsp/max(WSPfiltDerate,Rated_wsp) ! desired TSR, equal that in normal operation at current wind speed
               Cpderate = drabs*PeRated/(0.5*rho*pi*R**2*WSPfiltDerate**3) ! desired Cp
               PitchMinDerate = GetOptiPitchDerate(Cpderate,lambdaDerate,PitchMinDerate,PitchMin,Cpdata) 
            endif

            ! compute generator constant for operating point tracking (generalized minCt)
            KoptDerating = 0.5_mk*rho*pi*R**5*(1.0_mk/lambdaDerate)**3*Cpmax*drabs 
            ! KoptDerating = KoptDerating * (1.0-exp(PitchErrfiltfilt)) ! Make sure KoptDerating decreases when pitch angle is approaching 0 to force fullload operation
            if(PitchfiltDerate .le. SwitchVar%pitang_upper) then ! When approaching full-load operation, switch back Kopt to full value
               KoptDerating = Kopt_normal
            endif

            ! Overwrite Kopt using the value for operating point tracking (that will be used in torque_controller)
            Kopt = KoptDerating

            GenSpeedOptLimit2 = max(Rated_wsp,WSPfiltDerate) * lambdaDerate / R
            GenSpeedDerate = GenSpeedOptLimit2/SwitchVar%rel_sp_open_Qg

         endselect

         GenSpeedDerate = max(GenSpeedDerate,GenSpeedRefMin) ! VC edit: apply lower bound here already to get correct GenSpeedRefMax and GenTorqueRated
         GenSpeedRefMax = min(GenSpeedRefMax_normal,GenSpeedDerate) ! VC edit: do this for all cases in a single statement after select block
      endif
      GenSpeedDerate = GenSpeedRefMax
      GenSpeedRef_full = min(GenSpeedRefMax, GenSpeedRef_full)
      ! VC edit: update GenTorqueRated at every call. Do this for all cases in a single statement after select block
      GenTorqueRated = (Deratevar%dr*PeRated)/GenSpeedDerate ! This the derated generator torque. Choosing constant power mode in full load will enable power tracking in torquecontroller. 
      dump_array(32) = GenSpeedDerate      ! Save rated generator speed when derating

      if (Deratevar%strat.ge.4 .and. .not. firstStep) then ! VC edit: Split IF block.
      
         ! Minimum pitch angle may vary with the de-rating percentage
         ! PitchMin = max(PitchMin,PitchMinDerate) ! WARNING: doing this, we introduce a lower bound constraint and loose power tracking ability
         ! Dump output to array
         dump_array(30) = Kopt                ! Save generator constant when derating
         dump_array(31) = lambdaDerate       ! Save tip-speed-ratio at minimum Ct ! VC edit: generalize minCt
         dump_array(33) = PitchMinDerate     ! Save required pitch angle when operating at minimum Ct strategy ! VC edit: generalize minCt
         dump_array(34) = minCt               ! Save the thrust coefficient when derating
         dump_array(35) = Cpderate           ! Save the power coefficient when derating  ! VC edit: generalize minCt
         dump_array(36) = GenTorqueRated      ! Save rated generator torque when derating 
         dump_array(37) = GenSpeedOptLimit2   ! Save generator speed upper limit when follows the Kopt*w^2 relation when derating 
         dump_array(38) = Kopt*GenSpeedOptLimit2**2  ! Save generator torque upper limit which follows the Kopt*w^2 relation when derating 

         ! output for debug purpose
         if(debugFlag .eqv. .true.) then
               write(*,*) "Press 'c' to continue, 'q' to Quit the code."
               read(*,*) str
               if(str == 'q') then
                  stop
               elseif(str == 'c') then
                  write(*,*) "simulation continuing...."
               endif
         endif

      endif     

     

      firstStep = .false. 

   end subroutine derate

end subroutine normal_operation
!**************************************************************************************************
subroutine derate_operation(GenSpeed, PitchVect, wsp, Pe, TTfa_acc, GenTorqueRef, PitchColRef,dump_array)
   ! Controller for derate operation.
   ! Declare the input arguments for the subroutine
   real(mk), intent(in)    :: PitchVect(3) ! Measured pitch angles [rad].
   real(mk), intent(in)    :: GenSpeed     ! Measured generator speed [rad/s].
   real(mk), intent(in)    :: wsp          ! Measured wind speed [m/s].
   real(mk), intent(in)    :: Pe           ! Measured electrical power [W].
   real(mk), intent(in)    :: TTfa_acc     ! Measured tower top longitudinal acceleration.
   real(mk), intent(out)   :: GenTorqueRef   ! Generator torque reference [Nm].
   real(mk), intent(out)   :: PitchColRef    ! Reference collective pitch [rad].
   real(mk), intent(inout) :: dump_array(50) ! Array for output.

   ! Declare the local variables
   real(mk) WSPfilt
   real(mk) GenSpeedFilt, dGenSpeed_dtFilt,PeFilt
   real(mk) PitchMean, PitchMeanFilt, PitchMin
   real(mk) GenSpeedRef_full , GenSpeedDerate
   real(mk) Qdamp_ref, theta_dam_ref, P_filt
   real(mk) x, y(2)
   integer :: i
   logical :: firstStep = .true.
   real(mk), save :: KoptDerating = 1.0_mk, Kopt_normal = 1.0_mk
   ! real(mk) :: avgRotorSpeedDWM, avgPitchAngleDWM, avgCtDWM, downRegulationRatio
   ! real(mk) :: GenTorqueRatio = 1.0_mk, GenTorqueOptLimit2 = 0.0_mk
   real(mk) :: GenSpeedOptLimit2 = 0.0_mk
   real(mk) :: GenSpeedMaxDerate = 0.0_mk
   real(mk) :: Rated_wsp = 11.5_mk
   ! real(mk) :: sumWindspeedDWM = 0.0_mk
   ! real(mk) :: sumPitchAngleDWM = 0.0_mk
   ! real(mk) :: sumRotorSpeedDWM = 0.0_mk
   ! real(mk), save  :: time = 0.0_mk, time_0 = 0.0_mk  ! TODO: this time should be replaced by the 'time' passed in from HAWC2
   ! integer, save :: localStepNum = 0
   ! Declare some variables for debugging purpose
   character(1) str
   logical :: debugFlag = .false. 

   !***********************************************************************************************
   ! Inputs and their filtering
   !***********************************************************************************************

   ! Mean pitch angle
   PitchMean = (PitchVect(1) + PitchVect(2) + PitchVect(3)) / 3.0_mk
   ! Low-pass filtering of the rotor speed
   y = lowpass2orderfilt(deltat, stepno, omega2ordervar, GenSpeed)
   GenSpeedFilt = y(1)
   dGenSpeed_dtFilt = y(2)
   ! Low pass filtered power
   y = lowpass2orderfilt(deltat, stepno, power2ordervar, Pe)
   PeFilt=y(1)
   ! Low-pass filtering of the mean pitch angle for gain scheduling
   PitchMeanFilt = lowpass1orderfilt(deltat, stepno, pitchfirstordervar, PitchMean)
   PitchMeanFilt = min(PitchMeanFilt, 30.0_mk*degrad)
   ! Low-pass filtering of the nacelle wind speed
   WSPfilt = lowpass1orderfilt(deltat, stepno, wspfirstordervar, wsp)
   ! Minimum pitch angle may vary with filtered wind speed
   PitchMin = GetOptiPitch(WSPfilt)
   !***********************************************************************************************
   ! Limit reference speed for storm control
   !***********************************************************************************************
   if (Vcutout .gt. Vstorm) then
      GenSpeedRef_full = GenSpeedRefMax - max(0.0_mk, &
                         (WSPfilt - Vstorm)/(Vcutout - Vstorm)*(GenSpeedRefMax - GenSpeedRefMin))
   else
      GenSpeedRef_full = GenSpeedRefMax
   endif

   !***********************************************************************************************
   ! Select Case based on derate strategy
   !***********************************************************************************************
   select case (Deratevar%strat)

   case(1)  ! constant rotation
   
     GenSpeedDerate = ((Deratevar%dr*PeRated)/Kopt)**(1.0_mk/3.0_mk) ! Derated Rotor Speed: Remark: this is not completely right
     GenSpeedRefMax = min(GenSpeedRefMax,GenSpeedDerate)
     GenTorqueRated = (Deratevar%dr*PeRated)/GenSpeedRefMax

   case(2)  ! maximum rotation
     GenTorqueRated  =  (Deratevar%dr*PeRated)/GenSpeedRefMax  
   case(3)  ! minimun CT 
       ! Calculate the tip-speed-ratio and pitch angle through the dCp-Lambda and dCp-beta relation
       ! table
       if(firstStep) then
           do i = 1,(downRegulationData%NumLines - 1)
               if (Deratevar%dr<=downRegulationData%dCp(i) .and. Deratevar%dr >= downRegulationData%dCp(i+1)) then
                   lambdaAtMinCt = interpolate(Deratevar%dr, downRegulationData%dCp(i),downRegulationData%dCp(i+1), downRegulationData%Lambda(i), downRegulationData%Lambda(i+1))
                   pitchAtMinCt = interpolate(Deratevar%dr, downRegulationData%dCp(i),downRegulationData%dCp(i+1), downRegulationData%Pitch(i), downRegulationData%Pitch(i+1))
                   pitchAtMinCt = pitchAtMinCt*degrad  ! convert to rads
                   minCt = interpolate(Deratevar%dr, downRegulationData%dCp(i),downRegulationData%dCp(i+1), downRegulationData%Ct(i,1), downRegulationData%Ct(i+1,1))
               endif
           enddo
           ! compute generator constant for minimun Ct tracking
           KoptDerating = 0.5_mk*rho*pi*R**5*(1.0_mk/lambdaAtMinCt)**3*downRegulationData%Cp(1,1)*Deratevar%dr

           ! save the generator constant for normal optimal Cp tracking
           Kopt_normal = Kopt
           
           ! Overwrite Kopt using the value calculated by Cp associated with the minimum ct
           Kopt = KoptDerating

           ! get the Cp at the derating level
           CpAtMinCt = downRegulationData%Cp(1,1)*Deratevar%dr

           ! Keep the rated wind speed during derating
           ! FIXME: It should be a better way to define the limits on Generator speed limits for deratig case
           if(RatedWindSpeed .gt. 0.0) then
               Rated_wsp = RatedWindSpeed
           endif
           GenSpeedOptLimit2 = Rated_wsp * lambdaAtMinCt / R
           GenSpeedMaxDerate = GenSpeedOptLimit2/SwitchVar%rel_sp_open_Qg
           GenSpeedDerate = GenSpeedMaxDerate

           ! This is similar as keep the designed rated wind speed
           !GenTorqueRatio = (2*SwitchVar%rel_sp_open_Qg - 1)**2.0_mk
           ! GenTorqueOptLimit2 = GenTorqueRatio*Kopt*(GenSpeedRefMax)**2.0_mk
           !GenTorqueOptLimit2 = GenTorqueRatio*Kopt_normal*(GenSpeedRefMax)**2.0_mk
           !GenSpeedOptLimit2 = (GenTorqueOptLimit2/Kopt)**(1.0_mk/2.0_mk)
           !GenSpeedMaxDerate = GenSpeedOptLimit2/(2*SwitchVar%rel_sp_open_Qg - 1)
           !GenSpeedDerate = GenSpeedMaxDerate

           GenSpeedRefMax = min(GenSpeedRefMax,GenSpeedDerate)
           GenTorqueRated = (Deratevar%dr*PeRated)/GenSpeedRefMax
           firstStep = .false.
       else
           ! Minimum pitch angle may vary with the de-rating percentage
           PitchMin = max(PitchMin,pitchAtMinCt)
           ! Dump output to array
           dump_array(30) = Kopt                ! Save generator constant when derating
           dump_array(31) = lambdaAtMinCt       ! Save tip-speed-ratio at minimum Ct
           dump_array(32) = GenSpeedDerate      ! Save rated generator speed when derating
           dump_array(33) = pitchAtMinCt        ! Save required pitch angle when operating at minimum Ct strategy
           dump_array(34) = minCt               ! Save the thrust coefficient when derating
           dump_array(35) = CpAtMinCt           ! Save the power coefficient when derating 
           dump_array(36) = GenTorqueRated      ! Save rated generator torque when derating 
           dump_array(37) = GenSpeedOptLimit2   ! Save generator speed upper limit when follows the Kopt*w^2 relation when derating 
           dump_array(38) = Kopt*GenSpeedOptLimit2**2  ! Save generator torque upper limit which follows the Kopt*w^2 relation when derating 

          ! output for debug purpose
           if(debugFlag .eqv. .true.) then
               write(*,*) "Press 'c' to continue, 'q' to Quit the code."
               read(*,*) str
               if(str == 'q') then
                   stop
               elseif(str == 'c') then
                   write(*,*) "simulation continuing...."
               endif
           endif

       endif  ! endif firststep
       
300    format(1X, A10,F13.3,A18,F6.3,A18,F6.3,A10,F6.3)       
   end select

   GenSpeedRef_full = max(min(GenSpeedRef_full, GenSpeedRefMax), GenSpeedRefMin)
   
   !***********************************************************************************************
   ! PID regulation of generator torque
   !***********************************************************************************************
   call torquecontroller(GenSpeed, GenSpeedFilt, dGenSpeed_dtFilt, PitchMean, WSPfilt, PitchMin, &
                         GenSpeedRef_full, Pe, GenTorqueRef, dump_array)
   !***********************************************************************************************
   ! Active DT damping based on filtered rotor speed
   !***********************************************************************************************
   call drivetraindamper(GenSpeed, Qdamp_ref, dump_array)
   if (newtimestep) TimerGenCutin = TimerGenCutin + deltat
   x = switch_spline(TimerGenCutin, CutinVar%delay, 2.0_mk*CutinVar%delay)
   GenTorqueRef = min(max(GenTorqueRef + Qdamp_ref*x, 0.0_mk), GenTorqueMax)
   !***********************************************************************************************
   ! PID regulation of collective pitch angle
   !***********************************************************************************************
   call pitchcontroller(GenSpeedFilt, dGenSpeed_dtFilt, PitchMeanFilt, PeFilt, PitchMin, &
                        GenSpeedRef_full, PitchColRef, dump_array)
   !***********************************************************************************************
   ! Active Tower damping based on filtered tower top aceleration
   !***********************************************************************************************
   P_filt = lowpass1orderfilt(deltat, stepno, TTfa_PWRfirstordervar, GenTorqueRef*GenSpeedFilt)
   call towerdamper(TTfa_acc, theta_dam_ref, dump_array)
   x = switch_spline(P_filt, TTfa_PWR_lower*PeRated, TTfa_PWR_upper*PeRated)
   PitchColRef = min(max(PitchColRef + theta_dam_ref*x, PID_pit_var%outmin), PID_pit_var%outmax)
   ! Write into dump array
   dump_array(1) = GenTorqueRef*GenSpeedFilt    ! changed form GenSpeed to GenSpeedFilt
   dump_array(2) = WSPfilt
   dump_array(3) = GenSpeedFilt
   dump_array(20) = PitchMeanFilt
   return
end subroutine derate_operation
!**************************************************************************************************
subroutine start_up(CtrlStatus, GenSpeed, PitchVect, wsp, GenTorqueRef, PitchColRef, dump_array)
   !
   ! Start-up procedures.
   !
   integer,  intent(inout) :: CtrlStatus ! Integer indicating the status of the controller.&
                                         !  (0: Normal operation, <0: Start-up., >0: Shutdown)
   real(mk), intent(in)    :: GenSpeed     ! Measured generator speed [rad/s].
   real(mk), intent(in)    :: PitchVect(3) ! Measured pitch angles [rad].
   real(mk), intent(in)    :: wsp          ! Measured wind speed [m/s].
   real(mk), intent(out)   :: GenTorqueRef ! Generator torque reference [Nm].
   real(mk), intent(out)   :: PitchColRef  ! Reference collective pitch [rad].
   real(mk), intent(inout) :: dump_array(50) ! Array for output.
   real(mk) WSPfilt, GenSpeedFilt, dGenSpeed_dtFilt, PitchMin
   real(mk) GenSpeedFiltErr, err_pitch(2)
   real(mk) kgain_torque(3), kgain_pitch(3, 2)
   real(mk) dummy, y(2), PitchMean, PitchMeanFilt, GenSpeedRef
   ! Low-pass filtering of the rotor speed
   y = lowpass2orderfilt(deltat, stepno, omega2ordervar, GenSpeed)
   GenSpeedFilt = y(1)
   dGenSpeed_dtFilt = y(2)
   ! Mean pitch angle
   PitchMean = (PitchVect(1) + PitchVect(2) + PitchVect(3)) / 3.0_mk
   PitchMeanFilt = lowpass1orderfilt(deltat, stepno, pitchfirstordervar, PitchMean)
   PitchMeanFilt = min(PitchMeanFilt, 30.0_mk*degrad)
   ! Low-pass filtering of the nacelle wind speed
   WSPfilt = lowpass1orderfilt(deltat, stepno, wspfirstordervar, wsp)
   ! Minimum pitch angle may vary with filtered wind speed
   PitchMin = GetOptiPitch(WSPfilt)
   select case (PartialLoadControlMode)
   ! Speed set-point for K_opt*Omega^2 control of torque
   case (1)
      if (GenSpeedFilt .gt. 0.5_mk*(GenSpeedRefMax + GenSpeedRefMin)) then
         GenSpeedRef = GenSpeedRefMax
      else
         GenSpeedRef = GenSpeedRefMin
       endif
    ! Speed set-point for PID control of torque tracking the optimal TSR
    case (2)
      GenSpeedRef = min(max(WSPfilt*TSR_opt/R,GenSpeedRefMin),GenSpeedRefMax)
   end select
   GenSpeedFiltErr = GenSpeedFilt - GenSpeedRef
   ! Set point for the pitch feedback is 10% above the minimum speed
   err_pitch(1) = GenSpeedFilt - GenSpeedRefMax
   err_pitch(2) = 0.0_mk
   if ((GenSpeed+dGenSpeed_dtFilt*CutinVar%delay.lt.GenSpeedRefMin).and.(.not.generator_cutin)) then
      ! Track AOA with pitch reference
      PitchColRef = min(PitchColRefOld, atan2(WSPfilt, 0.75_mk*R*GenSpeedFilt) - 6.0_mk*degrad)
      ! Remember pitch reference for transition
      PitchColRef0 = PitchColRef
      ! Wind-up integral term of PID2 controller
      PID_pit_var%outmin = PitchColRef
      kgain_pitch = 1.0_mk
      dummy = PID2(stepno, deltat, kgain_pitch, PID_pit_var, err_pitch, AddedPitchRate,0.0_mk) ! VC edit: added argument init=0 in PID2
      ! Generator is still cut-out
      GenTorqueRef = 0.0_mk
      ! Timer generator cut-in
      TimerGenCutin = 0.0_mk
      ! Set the initial generator torque to K
      GenTorqueRef0 = min(PitchColRef*raddeg/10.0_mk*Kopt*GenSpeedFilt**2, GenTorqueRated)
      ! Windup the integral term of PID controller
      PID_gen_var%outmin = GenTorqueRef
      kgain_torque = 1.0_mk
      dummy = PID(stepno, deltat, kgain_torque, PID_gen_var, GenSpeedFiltErr)
   elseif (TimerStartup.lt.CutinVar%delay) then
      ! Start increasing the timer for the delay
      if (newtimestep) TimerStartup = TimerStartup + deltat
      ! Generator is cut-in
      generator_cutin = .true.
      ! Gradually set the minimum pitch angle to optimal pitch
      PID_pit_var%outmin = PitchColRef0 + &
                           (PitchMin - 0.5_mk*PitchColRef0)*TimerStartup/CutinVar%delay
      ! Let pitch PID2 control the speed while the torque is increased
      kgain_pitch = 1.0_mk
      PitchColRef = PID2(stepno, deltat, kgain_pitch, PID_pit_var, err_pitch, AddedPitchRate,0.0_mk) ! VC edit: added argument init=0 in PID2
      ! Linearly increase the torque reference
      GenTorqueRef = min(GenTorqueRef0, GenTorqueRef0*TimerStartup/CutinVar%delay)
      ! Windup the integral term of PID controller
      PID_gen_var%outmin = GenTorqueRef
      kgain_torque = 1.0_mk
      dummy = PID(stepno, deltat, kgain_torque, PID_gen_var, GenSpeedFiltErr)
      ! Remember pitch reference for transition
      PitchColRef0 = PitchColRef
   else
      ! Set pitch reference
      PitchColRef  = PitchColRef0
      GenTorqueRef = GenTorqueRef0
      ! Done with start-up
      CtrlStatus = 0
      TimerStartup = 0.0_mk
   endif
   ! Write into dump array
   dump_array(1) = GenTorqueRef*GenSpeed
   dump_array(2) = WSPfilt
   dump_array(3) = GenSpeedFilt
   dump_array(4) = GenSpeedFiltErr
   dump_array(6) = PID_gen_var%outpro
   dump_array(7) = PID_gen_var%outset
   dump_array(8) = PID_gen_var%outmin
   dump_array(9) = PID_gen_var%outmax
   dump_array(12) = err_pitch(2)
   dump_array(13) = PID_pit_var%outpro
   dump_array(14) = PID_pit_var%outset
   dump_array(15) = PID_pit_var%outmin
   dump_array(16) = PID_pit_var%outmax
   dump_array(18) = CtrlStatus
   dump_array(19) = AddedPitchRate
   dump_array(20) = PitchMeanFilt
   return
end subroutine start_up
!**************************************************************************************************
subroutine shut_down(CtrlStatus, GenSpeed, PitchVect, wsp, GenTorqueRef, PitchColRef, dump_array)
   !
   ! Shut-down procedures.
   !
   integer,  intent(in)    :: CtrlStatus ! Integer indicating the status of the controller. &
                                         !  (0: Normal operation, <0: Start-up., >0: Shutdown)
   real(mk), intent(in)    :: GenSpeed   ! Measured generator speed [rad/s].
   real(mk), intent(in)    :: PitchVect(3) ! Measured pitch angles [rad].
   real(mk), intent(in)    :: wsp          ! Measured wind speed [m/s].
   real(mk), intent(out)   :: GenTorqueRef ! Generator torque reference [Nm].
   real(mk), intent(out)   :: PitchColRef  ! Reference collective pitch [rad].
   real(mk), intent(inout) :: dump_array(50) ! Array for output.
   real(mk) y(2), WSPfilt, PitchMean, PitchMeanFilt
   ! Filtering
   WSPfilt = lowpass1orderfilt(deltat, stepno, wspfirstordervar, wsp)
   y = lowpass2orderfilt(deltat, stepno, omega2ordervar, GenSpeed)
   PitchMean = (PitchVect(1) + PitchVect(2) + PitchVect(3)) / 3.0_mk
   PitchMeanFilt = lowpass1orderfilt(deltat, stepno, pitchfirstordervar, PitchMean)
   PitchMeanFilt = min(PitchMeanFilt, 30.0_mk*degrad) ! Maximum of 30 deg
   ! Start increasing the timer for the delay
   if (newtimestep) TimerShutdown2 = TimerShutdown2 + deltat
   ! Generator settings
   select case(CtrlStatus)
      case(1, 3, 4, 7)
         if (GenSpeed .gt. GenSpeed_at_stop*0.8_mk) then
            GenTorqueRef = GenTorque_at_stop
         else
            if (newtimestep) TimerShutdown = TimerShutdown + deltat
            GenTorqueRef = max(0.0_mk, GenTorque_at_stop*(1.0_mk - TimerShutdown/CutoutVar%torquedelay))
         endif
      case(2, 5, 6)
         GenTorqueRef = 0.0_mk
      case(-2) ! Pitch-out before cut-in
         GenTorqueRef = 0.0_mk
   end select
   ! Pitch seetings
   select case(CtrlStatus)
     case(1, 3, 4, 7) ! Two pitch speed stop
       if (TimerShutdown2 .gt. CutoutVar%pitchdelay + CutoutVar%pitchdelay2) then
         PitchColRef = min(PitchStopAng, PitchColRefOld + deltat*CutoutVar%pitchvelmax2)
       elseif (TimerShutdown2 .gt. CutoutVar%pitchdelay) then
         PitchColRef = min(PitchStopAng, PitchColRefOld + deltat*CutoutVar%pitchvelmax)
       elseif (TimerShutdown2 .gt. 0.0_mk) then
         PitchColRef = PitchColRefOld
       endif
     case(2, 5, 6) ! Pitch out at maximum speed
       PitchColRef = min(PitchStopAng, PitchColRefOld + &
                         deltat*max(CutoutVar%pitchvelmax, CutoutVar%pitchvelmax2))
     case(-2) ! Pitch-out before cut-in
        PitchColRef = min(PitchStopAng, PitchColRefOld + deltat*CutoutVar%pitchvelmax)
     end select
   ! Write into dump array
   dump_array(1) = GenTorqueRef*GenSpeed
   dump_array(3) = y(1)
   dump_array(5) = y(2)
   dump_array(18) = CtrlStatus
   dump_array(20) = PitchMeanFilt
   return
end subroutine shut_down
!**************************************************************************************************
subroutine monitoring(CtrlStatus, GridFlag, GenSpeed, TTAcc, PitchVect, PitchColRefmonitor, dump_array)
   !
   ! Lower level system monitoring. It changes the controller status to:
   ! - (1) if filtered GenSpeed is higher than the overspeed limit.
   ! - (2) if GridFlag is not 0.
   ! - (3) if filtered TTAcc is higher than the safety limit.
   ! - (6) if GenSpeed is negative.
   ! - (7) if deviation of pitch angles from reference is larger than threshold value
   !
   integer, intent(inout) :: CtrlStatus ! Integer indicating the status of the controller.&
                                        !  (0: Normal operation, <0: Start-up., >0: Shutdown)
   integer, intent(inout)  :: GridFlag  ! Integer indicating the status of the grid.
   real(mk), intent(in)    :: TTAcc     ! Tower top acceleration [m/s**2].
   real(mk), intent(in)    :: GenSpeed  ! Measured generator speed [rad/s].
   real(mk), intent(in)    :: PitchVect(3) ! Measured pitch angles [rad].
   real(mk), intent(in)    :: PitchColRefmonitor  ! Reference collective pitch [rad].
   real(mk), intent(inout) :: dump_array(50) ! Array for output.
   real(mk) GenSpeedFilt, dGenSpeed_dtFilt, TTAccFilt
   real(mk) y(2),DiffPitch
   ! Low-pass filtering of the rotor speed
   y = lowpass2orderfilt(deltat, stepno, MoniVar%omega2ordervar, GenSpeed)
   GenSpeedFilt = y(1)
   dGenSpeed_dtFilt = y(2)
   ! Low-pass filtering of the nacelle wind speed
   TTAccFilt = lowpass1orderfilt(deltat, stepno, MoniVar%rystevagtfirstordervar, TTAcc)
   !***********************************************************************************************
   ! Grid monitoring
   !***********************************************************************************************
   if (GridFlag .gt. 0) then
      CtrlStatus = 2
      stoptype = 1
      GenSpeed_at_stop = GenSpeed
      GenTorque_at_stop = GenTorqueRefOld
   endif
   !***********************************************************************************************
   ! Overspeed monitoring
   !***********************************************************************************************
   if ((GenSpeedFilt .gt. MoniVar%overspeed) .and. (CtrlStatus.eq.0)) then
      CtrlStatus = 1
      stoptype = 1
      GenSpeed_at_stop = GenSpeed
      GenTorque_at_stop = GenTorqueRefOld
   endif
   !***********************************************************************************************
   ! Acceleration monitoring
   !***********************************************************************************************
   if ((TTAccFilt .gt. MoniVar%RysteVagtLevel) .and. (CtrlStatus .eq. 0)) then
      CtrlStatus = 3
      stoptype = 1
      GenSpeed_at_stop = GenSpeed
      GenTorque_at_stop = GenTorqueRefOld
   endif
   !***********************************************************************************************
   ! Reverse speed monitoring
   !***********************************************************************************************
   if ((GenSpeedFilt .lt. 0.0_mk) .and. (CtrlStatus .eq. 0)) then
      CtrlStatus = 6
      stoptype = 1
      GenSpeed_at_stop = GenSpeed
      GenTorque_at_stop = GenTorqueRefOld
   endif
   !***********************************************************************************************
   ! Pitch angle deviation 
   ! TODO: 
   !     Check: how does this monitoring function behave in IPC mode?
   !***********************************************************************************************
   NAve_Pitch = floor(TAve_Pitch/deltat)
   if (DeltaPitchThreshold*TAve_Pitch.gt.0.0_mk) then
     if (NAve_Pitch.gt.1000) then
        write(6,'(a)') 'ERROR: Length of time window for pitch angle averaging in the pitch deviation monitor is too long (>1000 time steps)'
        stop
     else
       ! Counter
       CountPitchDeviation=CountPitchDeviation+1
       ! Subtract last value
       AveragedMeanPitchAngles=AveragedMeanPitchAngles-PitchAngles(NAve_Pitch,1:3)/dfloat(NAve_Pitch)
       AveragedPitchReference=AveragedPitchReference-PitchRefs(NAve_Pitch,1:3)/dfloat(NAve_Pitch)
       ! Shift all values
       PitchAngles(2:NAve_Pitch,1:3)=PitchAngles(1:NAve_Pitch-1,1:3)
       PitchRefs(2:NAve_Pitch,1:3)=PitchRefs(1:NAve_Pitch-1,1:3)
       ! Update new values
       PitchAngles(1,1:3)=PitchVect
       PitchRefs(1,1)=PitchColRefmonitor
       PitchRefs(1,2)=PitchColRefmonitor
       PitchRefs(1,3)=PitchColRefmonitor
       ! Add new values
       AveragedMeanPitchAngles=AveragedMeanPitchAngles+PitchAngles(1,1:3)/dfloat(NAve_Pitch)
       AveragedPitchReference=AveragedPitchReference+PitchRefs(1,1:3)/dfloat(NAve_Pitch)
       ! Compute max difference
       DiffPitch=maxval(abs(AveragedMeanPitchAngles-AveragedPitchReference))*raddeg
       ! Output
       dump_array(27) = AveragedPitchReference(1)
       dump_array(28) = AveragedMeanPitchAngles(1)
       ! Check if too big
       if ((CountPitchDeviation.gt.2*NAve_Pitch) .and. (DiffPitch.gt. DeltaPitchThreshold) .and. (CtrlStatus .eq. 0)) then
         CtrlStatus = 7
         stoptype = 1
         GenSpeed_at_stop = GenSpeed
         GenTorque_at_stop = GenTorqueRefOld
       endif
     endif
   endif
   !***********************************************************************************************
   ! Write into dump array
   !***********************************************************************************************
   dump_array(18) = CtrlStatus
   dump_array(23) = TTAccFilt
   return
end subroutine monitoring
!**************************************************************************************************
subroutine torquecontroller(GenSpeed, GenSpeedFilt, dGenSpeed_dtFilt, PitchMean, WSPfilt, &
                            PitchMin, GenSpeedRef_full, Pe, GenTorqueRef, dump_array)
   !
   ! Generator torque controller. Controller that computes the generator torque reference.
   !
   ! subroutine input arguments
   real(mk), intent(in) :: GenSpeed          ! Measured generator speed [rad/s].
   real(mk), intent(in) :: GenSpeedFilt      ! Filtered generator speed [rad/s].
   real(mk), intent(in) :: dGenSpeed_dtFilt  ! Filtered generator acceleration [rad/s**2].
   real(mk), intent(in) :: PitchMean         ! Mean pitch angle [rad].
   real(mk), intent(in) :: PitchMin          ! Minimum pitch angle [rad].
   real(mk), intent(in) :: WSPfilt           ! Filtered wind speed [m/s].
   real(mk), intent(in) :: GenSpeedRef_full  ! Reference generator speed [rad/s].
   real(mk), intent(in) :: Pe                ! Measured electrical power [W].
   real(mk), intent(out) :: GenTorqueRef     ! Generator torque reference [Nm].
   real(mk), intent(inout) :: dump_array(50) ! Array for output.

   ! local variables
   real(mk) GenTorqueMin_full, GenTorqueMax_full, GenTorqueMin_partial, GenTorqueMax_partial
   real(mk) GenSpeed_min1, GenSpeed_min2, GenSpeed_max1, GenSpeed_max2, GenSpeedRef
   real(mk) x, switch, switch_pitang_lower, switch_pitang_upper,ConstantPowerTorque
   real(mk) kgain(3), GenSpeedFiltErr, GenSpeedErr, outmin, outmax, GenSpeedFiltTorque
   !***********************************************************************************************
   ! Set generator/rotor speed ref. changes between max. <-> min. for torque contr. 
   ! and remains at rated for pitch contr.
   !***********************************************************************************************
   if (newtimestep) then
        outmax_old=PID_gen_var%outmax
        outmin_old=PID_gen_var%outmin
   endif
   select case (PartialLoadControlMode)
   case (1)
      if (GenSpeedFilt .gt. 0.5_mk*(GenSpeedRefMax + GenSpeedRefMin)) then
         GenSpeedRef = GenSpeedRefMax
      else
         GenSpeedRef = GenSpeedRefMin
      endif
   case (2) 
      ! This option should be combined with effective wind speed estimator for industry application
      GenSpeedRef = WSPfilt*TSR_opt/R
      GenSpeedRef = min(max(GenSpeedRef, GenSpeedRefMin), GenSpeedRefMax)
   end select
   ! Rotor speed error
   GenSpeedErr = GenSpeed - GenSpeedRef
   GenSpeedFiltErr = GenSpeedFilt - GenSpeedRef
   !-----------------------------------------------------------------------------------------------
   ! Filter generator speed if drivetrain damping is actived
   !-----------------------------------------------------------------------------------------------
   if (DT_mode_filt_torque%f0 .gt. 0.0_mk) then
      GenSpeedFiltTorque=notch2orderfilt(deltat, stepno, DT_mode_filt_torque, GenSpeed)
   else
      ! GenSpeedFiltTorque= GenSpeed
      ! Use the LP filtered generator speed
      GenSpeedFiltTorque= GenSpeedFilt
   endif    
   !-----------------------------------------------------------------------------------------------
   ! Compute generator torque limits in full load region
   !-----------------------------------------------------------------------------------------------
   ConstantPowerTorque=min((GenTorqueRated*GenSpeedRef_full)/max(GenSpeedFiltTorque, GenSpeedRefMin),GenTorqueMax)
   GenTorqueMin_full = GenTorqueRated*(1.0_mk-TorqueCtrlRatio) + ConstantPowerTorque*TorqueCtrlRatio
   GenTorqueMax_full = GenTorqueMin_full
   !-----------------------------------------------------------------------------------------------
   ! Compute limits (torque and speed) in partial load region that opens in both ends
   !-----------------------------------------------------------------------------------------------
   select case (PartialLoadControlMode)
     ! Torque limits for K Omega^2 control of torque
     ! case = 1 means that tracking the optimal Cp in partial load region (region 2)
     case (1)
       ! Calculate the constant limits for opening and closing of torque limits
       GenSpeed_min1 = GenSpeedRefMin
       GenSpeed_min2 = GenSpeedRefMin/SwitchVar%rel_sp_open_Qg
       GenSpeed_max1 = (2.0_mk*SwitchVar%rel_sp_open_Qg - 1.0_mk)*GenSpeedRefMax
       GenSpeed_max2 = SwitchVar%rel_sp_open_Qg*GenSpeedRefMax
       ! Compute lower partial torque limits
       x = switch_spline(GenSpeedFilt, GenSpeed_min1, GenSpeed_min2)
       GenTorqueMin_partial = (Kopt*GenSpeedFilt**2 - Kopt_dot*dGenSpeed_dtFilt)*x
       GenTorqueMin_partial = min(GenTorqueMin_partial, Kopt*GenSpeed_max1**2)
       x = switch_spline(GenSpeedFilt, GenSpeed_max1, GenSpeed_max2)
       ! Compute upper partial torque limits
       GenTorqueMax_partial = (Kopt*GenSpeedFilt**2 - Kopt_dot*dGenSpeed_dtFilt)*(1.0_mk - x) + GenTorqueMax_full*x
       GenTorqueMax_partial = max(GenTorqueMax_partial,Kopt*GenSpeed_min2**2)
     ! Torque limits for PID control of torque
     ! case = 2 means that usign PID to control the torque in the partial load region (region 2)
     case (2)
       GenTorqueMin_partial = 0.0_mk
       GenTorqueMax_partial = GenTorqueMax_full
   end select
   ! Interpolation between partial and full load torque limits based on pitch
   switch_pitang_lower = 0.01_mk + PitchMin
   switch_pitang_upper = SwitchVar%pitang_upper + PitchMin
   if (PitchMean.le.switch_pitang_lower) then
     fullload=.false.
   endif
   if (PitchMean.ge.switch_pitang_upper) then
     fullload=.true.
   endif
   if (.not.fullload) then
     switch = switch_spline(PitchMean, PitchMin, switch_pitang_upper)
   else
     switch=1.0_mk
   endif
   outmin = (1.0_mk - switch)*GenTorqueMin_partial + switch*GenTorqueMin_full
   outmax = (1.0_mk - switch)*GenTorqueMax_partial + switch*GenTorqueMax_full
 
   ! Check derating limits
   ! TODO: check to see if it is needed when using min ct strategy ! VC: this should at least not be used for strategy 3 (following (Power,Speed) operating point in normal operation), it would harm power tracking and the limits are already in GenTorqueMin/Max_partial/full
   if (deratevar%strat > 0 .and. deratevar%strat < 3) then ! VC edit: changed logical operator
     outmin = min(outmin, GenTorqueRated)
     outmax = min(outmax, GenTorqueRated)
   endif
   ! print*, 'torquecontroller: ', fullload, switch, switch_pitang_lower*180/pi, switch_pitang_upper*180/pi, PitchMin*180/pi, PitchMean*180/pi
   ! print*, 'torquecontroller: ', GenTorqueMin_full*GenSpeedFilt, GenTorqueMax_full*GenSpeedFilt, PeRated*Deratevar%dr, switch, GenTorqueMin_partial*GenSpeedFilt, GenTorqueMax_partial*GenSpeedFilt, outmin, outmax, GenTorqueRated

   !***********************************************************************************************
   ! Rotor speed exclusion zone
   !***********************************************************************************************
   call rotorspeedexcl(GenSpeedFilt, Pe/GenSpeed, GenTorqueMin_partial, GenTorqueMax_partial, GenSpeedFiltErr, &
                       outmax, outmin, dump_array)
   !-----------------------------------------------------------------------------------------------
   ! Check the generator torque limits
   !-----------------------------------------------------------------------------------------------
   if ((abs(outmax-outmax_old)/deltat) .gt. PID_gen_var%velmax) then
     outmax = outmax_old + dsign(PID_gen_var%velmax*deltat, outmax-outmax_old)
   endif
   if ((abs(outmin-outmin_old)/deltat) .gt. PID_gen_var%velmax) then
     outmin = outmin_old + dsign(PID_gen_var%velmax*deltat, outmin-outmin_old)
   endif
   PID_gen_var%outmin = outmin
   PID_gen_var%outmax = outmax
   if (PID_gen_var%outmin .gt. PID_gen_var%outmax) PID_gen_var%outmin = PID_gen_var%outmax

   !-----------------------------------------------------------------------------------------------
   ! Compute PID feedback to generator torque demand
   !-----------------------------------------------------------------------------------------------
   kgain = 1.0_mk
   GenTorqueRef = PID(stepno, deltat, kgain, PID_gen_var, GenSpeedFiltErr)
   ! Write into dump array
   dump_array(4) = GenSpeedFiltErr
   dump_array(6) = PID_gen_var%outpro
   dump_array(7) = PID_gen_var%outset
   dump_array(8) = PID_gen_var%outmin
   dump_array(9) = PID_gen_var%outmax
   dump_array(10) = switch
   return
end subroutine torquecontroller
!**************************************************************************************************
subroutine pitchcontroller(GenSpeedFilt, dGenSpeed_dtFilt, PitchMeanFilt, PeFilt, PitchMin, &
                           GenSpeedRef_full, PitchColRef, dump_array)
   !
   ! Pitch controller. Controller that computes the reference collective pitch angle.
   !
   real(mk), intent(in) :: GenSpeedFilt     ! Filtered generator speed [rad/s].
   real(mk), intent(in) :: dGenSpeed_dtFilt ! Filtered generator acceleration [rad/s**2].
   real(mk), intent(in) :: PitchMeanFilt    ! Filtered mean pitch angle [rad].
   real(mk), intent(in) :: PitchMin         ! Minimum pitch angle [rad].
   real(mk), intent(in) :: GenSpeedRef_full ! Reference generator speed [rad/s].
   real(mk), intent(in) :: PeFilt               ! Measured electrical power [W].
   real(mk), intent(out) :: PitchColRef     ! Reference collective pitch angle [rad].
   real(mk), intent(inout) :: dump_array(50) ! Array for output.
   real(mk) GenSpeedFiltErr, added_term, aero_gain, aero_damp, kgain(3, 2), err_pitch(2)
   ! Rotor speed error
   GenSpeedFiltErr = GenSpeedFilt - GenSpeedRef_full
   ! Additional nonlinear pitch control term
   if ((PitNonLin1 .gt. 0.0_mk).and.(Err0 .gt. 0.0_mk).and.(ErrDot0.gt.0.0_mk)) then
     added_term = GenSpeedFiltErr/Err0 + dGenSpeed_dtFilt/ErrDot0
     if (added_term .gt. 1.0_mk) then
       AddedPitchRate = PitNonLin1*added_term + AddedPitchRate
     endif
   endif
   ! Limits
   PID_pit_var%outmin = PitchMin
   PID_pit_var%outmax = PitchStopAng
   ! Aerodynamic gain scheduling dQ/dtheta
   aero_gain = 1.0_mk + PitchGSVar%invkk1*PitchMeanFilt + PitchGSVar%invkk2*PitchMeanFilt**2
   kgain = 1.0_mk/aero_gain
   ! Nonlinear gain to avoid large rotor speed excursion
   if ((rel_limit .ne. 0.0_mk).and.(GenSpeedFiltErr.gt.0.0_mk)) then
     kgain = kgain*(GenSpeedFiltErr**2 / (GenSpeedRef_full*(rel_limit - 1.0_mk))**2 + 1.0_mk)
   endif
   ! Gainscheduling according to dQaero/dOmega
   aero_damp = 1.0_mk + PitchGSVar%invkk1_speed*PitchMeanFilt + &
               PitchGSVar%invkk2_speed*PitchMeanFilt**2
   PID_pit_var%kpro(1) = PID_pit_var%kpro_init(1) + PitchGSVar%kp_speed*aero_damp
   !-----------------------------------------------------------------------------------------------
   ! Compute PID feedback to pitch demand
   !-----------------------------------------------------------------------------------------------
   if (DT_mode_filt%f0 .gt. 0.0_mk) then
     err_pitch(1) = notch2orderfilt(deltat, stepno, DT_mode_filt, GenSpeedFiltErr)
     err_pitch(2) = notch2orderfilt(deltat, stepno, pwr_DT_mode_filt, PeFilt - PeRated*Deratevar%dr)
   else
     err_pitch(1) = GenSpeedFiltErr
     err_pitch(2) = PeFilt - PeRated*Deratevar%dr
   endif
   
   PitchColRef = PID2(stepno, deltat, kgain, PID_pit_var, err_pitch, AddedPitchRate, PitchMeanFilt) ! VC edit: added argument init=PitchMeanFilt in PID2

   ! Write into dump array
   dump_array(11) = GenSpeedFiltErr
   dump_array(12) = err_pitch(2)
   dump_array(13) = PID_pit_var%outpro
   dump_array(14) = PID_pit_var%outset
   dump_array(15) = PID_pit_var%outmin
   dump_array(16) = PID_pit_var%outmax
   dump_array(19) = AddedPitchRate
   return
end subroutine pitchcontroller

subroutine individualpitchcontroller(GenSpeedFilt, dGenSpeed_dtFilt, PitchMeanFilt, PeFilt, PitchMin, &
                           GenSpeedRef_full, PitchColRef, dump_array)
   !
   ! Individual Pitch controller. it computes the reference IPC pitch angle.
   !
   real(mk), intent(in) :: GenSpeedFilt     ! Filtered generator speed [rad/s].
   real(mk), intent(in) :: dGenSpeed_dtFilt ! Filtered generator acceleration [rad/s**2].
   real(mk), intent(in) :: PitchMeanFilt    ! Filtered mean pitch angle [rad].
   real(mk), intent(in) :: PitchMin         ! Minimum pitch angle [rad].
   real(mk), intent(in) :: GenSpeedRef_full ! Reference generator speed [rad/s].
   real(mk), intent(in) :: PeFilt               ! Measured electrical power [W].
   real(mk), intent(out) :: PitchColRef     ! Reference collective pitch angle [rad].
   real(mk), intent(inout) :: dump_array(50) ! Array for output.
   real(mk) GenSpeedFiltErr, added_term, aero_gain, aero_damp, kgain(3, 2), err_pitch(2)
   ! Rotor speed error
   GenSpeedFiltErr = GenSpeedFilt - GenSpeedRef_full
   ! Additional nonlinear pitch control term
   if ((PitNonLin1 .gt. 0.0_mk).and.(Err0 .gt. 0.0_mk).and.(ErrDot0.gt.0.0_mk)) then
     added_term = GenSpeedFiltErr/Err0 + dGenSpeed_dtFilt/ErrDot0
     if (added_term .gt. 1.0_mk) then
       AddedPitchRate = PitNonLin1*added_term + AddedPitchRate
     endif
   endif
   ! Limits
   PID_pit_var%outmin = PitchMin
   PID_pit_var%outmax = PitchStopAng
   ! Aerodynamic gain scheduling dQ/dtheta
   aero_gain = 1.0_mk + PitchGSVar%invkk1*PitchMeanFilt + PitchGSVar%invkk2*PitchMeanFilt**2
   kgain = 1.0_mk/aero_gain
   ! Nonlinear gain to avoid large rotor speed excursion
   if ((rel_limit .ne. 0.0_mk).and.(GenSpeedFiltErr.gt.0.0_mk)) then
     kgain = kgain*(GenSpeedFiltErr**2 / (GenSpeedRef_full*(rel_limit - 1.0_mk))**2 + 1.0_mk)
   endif
   ! Gainscheduling according to dQaero/dOmega
   aero_damp = 1.0_mk + PitchGSVar%invkk1_speed*PitchMeanFilt + &
               PitchGSVar%invkk2_speed*PitchMeanFilt**2
   PID_pit_var%kpro(1) = PID_pit_var%kpro_init(1) + PitchGSVar%kp_speed*aero_damp
   !-----------------------------------------------------------------------------------------------
   ! Compute PID feedback to pitch demand
   !-----------------------------------------------------------------------------------------------
   if (DT_mode_filt%f0 .gt. 0.0_mk) then
     err_pitch(1) = notch2orderfilt(deltat, stepno, DT_mode_filt, GenSpeedFiltErr)
     err_pitch(2) = notch2orderfilt(deltat, stepno, pwr_DT_mode_filt, PeFilt - PeRated*Deratevar%dr)
   else
     err_pitch(1) = GenSpeedFiltErr
     err_pitch(2) = PeFilt - PeRated*Deratevar%dr
   endif
   PitchColRef = PID2(stepno, deltat, kgain, PID_pit_var, err_pitch, AddedPitchRate,0.0_mk) ! VC edit: added argument init=0 in PID2
   ! Write into dump array
   dump_array(11) = GenSpeedFiltErr
   dump_array(12) = err_pitch(2)
   dump_array(13) = PID_pit_var%outpro
   dump_array(14) = PID_pit_var%outset
   dump_array(15) = PID_pit_var%outmin
   dump_array(16) = PID_pit_var%outmax
   dump_array(19) = AddedPitchRate
   return
end subroutine individualpitchcontroller
                           
!**************************************************************************************************
subroutine rotorspeedexcl(GenSpeedFilt, GenTorque, Qg_min_partial, GenTorqueMax_partial, GenSpeedFiltErr, &
                          outmax, outmin, dump_array)
   !
   ! Rotor speed exclusion zone. Subroutine that changes the generator torque limits and the
   ! generator speed error for the generator PID controller to avoid a rotor speed band.
   !
   real(mk), intent(in) :: GenSpeedFilt         ! Filtered measured generator speed [rad/s].
   real(mk), intent(in) :: Qg_min_partial       ! Generator torque lower limit [Nm].
   real(mk), intent(in) :: GenTorqueMax_partial ! Generator torque upper limit [Nm].
   real(mk), intent(in) :: GenTorque          ! Measured generator torque [Nm].
   real(mk), intent(inout) :: outmax          ! Generator torque maximum value [Nm].
   real(mk), intent(inout) :: outmin          ! Generator torque minimum value [Nm].
   real(mk), intent(inout) :: GenSpeedFiltErr ! Filtered generator speed error [rad/s].
   real(mk), intent(inout) :: dump_array(50)  ! Array for output.
   real(mk) y(2), GenSpeedFiltNotch, x1, x2
   real(mk) :: x
   real(mk) Lwr, Lwr_Tg, Hwr, Hwr_Tg, time_excl_delay
   ! Parameters
   Lwr = ExcluZone%Lwr
   Lwr_Tg = ExcluZone%Lwr_Tg
   Hwr = ExcluZone%Hwr
   Hwr_Tg = ExcluZone%Hwr_Tg
   time_excl_delay = ExcluZone%time_excl_delay
   ! Return if the upper speed limit set to zero or negative by the user
   if (Hwr .le. 0.0_mk) return
   ! Band stop filtering of the rotor speed
   x = 0.0_mk
   y = notch2orderfilt(deltat, stepno, ExcluZone%notch, GenSpeedFilt)
   GenSpeedFiltNotch = y(1)
   if (newtimestep) TimerExcl = TimerExcl + deltat
   select case (w_region)
       case (0)
         if ((GenSpeedFilt .gt. Lwr*0.99_mk) .and. (TimerGenCutin .gt. CutinVar%delay))then
             ! rotor reference angular speed
             GenSpeedFiltErr = GenSpeedFiltNotch - Lwr
             excl_flag = 0.0_mk
             w_region = 1
         elseif (GenSpeedFilt .gt. Hwr*(2.0_mk-0.99_mk)) then
             w_region = 3
         else
             w_region = 0
         endif
       case (1)
         if (GenTorque .gt. Lwr_Tg) then
             ! rotor reference angular speed
             TimerExcl = 0.0_mk
             excl_flag = 1.0_mk
             GenSpeedFiltErr = GenSpeedFiltNotch - Lwr
             w_region = 2
         elseif (GenSpeedFilt .lt. Lwr*0.99_mk) then
             w_region = 0
         else
             ! rotor reference angular speed
             if (excl_flag .eq. 0.0_mk) then
               x = 1.0_mk
             else
               x = switch_spline(TimerExcl, 0.0_mk, time_excl_delay)
             endif
             GenSpeedFiltErr = GenSpeedFiltNotch - (Lwr*x + Hwr*(1.0_mk - x))
             w_region = 1
         endif
       case (2)
         if (GenTorque .lt. Hwr_Tg) then
             TimerExcl = 0.0_mk
             excl_flag = 1.0_mk
             ! rotor reference angular speed
             GenSpeedFiltErr = GenSpeedFiltNotch - Hwr
             w_region = 1
         elseif (GenSpeedFilt .gt. Hwr*(2.0_mk-0.95_mk)) then
             w_region = 3
         else
             ! rotor reference angular speed
             if (excl_flag .eq. 0.0_mk) then
               x = 1.0_mk
             else
               x = switch_spline(TimerExcl, 0.0_mk, time_excl_delay)
             endif
             GenSpeedFiltErr = GenSpeedFiltNotch - (Hwr*x + Lwr*(1.0_mk - x))
             w_region = 2
         endif
       case default
         if (GenSpeedFilt .gt. Hwr*(2.0_mk - 0.99_mk)) then
             w_region = 3
         elseif (GenSpeedFilt .lt. Lwr*0.99_mk) then
             w_region = 0
         else
             ! rotor reference angular speed
             excl_flag = 0.0_mk
             GenSpeedFiltErr = GenSpeedFiltNotch - Hwr
             w_region = 2
         endif
   end select
   if ((w_region .eq. 1) .or. (w_region .eq. 2)) then
       x1 = switch_spline(GenSpeedFilt, Lwr*0.99_mk, Lwr)
       x2 = switch_spline(GenSpeedFilt, Hwr, Hwr*(2.0_mk-0.99_mk))
       !min/max generator torque @ exclution zone
       outmax  = GenTorqueMax_partial*(1.0_mk-x1 +x2) + Lwr_Tg * 1.05_mk*(x1-x1*x2)
       outmin  = Qg_min_partial*(1.0_mk-x1 +x2) + Hwr_Tg * 0.95_mk*(x1-x1*x2)
   endif
   dump_array(24) = w_region
   ! call open_log('test.log', 123)
   ! call log_info('Generator Torque:',GenTorque)
   return
end subroutine rotorspeedexcl
!**************************************************************************************************
subroutine drivetraindamper(GenSpeed, Qdamp_ref, dump_array)
   !
   ! Drivetrain damper.
   !
   real(mk), intent(in)  :: GenSpeed  ! Measured generator speed [rad/s].
   real(mk), intent(out) :: Qdamp_ref ! Generator torque reference component from the drivetrain &
                                      ! damper [Nm].
   real(mk), intent(inout) :: dump_array(50) ! Array for output.
   real(mk) omega_dtfilt
   if ((DT_damper%gain .ne. 0.0_mk) .and. (DT_damper%bandpass%f0 .gt. 0.0_mk)) then
      call damper(stepno, deltat, GenSpeed, DT_damper, Qdamp_ref, omega_dtfilt)
   else
      Qdamp_ref = 0.0_mk
   endif
   dump_array(17) = Qdamp_ref
   return
end subroutine drivetraindamper
!**************************************************************************************************
subroutine towerdamper(TTfa_acc, theta_dam_ref, dump_array)
   !
   ! Longitudinal tower damper.
   !
   real(mk), intent(in) :: TTfa_acc ! Measured tower top longitudinal acceleration [m/s**2].
   real(mk), intent(out) :: theta_dam_ref ! Reference pitch angle component from longitudinal tower&
                                          ! damper [rad].
   real(mk), intent(inout) :: dump_array(50) ! Array for output.
   real(mk) TTfa_acc_filt
   if ((TTfa_damper%gain .ne. 0.0_mk) .and. (TTfa_damper%bandpass%f0 .gt. 0.0_mk)) then
      call damper_twr(stepno, deltat, TTfa_acc, TTfa_damper, theta_dam_ref, TTfa_acc_filt)
      ! call log_info('pitch_twr_damper-ref: ',theta_dam_ref)
   else
      TTfa_acc_filt = 0.0_mk
      theta_dam_ref = 0.0_mk
   endif
   dump_array(25) = TTfa_acc_filt
   dump_array(26) = theta_dam_ref
   return
end subroutine towerdamper
!**************************************************************************************************
subroutine windEstimator(GenTorqueRef, GenSpeed, PitchMean, WindEstvar, Cptable, deltat,estAeroTorq,estLambda,estREWS,WSPfilt )
    !
    ! Wind Estimator based on "Estimation of effective wind speed" by Ostergaard et al. (2007)
    !
    real(mk), intent(in) :: GenTorqueRef, GenSpeed, PitchMean, deltat
    type(TWindEstvar), intent(inout) :: WindEstvar  
    type(TCpData), intent(in) :: Cptable 
    real(mk), intent(inout) :: estAeroTorq, estLambda, estREWS
    real(mk), intent(in) :: WSPfilt ! VC edit
    estAeroTorq = AeroTorqEstimator(GenTorqueRef, GenSpeed, WindEstvar, deltat) ! [Nm]
  !  PitchIndex = findloc(PitchMean, CpData%
  !  dump_array(50) = FLOAT(INT(1.26_mk*10.0_mk+0.5_mk))/10.0_mk
    estLambda= GradDesc(estAeroTorq, GenSpeed,PitchMean*raddeg,WindEstvar,CpData,WSPfilt)
    estREWS= GenSpeed*WindEstvar%radius/estLambda
    return
end subroutine windEstimator
!**************************************************************************************************
end module turbine_controller_mod
