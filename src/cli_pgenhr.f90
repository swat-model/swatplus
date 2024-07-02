      subroutine cli_pgenhr
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine distributes daily rainfall exponentially within the day

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hru_km(:)    |km^2          |area of HRU in square kilometers
!!    idg(:)       |none          |array location of random number seed
!!                                |used for a given process
!!    jj           |none          |HRU number
!!    rndseed(:,:) |none          |random number generator seed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    wst(:)%weat%ts(:)   |mm H2O        |rainfall during time step
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j            |none          |HRU number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Log

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use climate_module
      use time_module
      use hydrograph_module
      
      implicit none

      integer :: itime                      !none          |time step during day
      integer :: pt                         !min           |time during day
      integer :: k                          !none          |random number seed, counter
      real :: vv                            !none          |random number between 0.0 and 1.0 that 
                                            !              |represents time to peak rainfall rate
                                            !              |expressed as a fraction of storm duration
      real ::  blm                          !none          |lowest random number value allowed
      real :: qmn                           !none          |mean random number value
      real :: uplm                          !none          |highest random number value
      real :: dur                           !hours         |duration of storm during day
      real :: pkrain                        !mm H2O        |volume of rain at time of peak rainfall
      real :: rtp                           !min           |time of peak rainfall rate
      real :: xk1                           !none          |1st constant in dimensionless exponential
                                            !              |rainfall distribution
      real :: xk2                           !none          |2nd constant in dimensionless exponential
                                            !              |rainfall distribution
      real :: xkp1                          !hr            |1st constant in exponential rainfall
                                            !              |distribution
      real :: xkp2                          !hr            |2nd constant in exponential rainfall
                                            !              |distribution
      real :: rx                            !mm H2O        |total rainfall at end of time step
      real :: pkrr                          !mm/hr         |peak rainfall rate
      real :: sumrain                       !mm H2O        |total amount of daily rainfall prior to
                                            !              |time step
      real :: atri                          !none          |daily value generated for distribution                     

      !! zero subdaily precip array
      wst(iwst)%weat%ts = 0.
      
      if (wst(iwst)%weat%precip < 1.e-6) return

      !! need peak rainfall rate 
      !! calculate peak rate using same method used for peak runoff
      pkrr = 2. * wst(iwst)%weat%precip * wst(iwst)%weat%precip_half_hr

      !! generate random number between 0.0 and 1.0
      !! because all input set to constant value, vv always the same
      !! vv => time to peak expressed as fraction of total storm duration
      blm = 0.05
      qmn = 0.25
      uplm = 0.95
      k = 8
      vv = Atri(blm, qmn, uplm, k)
      
      !! calculate storm duration
      xk1 = vv / 4.605
      xk2 = (1.- vv) / 4.605
      dur = wst(iwst)%weat%precip / (pkrr * (xk1 + xk2))
      if (dur > 24.0) then
        dur = 24.0
        pkrr = wst(iwst)%weat%precip / (dur * (xk1 + xk2))
      end if

      !! calculate amount of total rainfall fallen at time of peak
      !! rainfall and time of peak rainfall in units of minutes
      pkrain = vv * wst(iwst)%weat%precip
      rtp = vv * dur * 60

      !! calculate constants for exponential rainfall distribution
      !! equation
      xkp1 = dur * xk1 
      xkp2 = dur * xk2

      pt = time%dtm
      itime = 1
      sumrain = 0.

      !! do before time of peak rainfall
      !! do while pt less than rtp
      do
        if (pt >= Int(rtp)) exit
        rx = pkrain - pkrr * xkp1 * (1. - Exp((Real(pt) - rtp) / (60. * xkp1)))
        wst(iwst)%weat%ts_next(itime) = rx - sumrain
        pt = pt + time%dtm
        itime = itime + 1
        if (itime > time%step) exit
        sumrain = rx
      end do

      !! after peak rainfall and before end of storm
      do
        if (pt >= Int(dur * 60.)) exit
        rx = pkrain + pkrr * xkp2 * (1. - Exp((rtp - Real(pt)) / (60. * xkp2)))
        wst(iwst)%weat%ts_next(itime) = rx - sumrain
        pt = pt + time%dtm
        itime = itime + 1
        if (itime > time%step) exit
        sumrain = rx
      end do

      !! at end of storm
      if (wst(iwst)%weat%precip > sumrain .and. itime <= time%step) then
        wst(iwst)%weat%ts_next(itime-1) = wst(iwst)%weat%ts_next(itime-1) + (wst(iwst)%weat%precip - sumrain)
      end if

      return
      end subroutine cli_pgenhr