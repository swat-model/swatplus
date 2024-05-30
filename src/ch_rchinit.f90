      subroutine ch_rchinit

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine initializes variables for the daily simulation of the
!!    channel routing command loop

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ihout       |none          |outflow hydrograph storage location number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bury        |mg pst        |loss of pesticide from active sediment layer
!!                               |by burial
!!    difus       |mg pst        |diffusion of pesticide from sediment to reach
!!    hdepth(:)   |m             |depth of flow during hour
!!    hhstor(:)   |m^3 H2O       |water stored in reach at end of hour
!!    hhtime(:)   |hr            |travel time of flow in reach for hour
!!    hrchwtr(:)  |m^3 H2O       |water stored in reach at beginning of hour
!!    hrtwtr(:)   |m^3           |water leaving reach in hour
!!    hsdti(:)    |m^3/s         |flow rate in reach for hour
!!    rcharea     |m^2           |cross-sectional area of flow
!!    rchdep      |m             |depth of flow on day
!!    rchwtr      |m^3 H2O       |water stored in reach at beginning of day
!!    reactw      |mg pst        |amount of pesticide in reach that is lost
!!                               |through reactions
!!    reactb      |mg pst        |amount of pesticide in sediment that is lost
!!                               |through reactions
!!                               |up by plant roots in the bank storage zone
!!    resuspst    |mg pst        |amount of pesticide moving from sediment to
!!                               |reach due to resuspension
!!    rtevp       |m^3 H2O       |evaporation from reach on day
!!    rttime      |hr            |reach travel time
!!    rttlc       |m^3 H2O       |transmission losses from reach on day
!!    rtwtr       |m^3 H2O       |water leaving reach on day
!!    sdti        |m^3/s         |flow rate in reach for day
!!    sedrch      |metric tons   |sediment transported out of reach on day
!!    setlpst     |mg pst        |amount of pesticide moving from water to
!!                               |sediment due to settling
!!    volatpst    |mg pst        |amount of pesticide lost from reach by
!!                               |volatilization
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ii          |none          |counter
!!    kk          |none          |counter
!!    jrch        |none          |reach number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use channel_module
      use hydrograph_module
      
      implicit none
      
      real :: rchwtr     !m^3 H2O       |water stored in reach at beginning of day
      real :: bury       !mg pst        |loss of pesticide from active sediment layer
                         !              |by burial
      real :: difus      !mg pst        |diffusion of pesticide from sediment to reach
      real :: reactb     !mg pst        |amount of pesticide in sediment that is lost
                         !              |through reactions
                         !              |up by plant roots in the bank storage zone
      real :: reactw     !mg pst        |amount of pesticide in reach that is lost
                         !              |through reactions
      real :: resuspst   !mg pst        |amount of pesticide moving from sediment to
                         !              |reach due to resuspension
      real :: setlpst    !mg pst        |amount of pesticide moving from water to
                         !              |sediment due to settling
      real :: volatpst   !mg pst        |amount of pesticide lost from reach by
                         !                volatilization

!! initialize daily variables
      rchwtr = ch(jrch)%rchstor
      
      bury = 0.
      difus = 0.
      peakr = 0.
      rcharea = 0.
      rchdep = 0.
      reactb = 0.
      reactw = 0.
      resuspst = 0.
      rtevp = 0.
      rttime = 0.
      rttlc = 0.
      rtwtr = 0.
      sdti = 0.
      sedrch = 0.
      setlpst = 0.
      volatpst = 0.
      ch(jrch)%vel_chan = 0.
      sedrch = 0.
      rch_san = 0.
      rch_sil = 0.
      rch_cla = 0.
      rch_sag = 0.
      rch_lag = 0.
      rch_gra = 0.

      return
      end subroutine ch_rchinit