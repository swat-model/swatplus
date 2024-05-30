      function regres(k)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this function calculates constituent loadings to the main channel using
!!    USGS regression equations

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hru_km(:)    |km^2          |area of HRU in square kilometers
!!    ihru         |none          |HRU number
!!    iregion(:)      |none          |precipitation category:
!!                                |  1 precipitation <= 508 mm/yr
!!                                |  2 precipitation > 508 and <= 1016 mm/yr
!!                                |  3 precipitation > 1016 mm/yr
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use hru_module, only : hru, ihru, iwgen 
      use climate_module
      use urban_data_module
      
      implicit none
            
      integer, intent (in) :: k                                            !none          |identification code for regression data
                                                                           !              |  1 carbonaceous oxygen demand
                                                                           !              |  2 suspended solid load
                                                                           !              |  3 total nitrogen
                                                                           !              |  4 total phosphorus
      real, dimension (5,3) :: beta                                        !              |
      real :: regres                                                       !kg            |amount of constituent removed in surface
                                                                           !              |runoff
      integer :: ulu                                                       !              |
      integer :: j                                                         !none          |HRU number
      integer :: ii                                                        !none          |precipitation category
      real, dimension(5,3) :: bcod =                                   &   !none          |regression coefficients for calculating
                                                                           !              |carbonaceous oxygen demand of urban runoff                                 
           reshape ((/407.0, 0.626, 0.710, 0.379, 1.518,               &
                      151.0, 0.823, 0.726, 0.564, 1.451,               &
                      102.0, 0.851, 0.601, 0.528, 1.978/), (/5,3/))
      real, dimension(5,3) :: bsus =                                   & 
           reshape ((/1778.0, 0.867, 0.728, 0.157, 2.367,              &
                       812.0, 1.236, 0.436, 0.202, 1.938,              &
                        97.7, 1.002, 1.009, 0.837, 2.818/), (/5,3/))
      real, dimension(5,3) :: btn =                                    &
           reshape ((/20.2, 0.825, 1.070, 0.479, 1.258,                &
                      4.04, 0.936, 0.937, 0.692, 1.373,                &
                      1.66, 0.703, 0.465, 0.521, 1.845/), (/5,3/))
      real, dimension(5,3) :: btp =                                    &
           reshape ((/1.725, 0.884, 0.826, 0.467, 2.130,               &
                      0.697, 1.008, 0.628, 0.469, 1.790,               &
                      1.618, 0.954, 0.789, 0.289, 2.247/), (/5,3/))

      j = ihru

      ii = wgn_pms(iwgen)%ireg
      ulu = hru(j)%luse%urb_lu


      beta = 0.
      if (k==1) beta = bcod
      if (k==2) beta = bsus
      if (k==3) beta = btn
      if (k==4) beta = btp

      regres = beta(1,ii) * (w%precip / 25.4) ** beta(2,ii) *           &
              (hru(j)%km * urbdb(ulu)%fimp / 2.589) **beta(3,ii)*       &
              (urbdb(ulu)%fimp * 100. + 1.) ** beta(4,ii) * beta(5,ii)


      regres = regres / 2.205      !! convert from lbs to kg

      return
      end