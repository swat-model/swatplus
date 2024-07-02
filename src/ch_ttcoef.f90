      subroutine ch_ttcoef(k)
          
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine computes travel time coefficients for routing
!!    along the main channel

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    ch_d(:)     |m             |average depth of main channel
!!    ch_n(2,:)   |none          |Manning"s "n" value for the main channel
!!    ch_s(2,:)   |m/m           |average slope of main channel
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    chsslope(:)   |none          |change in horizontal distance per unit
!!                               |change in vertical distance on channel side
!!                               |slopes; always set to 2 (slope=1/2)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    aa          |none          |area/area=1 (used to calculate velocity with
!!                               |Manning"s equation)
!!    a           |m^2           |cross-sectional area of channel
!!    b           |m             |bottom width of channel
!!    d           |m             |depth of flow 
!!    fps         |none          |change in horizontal distance per unit
!!                               |change in vertical distance on floodplain side
!!                               |slopes; always set to 4 (slope=1/4)
!!    k           |none          |dummy argument (HRU number)
!!    p           |m             |wetting perimeter
!!    qq1         |m^3/s         |flow rate for a specified depth
!!    rh          |m             |hydraulic radius of channel
!!    tt1         |km s/m        |time coefficient for specified depth
!!    tt2         |km s/m        |time coefficient for bankfull depth
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Sqrt
!!    SWAT: Qman

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
      
      use channel_data_module
      use channel_module
      use channel_velocity_module
      
      implicit none

      integer, intent (in) :: k  !none          |dummy argument (HRU number)
      real :: fps                !none          |change in horizontal distance per unit
                                 !              |change in vertical distance on floodplain side
                                 !              |slopes; always set to 4 (slope=1/4)
      real :: d                  !m             |depth of flow
      real :: b                  !m             |bottom width of channel
      real :: p                  !m             |wetting perimeter 
      real :: a                  !m^2           |cross-sectional area of channel
      real :: qq1                !m^3/s         |flow rate for a specified depth
      real :: rh                 !m             |hydraulic radius of channel
      real :: tt1                !km s/m        |time coefficient for specified depth
      real :: tt2                !km s/m        |time coefficient for bankfull depth
      real :: aa                 !none          |area/area=1 (used to calculate velocity with
                                 !              |Manning"s equation)
      real :: chsslope           !none          |change in horizontal distance per unit
                                 !              |change in vertical distance on channel side
                                 !              |slopes; always set to 2 (slope=1/2)
      real :: qman               !m^3/s or m/s  |flow rate or flow velocity


      aa = 1.
      b = 0.
      d = 0.
!!    If side slope is not set in .rte file then assume this default
!!    If it is main reach default side slope to 2:1 if it is a waterway default to 8:1
      if (ch_hyd(k)%side <= 1.e-6) then
         chsslope = 2.
      else
         chsslope = ch_hyd(k)%side
      end if

      fps = 4.
      d = ch_hyd(k)%d
      b = ch_hyd(k)%w - 2. * d * chsslope


!!    check if bottom width (b) is < 0
      if (b <= 0.) then
        b = 0.
        chsslope = 0.
        b = .5 * ch_hyd(k)%w
        b = Max(0., b)
        chsslope = (ch_hyd(k)%w - b) / (2. * d)
      end if
      ch_vel(k)%wid_btm = b
      ch_vel(k)%dep_bf = d

!!    compute flow and travel time at bankfull depth
      p = 0.
      a = 0.
      rh = 0.
      tt2 = 0.
      p = b + 2. * d * Sqrt(chsslope * chsslope + 1.)
      a = b * d + chsslope * d * d
      rh = a / p
      ch_vel(k)%area = a
      ch_vel(k)%vel_bf = Qman(a, rh, ch_hyd(k)%n, ch_hyd(k)%s)
      ch_vel(k)%velav_bf = Qman(aa, rh, ch_hyd(k)%n, ch_hyd(k)%s)
      ch_vel(k)%celerity_bf = ch_vel(k)%velav_bf * 5. / 3.
      ch_vel(k)%st_dis = ch_hyd(k)%l / ch_vel(k)%celerity_bf / 3.6
      tt2 = ch_hyd(k)%l * a / ch_vel(k)%vel_bf

!!    compute flow and travel time at 1.2 bankfull depth
      d = 0.
      rh = 0.
      qq1 = 0.
      tt1 = 0.
      d = 1.2 * ch_hyd(k)%d
      a = a + (ch_hyd(k)%w * ch_hyd(k)%d + fps * (d - ch_hyd(k)%d) ** 2)
      p = p + 4. * ch_hyd(k)%w + (0.4 * ch_hyd(k)%d * Sqrt(fps * fps + 1.))
      rh = a / p
      qq1 = Qman(a, rh, ch_hyd(k)%n, ch_hyd(k)%s)
      tt1 = ch_hyd(k)%l * a / qq1

!!    compute flow and travel time at 0.1 bankfull depth
      a = 0.
      d = 0.
      p = 0.
      rh = 0.
      qq1 = 0.
      tt1 = 0.
      d = 0.1 * ch_hyd(k)%d
      p = b + 2. * d * Sqrt(chsslope * chsslope + 1.)
      a = b * d + chsslope * d * d
      rh = a / p
      qq1 = Qman(a, rh, ch_hyd(k)%n, ch_hyd(k)%s)
      tt1 = ch_hyd(k)%l * a / qq1
      ch_vel(k)%vel_1bf = Qman(aa, rh, ch_hyd(k)%n, ch_hyd(k)%s)
      ch_vel(k)%celerity_1bf = ch_vel(k)%vel_1bf * 5. / 3.
      ch_vel(k)%stor_dis_1bf = ch_hyd(k)%l / ch_vel(k)%celerity_1bf / 3.6

      return
      end subroutine ch_ttcoef