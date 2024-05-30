      subroutine ttcoef_wway(k)     
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine computes travel time coefficients for routing
!!    along the main channel - grassed waterways

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    grwat_w(:)  |m             |average width of main channel
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Sqrt
!!    SWAT: Qman

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
      use hru_module, only : hru
      use channel_velocity_module
      
      implicit none
       
      integer :: k              !none          |dummy argument (HRU number)
      real :: fps               !none          |change in horizontal distance per unit
                                !              |change in vertical distance on floodplain side
                                !              |slopes; always set to 4 (slope=1/4)
      real :: b                 !m             |bottom width of channel
      real :: d                 !m             |depth of flow 
      real :: p                 !m             |wetting perimeter
      real :: a                 !m^2           |cross-sectional area of channel
      real :: qq1               !m^3/s         |flow rate for a specified depth
      real :: rh                !m             |hydraulic radius of channel
      real :: tt1               !km s/m        |time coefficient for specified depth
      real :: tt2               !km s/m        |time coefficient for bankfull depth
      real :: aa                !none          |area/area=1 (used to calculate velocity with
                                !              |Manning"s equation)
      real :: chsslope          !none          |change in horizontal distance per unit
                                !              |change in vertical distance on channel side
                                !              |slopes; always set to 2 (slope=1/2) 
      real :: qman              !m^3/s or m/s  |flow rate or flow velocity
      
      aa = 1.
      b = 0.
      d = 0.
      
!!    If side slope is not set in .rte file then assume this default
!!    If it is main reach default side slope to 2:1 if it is a waterway default to 8:1
!      if (ch_hyd(k)%side <= 1.e-6) then
         chsslope = 8.
!      else
!         chsslope = ch_hyd(k)%side
!      end if

      fps = 4.
      d = hru(k)%lumv%grwat_d
      b = hru(k)%lumv%grwat_w - 2. * d * chsslope


!!    check if bottom width (b) is < 0
      if (b <= 0.) then
        b = 0.
        chsslope = 0.
        b = .5 * hru(k)%lumv%grwat_w
        chsslope = (hru(k)%lumv%grwat_w - b) / (2. * d)
      end if
      grwway_vel(k)%wid_btm = b
      grwway_vel(k)%dep_bf = d

!!    compute flow and travel time at bankfull depth
      p = 0.
      a = 0.
      rh = 0.
      tt2 = 0.
      p = b + 2. * d * Sqrt(chsslope * chsslope + 1.)
      a = b * d + chsslope * d * d
      rh = a / p
      grwway_vel(k)%area = a
      grwway_vel(k)%vel_bf = Qman(a, rh, hru(k)%lumv%grwat_n, hru(k)%lumv%grwat_s)
      grwway_vel(k)%velav_bf = Qman(aa, rh, hru(k)%lumv%grwat_n, hru(k)%lumv%grwat_s)
      grwway_vel(k)%celerity_bf = grwway_vel(k)%velav_bf * 5. / 3.
      grwway_vel(k)%st_dis = hru(k)%lumv%grwat_l / grwway_vel(k)%celerity_bf / 3.6
      tt2 = hru(k)%lumv%grwat_l * a / grwway_vel(k)%vel_bf

!!    compute flow and travel time at 1.2 bankfull depth
      d = 0.
      rh = 0.
      qq1 = 0.
      tt1 = 0.
      d = 1.2 * hru(k)%lumv%grwat_d
      a = a + (hru(k)%lumv%grwat_w * hru(k)%lumv%grwat_d + fps * (d - hru(k)%lumv%grwat_d) ** 2)
      p=p + 4.*hru(k)%lumv%grwat_w + (0.4 * hru(k)%lumv%grwat_d * Sqrt(fps * fps + 1.))
      rh = a / p
      qq1 = Qman(a, rh, hru(k)%lumv%grwat_n, hru(k)%lumv%grwat_s)
      tt1 = hru(k)%lumv%grwat_l * a / qq1

!!    compute flow and travel time at 0.1 bankfull depth
      a = 0.
      d = 0.
      p = 0.
      rh = 0.
      qq1 = 0.
      tt1 = 0.
      d = 0.1 * hru(k)%lumv%grwat_d
      p = b + 2. * d * Sqrt(chsslope * chsslope + 1.)
      a = b * d + chsslope * d * d
      rh = a / p
      qq1 = Qman(a, rh, hru(k)%lumv%grwat_n, hru(k)%lumv%grwat_s)
      tt1 = hru(k)%lumv%grwat_l * a / qq1
      grwway_vel(k)%vel_1bf = Qman(aa, rh, hru(k)%lumv%grwat_n, hru(k)%lumv%grwat_s)
      grwway_vel(k)%celerity_1bf = grwway_vel(k)%vel_1bf * 5. / 3.
      grwway_vel(k)%stor_dis_1bf = hru(k)%lumv%grwat_l / grwway_vel(k)%celerity_1bf / 3.6

      return
      end