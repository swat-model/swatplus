      subroutine sd_rating_curve (i)
      
      use sd_channel_module
      use channel_velocity_module
      use maximum_data_module
      !use hydrograph_module
      
      implicit none      

      integer, intent (in) :: i     !none           |counter  
      integer :: i_dep              !none           |counter
      integer :: ifp_dep            !none           |counter
      
      real :: a                     !m^2            |cross-sectional area of channel
      real :: b                     !m              |bottom width of channel
      real :: p                     !m              |wetting perimeter
      real :: rh                    !m              |hydraulic radius
      real :: qman                  !m^3/s or m/s   |flow rate or flow velocity
      real :: dep                   !               |
      real :: a_bf                  !               |
      real :: p_bf                  !               |
      real :: vol_bf
      real :: vel                   !               |
      real :: frac_abov

      b = sd_ch(i)%chw - 2. * sd_ch(i)%chd * sd_ch(i)%chss
      !! check if bottom width (b) is < 0
      if (b <= 0.) then
        b = .5 * sd_ch(i)%chw
        b = Max(0., b)
        sd_ch(i)%chss = (sd_ch(i)%chw - b) / (2. * sd_ch(i)%chd)
      end if
      
      !! compute rating curve at 0.1 and 1.0 times bankfull depth
      do i_dep = 1, 2
        if (i_dep == 1) dep = 0.1 * sd_ch(i)%chd
        if (i_dep == 2) dep = sd_ch(i)%chd
        !! c^2=a^2+b^2 - a=dep; a/b=slope; b^2=a^2/slope^2
        p = b + 2. * Sqrt(dep ** 2 * (1. + 1. / (sd_ch(i)%chss ** 2)))
        a = b * dep + dep / sd_ch(i)%chss
        rh = a / p
        ch_rcurv(i)%elev(i_dep)%dep = dep
        ch_rcurv(i)%elev(i_dep)%wet_perim = p
        ch_rcurv(i)%elev(i_dep)%xsec_area = a
        
        ch_rcurv(i)%elev(i_dep)%top_wid = b + 2. * dep * sd_ch(i)%chss
        ch_rcurv(i)%elev(i_dep)%surf_area = ch_rcurv(i)%elev(i_dep)%top_wid * sd_ch(i)%chl
        ch_rcurv(i)%elev(i_dep)%vol = a * sd_ch(i)%chl * 1000.
        ch_rcurv(i)%elev(i_dep)%vol_ch = ch_rcurv(i)%elev(i_dep)%vol
        ch_rcurv(i)%elev(i_dep)%vol_fp = 0.
        
        ch_rcurv(i)%elev(i_dep)%flo_rate = Qman(a, rh, sd_ch(i)%chn, sd_ch(i)%chs)
        vel = Qman(1., rh, sd_ch(i)%chn, sd_ch(i)%chs)
        ch_rcurv(i)%elev(i_dep)%ttime = sd_ch(i)%chl / (3.6 * vel)
        
        !! save bankfull depth and area for flood plain calculations
        if (i_dep == 2) then
          p_bf = p
          a_bf = a
          vol_bf = ch_rcurv(i)%elev(i_dep)%vol_ch
        end if
      end do
        
      !! compute rating curve at 1.2 and 2.0 times bankfull depth (flood plain)
      do i_dep = 1, 2
        !! dep = depth above bankfull
        if (i_dep == 1) frac_abov = 0.2
        if (i_dep == 2) frac_abov = 1.
        dep = frac_abov * sd_ch(i)%chd
        !! flood plain perimeter - p^2 = dep^2 + width^2
        p = p_bf + 2. * Sqrt(dep ** 2 * (1. + 1. / (sd_ch(i)%fps ** 2)))
        !! flood plain cross section area - dep*width = dep^2 / slope (slope = dep/width)
        a = a_bf + b * dep + dep ** 2 / sd_ch(i)%fps
        rh = a / p
        ifp_dep = i_dep + 2
        ch_rcurv(i)%elev(ifp_dep)%dep = (1. + frac_abov) * sd_ch(i)%chd
        ch_rcurv(i)%elev(ifp_dep)%wet_perim = p
        ch_rcurv(i)%elev(ifp_dep)%xsec_area = a
        ch_rcurv(i)%elev(ifp_dep)%top_wid = sd_ch(i)%chw + 2. * dep / sd_ch(i)%fps
        ch_rcurv(i)%elev(ifp_dep)%surf_area = ch_rcurv(i)%elev(ifp_dep)%top_wid * sd_ch(i)%chl
        ch_rcurv(i)%elev(ifp_dep)%vol_ch = vol_bf
        ch_rcurv(i)%elev(ifp_dep)%vol_fp = ch_rcurv(i)%elev(ifp_dep)%top_wid * dep * sd_ch(i)%chl * 1000.
        ch_rcurv(i)%elev(ifp_dep)%vol = vol_bf + ch_rcurv(i)%elev(ifp_dep)%vol_fp
        ch_rcurv(i)%elev(ifp_dep)%flo_rate = Qman(a, rh, sd_ch(i)%fpn, sd_ch(i)%chs)
        vel = Qman(1., rh, sd_ch(i)%fpn, sd_ch(i)%chs)
        ch_rcurv(i)%elev(ifp_dep)%ttime = sd_ch(i)%chl / (3.6 * vel)
      end do

      return
      end subroutine sd_rating_curve