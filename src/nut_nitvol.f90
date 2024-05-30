      subroutine nut_nitvol

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine estimates daily mineralization (NH3 to NO3)
!!    and volatilization of NH3

      use septic_data_module
      use basin_module
      use organic_mineral_mass_module
      use hru_module, only : ihru, i_sep, isep
      use soil_module
      
      implicit none 
      
      integer :: j                !none          |HRU number
      integer :: k                !none          |counter 
      real :: sw25                !              |
      real :: swwp                !              |  
      real :: swf                 !cfu           |fraction of manure containing active colony forming units 
      real :: xx                  !              | 
      real :: dmidl               !              | 
      real :: dpf                 !              |  
      real :: akn                 !              | 
      real :: akv                 !              | 
      real :: rnv                 !              |
      real :: rnit                !kg N/ha       |amount of nitrogen moving from the NH3 to the
                                  !              |NO3 pool (nitrification) in the layer
      real :: rvol                !kg N/ha       |amount of nitrogen lost from the NH3 pool due
                                  !              |to volatilization
      real :: tf                  !              | 
      real :: cecf = 0.15         !none          |volatilization CEC factor 
 
      j = 0
      j = ihru 

      do k = 1, soil(j)%nly
        tf = 0.
        tf = .41 * (soil(j)%phys(k)%tmp - 5.) / 10.

        if (soil1(j)%mn(k)%nh4 > 0. .and. tf >= 0.001) then
          sw25 = 0.
          swwp = 0.
          sw25 = soil(j)%phys(k)%wpmm + 0.25 * soil(j)%phys(k)%fc
          swwp = soil(j)%phys(k)%wpmm + soil(j)%phys(k)%st
          if (swwp < sw25) then
            swf = 0.
            swf=(swwp-soil(j)%phys(k)%wpmm)/(sw25-soil(j)%phys(k)%wpmm)
          else
            swf = 1.
          endif

          if (k == 1) then
            xx = 0.
          else
            xx = soil(j)%phys(k-1)%d
          endif

          dmidl = (soil(j)%phys(k)%d + xx) / 2.
          dpf = 1. - dmidl / (dmidl + Exp(4.706 - .0305 * dmidl))
          akn = tf * swf
          akv = tf * dpf * cecf
          rnv = soil1(j)%mn(k)%nh4 * (1. - Exp(-akn - akv))
          rnit = 1. - Exp(-akn)
          rvol = 1. - Exp(-akv)

          !! calculate nitrification (NH3 => NO3)
	    !! apply septic algorithm only to active septic systems
          if(k/=i_sep(j).or.sep(isep)%opt /= 1) then  ! J.Jeong for septic, biozone layer
             if (rvol + rnit > 1.e-6) then
               rvol = rnv * rvol / (rvol + rnit)
               rnit = rnv - rvol
               if (rnit < 0.) rnit = 0.
               soil1(j)%mn(k)%nh4 = Max(1.e-6, soil1(j)%mn(k)%nh4 - rnit)
             endif
             if (soil1(j)%mn(k)%nh4 < 0.) then
               rnit = rnit + soil1(j)%mn(k)%nh4
               soil1(j)%mn(k)%nh4 = 0.
             endif
             soil1(j)%mn(k)%no3 = soil1(j)%mn(k)%no3 + rnit

             !! calculate ammonia volatilization
             soil1(j)%mn(k)%nh4 = Max(1.e-6, soil1(j)%mn(k)%nh4 - rvol)
             if (soil1(j)%mn(k)%nh4 < 0.) then
               rvol = rvol + soil1(j)%mn(k)%nh4
               soil1(j)%mn(k)%nh4 = 0.
             endif
          end if
        end if

      end do

      return
      end subroutine nut_nitvol