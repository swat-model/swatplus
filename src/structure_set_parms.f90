      subroutine structure_set_parms (str_name, istr, j)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine controls the simulation of the land phase of the 
!!    hydrologic cycle
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use mgt_operations_module
      use hru_module, only : hru, sdr, iseptic, t_ov, tc_gwat
      use soil_module
      
      implicit none
    
      character (len=16), intent (in) :: str_name     !              |
      integer, intent (in) :: istr                    !              |
      integer, intent (in) :: j                       !none          |HRU number
      integer :: jj                                   !none          |counter
      real :: tch                                     !              |
      real :: b                                       !m             |bottom width of channel

      select case(str_name)

      case ("tiledrain")
        hru(j)%sdr = sdr(istr)
        hru(j)%lumv%sdr_dep = sdr(istr)%depth
        !! define soil layer the drainage tile is in
        if (sdr(istr)%depth > 0) then
          do jj = 1, soil(j)%nly
            if (hru(j)%lumv%sdr_dep < soil(j)%phys(jj)%d) hru(j)%lumv%ldrain = jj
            if (hru(j)%lumv%sdr_dep < soil(j)%phys(jj)%d) exit
          end do
        else
          hru(j)%lumv%ldrain = 0
        end if
        !! setting tile lage time
        if (hru(j)%lumv%ldrain > 0 .and. sdr(istr)%lag > 0.01) then
          hru(j)%lumv%tile_ttime = 1. - Exp(-24. /sdr(istr)%lag)
        else
          hru(j)%lumv%tile_ttime = 0.
        end if

      case ("fstrip")
        hru(j)%lumv%vfsi = filtstrip_db(istr)%vfsi
        hru(j)%lumv%vfsratio = filtstrip_db(istr)%vfsratio
        hru(j)%lumv%vfscon = filtstrip_db(istr)%vfscon
        hru(j)%lumv%vfsch = filtstrip_db(istr)%vfsch

        !! Set some defaults if needed
        if (hru(j)%lumv%vfsratio <= 0.) hru(j)%lumv%vfsratio = 0.
        !! minimum value for vfsratio is 0 max is 300
        if (hru(j)%lumv%vfsratio <= 0.) hru(j)%lumv%vfsratio = 0.
        if (hru(j)%lumv%vfsratio > 300) hru(j)%lumv%vfsratio = 300
        !! minimum value for vfscon is 0.1 default is 0.5 max is 0.95
        if (hru(j)%lumv%vfscon <= 0) hru(j)%lumv%vfscon = 0.5
        if (hru(j)%lumv%vfscon <= 0.1) hru(j)%lumv%vfscon = 0.1
        if (hru(j)%lumv%vfscon > 0.95) hru(j)%lumv%vfscon = 0.95
        !! minimum value for vfsch is 0 max is .95
        if (hru(j)%lumv%vfsch <= 0.) hru(j)%lumv%vfsch = 0.
        if (hru(j)%lumv%vfsch > .95) hru(j)%lumv%vfsch = .95

      case ("grassww")
        hru(j)%lumv%ngrwat = istr
        if (istr > 0)  then
          hru(j)%lumv%grwat_i = grwaterway_db(istr)%grwat_i
          hru(j)%lumv%grwat_n = grwaterway_db(istr)%grwat_n
          hru(j)%lumv%grwat_spcon = grwaterway_db(istr)%grwat_spcon
          hru(j)%lumv%grwat_d = grwaterway_db(istr)%grwat_d
          hru(j)%lumv%grwat_w = grwaterway_db(istr)%grwat_w
          hru(j)%lumv%grwat_l = grwaterway_db(istr)%grwat_l
          hru(j)%lumv%grwat_s = grwaterway_db(istr)%grwat_s
          !! Set defaults - Mannings via Fiener, 2006
          if (hru(j)%lumv%grwat_n <=0.) hru(j)%lumv%grwat_n = 0.35 
          !! length based on one side of a square HRU
          if (hru(j)%lumv%grwat_l <=0.) hru(j)%lumv%grwat_l = hru(j)%km**.5
          !! default to a workable depth based on with and 8:1 sideslopes
          if (hru(j)%lumv%grwat_d <= 0.) then
            hru(j)%lumv%grwat_d = 3. / 64. * hru(j)%lumv%grwat_w
          end if
          !! Default to 3/4 HRU slope
          if (hru(j)%lumv%grwat_s <=0.) hru(j)%lumv%grwat_s = hru(j)%topo%slope * .75
          !! default sed coeff to 0.005
          if (hru(j)%lumv%grwat_spcon <= 0.) hru(j)%lumv%grwat_spcon = 0.005

          !! Calculate time of concentration for waterway similar to hydro_init.f
          tch = .62 * hru(j)%lumv%grwat_l * hru(j)%lumv%grwat_n ** .6 / (hru(j)%km ** .125 * hru(j)%lumv%grwat_s ** .375)
          tc_gwat(j) = tch + t_ov(j)

          !!Check the channel to make sure the enter width and depth will work with 8:1 trap channel, assume width is correct
          b = hru(j)%lumv%grwat_w - 2. * hru(j)%lumv%grwat_d * 8
          
          !! Depth and Width not possible with 8:1 sideslope and trapazoidal channel assume b =.25*width
          if (b <= 0.) hru(j)%lumv%grwat_d = 3. / 64. * hru(j)%lumv%grwat_w

          call ttcoef_wway(j)
        end if

      case ("user_def")                 !user defined Upland CP removal MJW
          hru(j)%lumv%bmp_flag = 1
          hru(j)%lumv%bmp_sed = bmpuser_db(istr)%bmp_sed !! Sediment
          hru(j)%lumv%bmp_pp = bmpuser_db(istr)%bmp_pp   !! Particulate P
          hru(j)%lumv%bmp_sp = bmpuser_db(istr)%bmp_sp   !! Soluble P
          hru(j)%lumv%bmp_pn =  bmpuser_db(istr)%bmp_pn  !! Particulate N
          hru(j)%lumv%bmp_sn = bmpuser_db(istr)%bmp_sn   !! Soluble N
          hru(j)%lumv%bmp_bac = bmpuser_db(istr)%bmp_bac !! Bacteria
      
      case ("septic")
        iseptic(j) = istr

      end select

      return
      end subroutine structure_set_parms