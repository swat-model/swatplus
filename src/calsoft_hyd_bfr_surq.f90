      subroutine calsoft_hyd_bfr_surq

      use hru_module, only : cn2, hru, hru_init
      use soil_module
      use plant_module
      use hydrograph_module
      use ru_module
      use aquifer_module
      use channel_module
      use hru_lte_module
      use sd_channel_module
      use basin_module
      use maximum_data_module
      use calibration_data_module
      use conditional_module
      use reservoir_module
      use organic_mineral_mass_module
      use time_module
      
      implicit none
      
      integer :: isim          !          |
      integer :: ireg          !none      |counter
      integer :: ilum          !none      |counter
      integer :: iihru         !none      |counter
      integer :: icn           !none      |counter
      integer :: ihru_s        !none      |counter
      integer :: iter_ind      !          |end of loop
      real :: rmeas            !          |
      real :: denom            !          |
      real :: soft             !          |
      real :: diff             !          |
      real :: chg_val          !          |  

      ! calibrate cn3_swf for surface runoff
        iter_ind = 1
  
        ! 1st cn3_swf adjustment
        isim = 0
        do ireg = 1, db_mx%lsu_reg
          do ilum = 1, region(ireg)%nlum
            soft = lscal(ireg)%lum(ilum)%meas%srr * lscal(ireg)%lum(ilum)%precip_aa
            diff = 0.
            if (soft > 1.e-6) diff = abs((soft - lscal(ireg)%lum(ilum)%aa%srr) / soft)
            if (diff > .02 .and. lscal(ireg)%lum(ilum)%ha > 1.e-6 .and. lscal(ireg)%lum(ilum)%prm_lim%cn3_swf < 1.e-6) then
            isim = 1
            
                lscal(ireg)%lum(ilum)%prm_prev = lscal(ireg)%lum(ilum)%prm
                lscal(ireg)%lum(ilum)%prev = lscal(ireg)%lum(ilum)%aa

                diff = lscal(ireg)%lum(ilum)%meas%srr * lscal(ireg)%lum(ilum)%precip_aa - lscal(ireg)%lum(ilum)%aa%srr
                chg_val = - diff / 100.     !assume 10 mm runoff for .1 cn3_swf
                !chg_val = - diff / 700.     !assume 10 mm runoff for .5 cn3_swf
                lscal(ireg)%lum(ilum)%prm_prev%cn3_swf = lscal(ireg)%lum(ilum)%prm%cn3_swf
                lscal(ireg)%lum(ilum)%prm%cn3_swf = lscal(ireg)%lum(ilum)%prm%cn3_swf + chg_val
                lscal(ireg)%lum(ilum)%prev%srr = lscal(ireg)%lum(ilum)%aa%srr
                
                if (lscal(ireg)%lum(ilum)%prm%cn3_swf >= ls_prms(10)%pos) then
                  chg_val = ls_prms(10)%pos - lscal(ireg)%lum(ilum)%prm_prev%cn3_swf
                  lscal(ireg)%lum(ilum)%prm%cn3_swf = ls_prms(10)%pos
                  lscal(ireg)%lum(ilum)%prm_lim%cn3_swf = 1.
                end if
                if (lscal(ireg)%lum(ilum)%prm%cn3_swf <= ls_prms(10)%neg) then
                  chg_val = ls_prms(10)%neg - lscal(ireg)%lum(ilum)%prm_prev%cn3_swf
                  lscal(ireg)%lum(ilum)%prm%cn3_swf = ls_prms(10)%neg
                  lscal(ireg)%lum(ilum)%prm_lim%cn3_swf = 1.
                end if

            do ihru_s = 1, region(ireg)%num_tot
              iihru = region(ireg)%num(ihru_s)
              if (lscal(ireg)%lum(ilum)%meas%name == hru(iihru)%lum_group_c .or. lscal(ireg)%lum(ilum)%meas%name == "basin") then
                !set parms for 1st perco calibration
                !! don't change for tile  ***Mike
                if (hru(iihru)%tiledrain == 0) then
                hru(iihru)%hyd%cn3_swf = hru(iihru)%hyd%cn3_swf + chg_val
                hru(iihru)%hyd%cn3_swf = amin1 (hru(iihru)%hyd%cn3_swf, ls_prms(10)%up)
                hru(iihru)%hyd%cn3_swf = Max (hru(iihru)%hyd%cn3_swf, ls_prms(10)%lo)
                hru_init(iihru)%hyd%cn3_swf = hru(iihru)%hyd%cn3_swf
                call curno (cn2(iihru), iihru)
                end if
              end if
            end do
            lscal(ireg)%lum(ilum)%nbyr = 0
            lscal(ireg)%lum(ilum)%precip_aa = 0.
            lscal(ireg)%lum(ilum)%aa = lscal_z
            !plcal(ireg)%lum(ilum)%nbyr = 0
            !plcal(ireg)%lum(ilum)%precip_aa = 0.
            !plcal(ireg)%lum(ilum)%ha = 0.
            !plcal(ireg)%lum(ilum)%aa = plcal_z
          end if
          end do
        end do

        !! re-initialize all objects
        call re_initialize

        ! 1st cn3_swf adjustment 
        if (isim > 0) then
          cal_sim =  " first cn3_swf adj "
          cal_adj = chg_val
          call time_control
        end if

          ! adjust surface runoff using cn3_swf
          do icn = 1, 2 !iter_ind
          isim = 0
          do ireg = 1, db_mx%lsu_reg
          do ilum = 1, region(ireg)%nlum
            soft = lscal(ireg)%lum(ilum)%meas%srr * lscal(ireg)%lum(ilum)%precip_aa
            diff = 0.
            if (soft > 1.e-6) diff = abs((soft - lscal(ireg)%lum(ilum)%aa%srr) / soft)
            if (diff > .02 .and. lscal(ireg)%lum(ilum)%ha > 1.e-6) then ! .and. lscal(ireg)%lum(ilum)%prm_lim%cn3_swf < 1.e-6) then
            isim = 1
            
                rmeas = lscal(ireg)%lum(ilum)%meas%srr * lscal(ireg)%lum(ilum)%precip_aa
                denom = lscal(ireg)%lum(ilum)%prev%srr - lscal(ireg)%lum(ilum)%aa%srr
                if (abs(denom) > 1.) then
                  chg_val = - (lscal(ireg)%lum(ilum)%prm_prev%cn3_swf - lscal(ireg)%lum(ilum)%prm%cn3_swf)                  &
                    * (lscal(ireg)%lum(ilum)%aa%srr - rmeas) / denom
                else
                  chg_val = - diff / 10.
                end if
                lscal(ireg)%lum(ilum)%prm_prev%cn3_swf = lscal(ireg)%lum(ilum)%prm%cn3_swf
                lscal(ireg)%lum(ilum)%prm%cn3_swf = lscal(ireg)%lum(ilum)%prm%cn3_swf + chg_val
                lscal(ireg)%lum(ilum)%prev%srr = lscal(ireg)%lum(ilum)%aa%srr
                                
                if (lscal(ireg)%lum(ilum)%prm%cn3_swf >= ls_prms(10)%pos) then
                  chg_val = ls_prms(10)%pos - lscal(ireg)%lum(ilum)%prm_prev%cn3_swf
                  lscal(ireg)%lum(ilum)%prm%cn3_swf = ls_prms(10)%pos
                  lscal(ireg)%lum(ilum)%prm_lim%cn3_swf = 1.
                end if
                if (lscal(ireg)%lum(ilum)%prm%cn3_swf <= ls_prms(10)%neg) then
                  chg_val = ls_prms(10)%neg - lscal(ireg)%lum(ilum)%prm_prev%cn3_swf
                  lscal(ireg)%lum(ilum)%prm%cn3_swf = ls_prms(10)%neg
                  lscal(ireg)%lum(ilum)%prm_lim%cn3_swf = 1.
                end if
            
            !check all hru"s for proper lum
            do ihru_s = 1, region(ireg)%num_tot
              iihru = region(ireg)%num(ihru_s)
              if (lscal(ireg)%lum(ilum)%meas%name == hru(iihru)%lum_group_c .or. lscal(ireg)%lum(ilum)%meas%name == "basin") then
                !set parms for 1st perco calibration
                !! don't change for tile  *********************Mike
                if (hru(iihru)%tiledrain == 0) then
                hru(iihru)%hyd%cn3_swf = hru(iihru)%hyd%cn3_swf + chg_val
                hru(iihru)%hyd%cn3_swf = amin1 (hru(iihru)%hyd%cn3_swf, ls_prms(10)%up)
                hru(iihru)%hyd%cn3_swf = Max (hru(iihru)%hyd%cn3_swf, ls_prms(10)%lo)
                hru_init(iihru)%hyd%cn3_swf = hru(iihru)%hyd%cn3_swf
                call curno (cn2(iihru), iihru)
                end if
              end if
            end do
            lscal(ireg)%lum(ilum)%nbyr = 0
            lscal(ireg)%lum(ilum)%precip_aa = 0.
            lscal(ireg)%lum(ilum)%aa = lscal_z
            !plcal(ireg)%lum(ilum)%nbyr = 0
            !plcal(ireg)%lum(ilum)%precip_aa = 0.
            !plcal(ireg)%lum(ilum)%ha = 0.
            !plcal(ireg)%lum(ilum)%aa = plcal_z
            end if
          end do
        end do

        !! re-initialize all objects
        call re_initialize

        ! cn3_swf adjustment
        if (isim > 0) then
          cal_sim =  " cn3_swf adj "
          cal_adj = chg_val
          !pco%wb_hru%a = "y"
          !if (icn == 2) pco%wb_hru%d = "y"
          call time_control
        end if
        end do      ! icn

	  return
      end subroutine calsoft_hyd_bfr_surq