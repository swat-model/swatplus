      subroutine calsoft_hyd_bfr_latq

      use hru_module, only : hru, hru_init
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
      integer :: ihru_s        !none      |counter
      integer :: iter_ind      !          |end of loop
      integer :: ik            !none      |counter
      real :: rmeas            !          |
      real :: denom            !          |
      real :: soft             !          |
      real :: diff             !          |
      real :: chg_val          !          |  

      ! calibrate lateral flow
        iter_ind = 1

        ! 1st latq_co adjustment for lateral soil flow
        isim = 0
        do ireg = 1, db_mx%lsu_reg
          do ilum = 1, region(ireg)%nlum
            !check all hru"s for proper lum
            soft = lscal(ireg)%lum(ilum)%meas%lfr * lscal(ireg)%lum(ilum)%precip_aa
            diff = 0.
            if (soft > 1.e-6) diff = abs((soft - lscal(ireg)%lum(ilum)%aa%lfr) / soft)
            if (diff > .1 .and. lscal(ireg)%lum(ilum)%ha > 1.e-6) then
            isim = 1
            
                lscal(ireg)%lum(ilum)%prm_prev = lscal(ireg)%lum(ilum)%prm
                lscal(ireg)%lum(ilum)%prev = lscal(ireg)%lum(ilum)%aa

                diff = lscal(ireg)%lum(ilum)%meas%lfr * lscal(ireg)%lum(ilum)%precip_aa - lscal(ireg)%lum(ilum)%aa%lfr
                chg_val =  0.005 * diff
                !if (soft < lscal(ireg)%lum(ilum)%aa%lfr) then
                !  chg_val = 1. / (abs((soft - lscal(ireg)%lum(ilum)%aa%lfr) / soft) + 1.05)
                !else
                !  chg_val = abs((lscal(ireg)%lum(ilum)%aa%lfr - soft) / lscal(ireg)%lum(ilum)%aa%lfr) + 1.05
                !end if
                lscal(ireg)%lum(ilum)%prm_prev%lat_len = lscal(ireg)%lum(ilum)%prm%lat_len
                lscal(ireg)%lum(ilum)%prm%lat_len = lscal(ireg)%lum(ilum)%prm%lat_len + chg_val
                lscal(ireg)%lum(ilum)%prev%lfr = lscal(ireg)%lum(ilum)%aa%lfr
                           
                if (lscal(ireg)%lum(ilum)%prm%lat_len >= ls_prms(3)%pos) then
                  lscal(ireg)%lum(ilum)%prm%lat_len = ls_prms(3)%pos
                  chg_val = ls_prms(3)%pos
                  lscal(ireg)%lum(ilum)%prm_lim%lat_len = 1.
                end if
                if (lscal(ireg)%lum(ilum)%prm%lat_len <= ls_prms(3)%neg) then
                  lscal(ireg)%lum(ilum)%prm%lat_len = ls_prms(3)%neg
                  chg_val = ls_prms(3)%neg
                  lscal(ireg)%lum(ilum)%prm_lim%lat_len = 1.
                end if
                
            do ihru_s = 1, region(ireg)%num_tot
              iihru = region(ireg)%num(ihru_s)
              if (lscal(ireg)%lum(ilum)%meas%name == hru(iihru)%lum_group_c .or. lscal(ireg)%lum(ilum)%meas%name == "basin") then
                !set parms for 1st perco calibration
                hru(iihru)%hyd%latq_co = hru(iihru)%hyd%latq_co + chg_val
                hru(iihru)%hyd%latq_co = amin1 (hru(iihru)%hyd%latq_co, ls_prms(3)%up)
                hru(iihru)%hyd%latq_co = Max (hru(iihru)%hyd%latq_co, ls_prms(3)%lo)
                hru_init(iihru)%hyd%latq_co = hru(iihru)%hyd%latq_co
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

        ! 1st latq_co adjustment 
        if (isim > 0) then
          cal_sim =  " first latq_co adj "
          cal_adj = chg_val
          call time_control
        end if

        ! adjust latq_co adjustment for lateral soil flow
        do ik = 1, 2    !iter_ind
          isim = 0
        do ireg = 1, db_mx%lsu_reg
          do ilum = 1, region(ireg)%nlum
            soft = lscal(ireg)%lum(ilum)%meas%lfr * lscal(ireg)%lum(ilum)%precip_aa
            diff = 0.
            if (soft > 1.e-6) diff = abs((soft - lscal(ireg)%lum(ilum)%aa%lfr) / soft)
            if (diff > .05 .and. lscal(ireg)%lum(ilum)%ha > 1.e-6) then
            isim = 1
            
                rmeas = lscal(ireg)%lum(ilum)%meas%lfr * lscal(ireg)%lum(ilum)%precip_aa
                denom = lscal(ireg)%lum(ilum)%prev%lfr - lscal(ireg)%lum(ilum)%aa%lfr
                if (abs(denom) > 1.) then
                  chg_val = - (lscal(ireg)%lum(ilum)%prm_prev%lat_len - lscal(ireg)%lum(ilum)%prm%lat_len)                  &
                    * (lscal(ireg)%lum(ilum)%aa%lfr - rmeas) / denom
                else
                  chg_val = - diff
                end if
                lscal(ireg)%lum(ilum)%prm_prev%lat_len = lscal(ireg)%lum(ilum)%prm%lat_len
                lscal(ireg)%lum(ilum)%prm%lat_len = lscal(ireg)%lum(ilum)%prm%lat_len + chg_val
                lscal(ireg)%lum(ilum)%prev%lfr = lscal(ireg)%lum(ilum)%aa%lfr
                           
                if (lscal(ireg)%lum(ilum)%prm%lat_len >= ls_prms(3)%pos) then
                  lscal(ireg)%lum(ilum)%prm%lat_len = ls_prms(3)%pos
                  chg_val = ls_prms(3)%pos
                  lscal(ireg)%lum(ilum)%prm_lim%lat_len = 1.
                end if
                if (lscal(ireg)%lum(ilum)%prm%lat_len <= ls_prms(3)%neg) then
                  lscal(ireg)%lum(ilum)%prm%lat_len = ls_prms(3)%neg
                  chg_val = ls_prms(3)%neg
                  lscal(ireg)%lum(ilum)%prm_lim%lat_len = 1.
                end if
                
            !check all hru"s for proper lum
            do ihru_s = 1, region(ireg)%num_tot
              iihru = region(ireg)%num(ihru_s)
              if (lscal(ireg)%lum(ilum)%meas%name == hru(iihru)%lum_group_c .or. lscal(ireg)%lum(ilum)%meas%name == "basin") then
                !set parms for 1st perco calibration
                hru(iihru)%hyd%latq_co = hru(iihru)%hyd%latq_co + chg_val
                hru(iihru)%hyd%latq_co = amin1 (hru(iihru)%hyd%latq_co, ls_prms(3)%up)
                hru(iihru)%hyd%latq_co = Max (hru(iihru)%hyd%latq_co, ls_prms(3)%lo)
                hru_init(iihru)%hyd%latq_co = hru(iihru)%hyd%latq_co
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

        ! latq_co adjustment for lateral soil flow
        if (isim > 0) then
          cal_sim =  " latq_co adj "
          cal_adj = chg_val
          call time_control
        end if
        end do  

	  return
      end subroutine calsoft_hyd_bfr_latq