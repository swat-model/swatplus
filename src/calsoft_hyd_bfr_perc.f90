      subroutine calsoft_hyd_bfr_perc

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
      integer :: iperco        !none      |counter
      real :: rmeas            !          |
      real :: denom            !          |
      real :: soft             !          |
      real :: diff             !          |
      real :: chg_val          !          | 
      real :: perc_ln_func

      ! calibrate percolation
        iter_ind = 3
        
        ! 1st perco for percolation
        isim = 0
        do ireg = 1, db_mx%lsu_reg
          do ilum = 1, region(ireg)%nlum
            !check all hru"s for proper lum
            soft = lscal(ireg)%lum(ilum)%meas%bfr * lscal(ireg)%lum(ilum)%precip_aa
            diff = 0.
            if (soft > 1.e-6) diff = abs((soft - lscal(ireg)%lum(ilum)%aa%bfr) / soft)
            if (diff > .05 .and. lscal(ireg)%lum(ilum)%ha > 1.e-6 .and. lscal(ireg)%lum(ilum)%prm_lim%perco < 1.e-6) then
            isim = 1
            
                !set parms for 1st perco calibration
                lscal(ireg)%lum(ilum)%prm_prev = lscal(ireg)%lum(ilum)%prm
                lscal(ireg)%lum(ilum)%prev = lscal(ireg)%lum(ilum)%aa

                !chg_val = (soft - lscal(ireg)%lum(ilum)%aa%bfr) / lscal(ireg)%lum(ilum)%aa%bfr   ! assume perco is linear
                chg_val = 0.001 * (soft - lscal(ireg)%lum(ilum)%aa%bfr)
                lscal(ireg)%lum(ilum)%prm_prev%perco = lscal(ireg)%lum(ilum)%prm%perco 
                lscal(ireg)%lum(ilum)%prm%perco = lscal(ireg)%lum(ilum)%prm%perco + chg_val
                lscal(ireg)%lum(ilum)%prev%bfr = lscal(ireg)%lum(ilum)%aa%bfr

                if (lscal(ireg)%lum(ilum)%prm%perco >= ls_prms(8)%pos) then
                  lscal(ireg)%lum(ilum)%prm%perco = ls_prms(8)%pos
                  chg_val = ls_prms(8)%pos
                  lscal(ireg)%lum(ilum)%prm_lim%perco = 1.
                end if
                if (lscal(ireg)%lum(ilum)%prm%perco <= ls_prms(8)%neg) then
                  lscal(ireg)%lum(ilum)%prm%perco = ls_prms(8)%neg
                  chg_val = ls_prms(8)%neg
                  lscal(ireg)%lum(ilum)%prm_lim%perco = 1.
                end if
                
            do ihru_s = 1, region(ireg)%num_tot
              iihru = region(ireg)%num(ihru_s)
              if (lscal(ireg)%lum(ilum)%meas%name == hru(iihru)%lum_group_c .or. lscal(ireg)%lum(ilum)%meas%name == "basin") then
                !! don't change for tile  *********************Mike
                if (hru(iihru)%tiledrain == 0) then
                hru(iihru)%hyd%perco = hru(iihru)%hyd%perco + chg_val
                hru(iihru)%hyd%perco = amin1 (hru(iihru)%hyd%perco, ls_prms(8)%up)
                hru(iihru)%hyd%perco = Max (hru(iihru)%hyd%perco, ls_prms(8)%lo)
                hru_init(iihru)%hyd%perco = hru(iihru)%hyd%perco
                if (hru(iihru)%hyd%perco > 1.e-9) then
                  perc_ln_func = 1.0052 * log(-log(hru(iihru)%hyd%perco - 1.e-6)) + 5.6862
                  hru(iihru)%hyd%perco_lim = exp(-perc_ln_func)
                  hru(iihru)%hyd%perco_lim = amin1 (1., hru(iihru)%hyd%perco_lim)
                else
                  hru(iihru)%hyd%perco_lim = 0.
                end if
                hru_init(iihru)%hyd%perco_lim = hru(iihru)%hyd%perco_lim
                end if
              end if
            end do
            
            lscal(ireg)%lum(ilum)%nbyr = 0
            lscal(ireg)%lum(ilum)%precip_aa = 0.
            lscal(ireg)%lum(ilum)%aa = lscal_z
            else
            lscal(ireg)%lum(ilum)%prm_lim%perco = 1.
            end if
            end do
        end do
        
        !zero plant calibration data in case plants are calibrated
        !do ireg = 1, db_mx%plcal_reg
        !  do ilum = 1, plcal(ireg)%lum_num
        !    plcal(ireg)%lum(ilum)%nbyr = 0
        !    plcal(ireg)%lum(ilum)%precip_aa = 0.
        !    plcal(ireg)%lum(ilum)%ha = 0.
        !    plcal(ireg)%lum(ilum)%aa = plcal_z
        !  end do
        !end do
        
        !! re-initialize all objects
        call re_initialize

        ! 1st perco adjustment 
        if (isim > 0) then
          cal_sim =  " first perco adj "
          cal_adj = chg_val
          call time_control
        end if
  
          ! adjust percolation using perco
          do iperco = 1, 3  !iter_ind
          isim = 0
          do ireg = 1, db_mx%lsu_reg
          do ilum = 1, region(ireg)%nlum
            soft = lscal(ireg)%lum(ilum)%meas%bfr * lscal(ireg)%lum(ilum)%precip_aa
            diff = 0.
            if (soft > 1.e-6) diff = abs((soft - lscal(ireg)%lum(ilum)%aa%bfr) / soft)
            if (diff > .05 .and. lscal(ireg)%lum(ilum)%ha > 1.e-6) then
            isim = 1

                rmeas = lscal(ireg)%lum(ilum)%meas%bfr * lscal(ireg)%lum(ilum)%precip_aa
                denom = lscal(ireg)%lum(ilum)%prev%bfr - lscal(ireg)%lum(ilum)%aa%bfr
                if (abs(denom) > 2.) then
                  chg_val = - (lscal(ireg)%lum(ilum)%prm_prev%perco - lscal(ireg)%lum(ilum)%prm%perco)                  &
                    * (lscal(ireg)%lum(ilum)%aa%bfr - rmeas) / denom
                else
                  chg_val = 0.
                end if
                lscal(ireg)%lum(ilum)%prm_prev%perco = lscal(ireg)%lum(ilum)%prm%perco 
                lscal(ireg)%lum(ilum)%prm%perco = lscal(ireg)%lum(ilum)%prm%perco + chg_val
                lscal(ireg)%lum(ilum)%prev%bfr = lscal(ireg)%lum(ilum)%aa%bfr

                if (lscal(ireg)%lum(ilum)%prm%perco >= ls_prms(8)%pos) then
                  lscal(ireg)%lum(ilum)%prm%perco = ls_prms(8)%pos
                  chg_val = ls_prms(8)%pos
                  lscal(ireg)%lum(ilum)%prm_lim%perco = 1.
                end if
                if (lscal(ireg)%lum(ilum)%prm%perco <= ls_prms(8)%neg) then
                  lscal(ireg)%lum(ilum)%prm%perco = ls_prms(8)%neg
                  chg_val = ls_prms(8)%neg
                  lscal(ireg)%lum(ilum)%prm_lim%perco = 1.
                end if
                
            do ihru_s = 1, region(ireg)%num_tot
              iihru = region(ireg)%num(ihru_s)
              if (lscal(ireg)%lum(ilum)%meas%name == hru(iihru)%lum_group_c .or. lscal(ireg)%lum(ilum)%meas%name == "basin") then
                !set parms for 1st perco calibration
                !! don't change for tile  *********************Mike
                if (hru(iihru)%tiledrain == 0) then
                hru(iihru)%hyd%perco = hru(iihru)%hyd%perco + chg_val
                hru(iihru)%hyd%perco = amin1 (hru(iihru)%hyd%perco, ls_prms(8)%up)
                hru(iihru)%hyd%perco = Max (hru(iihru)%hyd%perco, ls_prms(8)%lo)
                hru_init(iihru)%hyd%perco = hru(iihru)%hyd%perco
                if (hru(iihru)%hyd%perco > 1.e-9) then
                  perc_ln_func = 1.0052 * log(-log(hru(iihru)%hyd%perco - 1.e-6)) + 5.6862
                  hru(iihru)%hyd%perco_lim = exp(-perc_ln_func)
                  hru(iihru)%hyd%perco_lim = amin1 (1., hru(iihru)%hyd%perco_lim)
                else
                  hru(iihru)%hyd%perco_lim = 0.
                end if
                hru_init(iihru)%hyd%perco_lim = hru(iihru)%hyd%perco_lim
                end if
              end if
            end do
            
            lscal(ireg)%lum(ilum)%nbyr = 0
            lscal(ireg)%lum(ilum)%precip_aa = 0.
            lscal(ireg)%lum(ilum)%aa = lscal_z
            else
            lscal(ireg)%lum(ilum)%prm_lim%perco = 1.
            end if
          end do
          end do
          
        !! re-initialize all objects
        call re_initialize

        ! perco adjustment 
        if (isim > 0) then
          cal_sim =  " perco adj "
          cal_adj = chg_val
          call time_control
        end if
        
        end do      ! iperco  

	  return
      end subroutine calsoft_hyd_bfr_perc