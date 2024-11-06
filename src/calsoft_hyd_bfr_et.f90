      subroutine calsoft_hyd_bfr_et

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
      
      integer :: isim = 0      !          |
      integer :: ireg = 0      !none      |counter
      integer :: ilum = 0      !none      |counter
      integer :: iihru = 0     !none      |counter
      integer :: ihru_s = 0    !none      |counter
      integer :: iter_ind = 0  !          |end of loop
      integer :: ietco = 0     !none      |counter
      real :: rmeas = 0.       !          |
      real :: denom = 0.       !          |
      real :: soft = 0.        !          |
      real :: diff = 0.        !          |
      real :: chg_val = 0.     !          |   
      real :: pred = 0.

      ! calibrate esco and pet for water yield
        iter_ind = 1
        
              
            

                

            
        


        
        ! 1st esco adjustment
        isim = 0
        do ireg = 1, db_mx%lsu_reg
          do ilum = 1, region(ireg)%nlum
            soft = (lscal(ireg)%lum(ilum)%meas%wyr + lscal(ireg)%lum(ilum)%meas%pcr) * lscal(ireg)%lum(ilum)%precip_aa
            pred = lscal(ireg)%lum(ilum)%aa%wyr
            diff = 0.
            if (soft > 1.e-6) diff = abs((soft - pred) / soft)
            if (diff > .01 .and. lscal(ireg)%lum(ilum)%ha > 1.e-6 .and. lscal(ireg)%lum(ilum)%prm_lim%etco < 1.e-6) then
            isim = 1
            
                lscal(ireg)%lum(ilum)%prm_prev = lscal(ireg)%lum(ilum)%prm
                lscal(ireg)%lum(ilum)%prev = lscal(ireg)%lum(ilum)%aa

                diff = soft - pred
                chg_val = - diff / 250.     ! increment etco .4 for every 100 mm difference
                lscal(ireg)%lum(ilum)%prm_prev%etco = lscal(ireg)%lum(ilum)%prm%etco
                lscal(ireg)%lum(ilum)%prm%etco = lscal(ireg)%lum(ilum)%prm%etco + chg_val
                lscal(ireg)%lum(ilum)%prev%wyr = lscal(ireg)%lum(ilum)%aa%wyr
                
                if (lscal(ireg)%lum(ilum)%prm%etco >= ls_prms(2)%pos) then
                  chg_val = ls_prms(2)%pos - lscal(ireg)%lum(ilum)%prm_prev%etco
                  lscal(ireg)%lum(ilum)%prm%etco = ls_prms(2)%pos
                  lscal(ireg)%lum(ilum)%prm_lim%etco = 1.
                end if
                if (lscal(ireg)%lum(ilum)%prm%etco <= ls_prms(2)%neg) then
                  chg_val = lscal(ireg)%lum(ilum)%prm_prev%etco + ls_prms(2)%neg
                  lscal(ireg)%lum(ilum)%prm%etco = ls_prms(2)%neg
                  lscal(ireg)%lum(ilum)%prm_lim%etco = 1.
                end if
                           
            do ihru_s = 1, region(ireg)%num_tot
              iihru = region(ireg)%num(ihru_s)
              if (lscal(ireg)%lum(ilum)%meas%name == hru(iihru)%lum_group_c .or. lscal(ireg)%lum(ilum)%meas%name == "basin") then
                !set parms for 1st et calibration
                hru(iihru)%hyd%esco = hru(iihru)%hyd%esco + chg_val
                hru(iihru)%hyd%esco = amin1 (hru(iihru)%hyd%esco, ls_prms(2)%up)
                hru(iihru)%hyd%esco = Max (hru(iihru)%hyd%esco, ls_prms(2)%lo)
                hru_init(iihru)%hyd%esco = hru(iihru)%hyd%esco
                !hru(iihru)%hyd%epco = hru(iihru)%hyd%epco + chg_val * .5
                !hru(iihru)%hyd%epco = amin1 (hru(iihru)%hyd%epco, ls_prms(2)%up)
                !hru(iihru)%hyd%epco = Max (hru(iihru)%hyd%epco, ls_prms(2)%lo)
                !hru_init(iihru)%hyd%epco = hru(iihru)%hyd%epco
              end if
            end do
            lscal(ireg)%lum(ilum)%nbyr = 0
            lscal(ireg)%lum(ilum)%precip_aa = 0.
            lscal(ireg)%lum(ilum)%aa = lscal_z
            !zero plant calibration data in case plants are calibrated
            !plcal(ireg)%lum(ilum)%nbyr = 0
            !plcal(ireg)%lum(ilum)%precip_aa = 0.
            !plcal(ireg)%lum(ilum)%ha = 0.
            !plcal(ireg)%lum(ilum)%aa = plcal_z

          end if
          end do
        end do
            
        !! re-initialize all objects
        call re_initialize

        ! 1st esco adjustment 
        if (isim > 0) then
          cal_sim =  " first esco adj "
          cal_adj = chg_val
          call time_control
        end if
        
        ! adjust et using esco
        do ietco = 1, 2     !iter_ind
          isim = 0
          do ireg = 1, db_mx%lsu_reg
          do ilum = 1, region(ireg)%nlum
            soft = (lscal(ireg)%lum(ilum)%meas%wyr + lscal(ireg)%lum(ilum)%meas%pcr) * lscal(ireg)%lum(ilum)%precip_aa
            pred = lscal(ireg)%lum(ilum)%aa%wyr
            diff = 0.
            if (soft > 1.e-6) diff = abs((soft - pred) / soft)
            if (diff > .01 .and. lscal(ireg)%lum(ilum)%ha > 1.e-6 .and. lscal(ireg)%lum(ilum)%prm_lim%etco < 1.e-6) then
            isim = 1
            
                rmeas = soft
                denom = lscal(ireg)%lum(ilum)%prev%wyr - lscal(ireg)%lum(ilum)%aa%wyr
                if (abs(denom) > 1.) then
                  chg_val = - (lscal(ireg)%lum(ilum)%prm_prev%etco - lscal(ireg)%lum(ilum)%prm%etco)                &
                                                                                 * (pred - rmeas) / denom
                else
                  diff = soft - pred
                  chg_val = - diff / 200.
                end if
                ! lower chg_val - changing both esco and epco causes esco to go too low
                !chg_val = chg_val * .5
                
                lscal(ireg)%lum(ilum)%prm_prev%etco = lscal(ireg)%lum(ilum)%prm%etco
                lscal(ireg)%lum(ilum)%prm%etco = lscal(ireg)%lum(ilum)%prm%etco + chg_val
                lscal(ireg)%lum(ilum)%prev%wyr = lscal(ireg)%lum(ilum)%aa%wyr
                      
                if (lscal(ireg)%lum(ilum)%prm%etco >= ls_prms(2)%pos) then
                  chg_val = ls_prms(2)%pos - lscal(ireg)%lum(ilum)%prm_prev%etco
                  lscal(ireg)%lum(ilum)%prm%etco = ls_prms(2)%pos
                  lscal(ireg)%lum(ilum)%prm_lim%etco = 1.
                end if
                if (lscal(ireg)%lum(ilum)%prm%etco <= ls_prms(2)%neg) then
                  chg_val = ls_prms(2)%neg - lscal(ireg)%lum(ilum)%prm_prev%etco
                  lscal(ireg)%lum(ilum)%prm%etco = ls_prms(2)%neg
                  lscal(ireg)%lum(ilum)%prm_lim%etco = 1.
                end if
                
            do ihru_s = 1, region(ireg)%num_tot
                iihru = region(ireg)%num(ihru_s)
              if (lscal(ireg)%lum(ilum)%meas%name == hru(iihru)%lum_group_c .or. lscal(ireg)%lum(ilum)%meas%name == "basin") then
                !set parms for et calibration
                hru(iihru)%hyd%esco = hru(iihru)%hyd%esco + chg_val
                hru(iihru)%hyd%esco = amin1 (hru(iihru)%hyd%esco, ls_prms(2)%up)
                hru(iihru)%hyd%esco = Max (hru(iihru)%hyd%esco, ls_prms(2)%lo)
                hru_init(iihru)%hyd%esco = hru(iihru)%hyd%esco
                !hru(iihru)%hyd%epco = hru(iihru)%hyd%epco + chg_val * .5
                !hru(iihru)%hyd%epco = amin1 (hru(iihru)%hyd%epco, ls_prms(2)%up)
                !hru(iihru)%hyd%epco = Max (hru(iihru)%hyd%epco, ls_prms(2)%lo)
                !hru_init(iihru)%hyd%epco = hru(iihru)%hyd%epco
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

          !! re-initialize all objects
          call re_initialize

          ! et adjustment 
          if (isim > 0) then
            cal_sim =  " esco adj "
            cal_adj = chg_val
            call time_control
          end if
        
        end do      ! iesco

      return
      end subroutine calsoft_hyd_bfr_et