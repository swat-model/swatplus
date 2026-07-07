      subroutine calsoft_hyd_bfr_et

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
      
      integer :: iter_all      !none      |counter
      integer :: iterall       !none      |counter
      integer :: isim          !          |
      integer :: ireg          !none      |counter
      integer :: ilum          !none      |counter
      integer :: iihru         !none      |counter
      integer :: icn           !none      |counter
      integer :: ihru_s        !none      |counter
      integer :: iter_ind      !          |end of loop
      integer :: ietco         !none      |counter
      integer :: ik            !none      |counter
      integer :: nly           !          |end of loop
      integer :: iperco        !none      |counter
      real :: rmeas            !          |
      real :: denom            !          |
      real :: soft             !          |
      real :: diff             !          |
      real :: rto              !          |
      real :: chg_val          !          | 
      real :: dep_below_soil   !          |  
      real :: perc_ln_func

      ! calibrate esco and pet for water yield
        iter_ind = 1
        
        ! 1st esco adjustment
        isim = 0
        do ireg = 1, db_mx%lsu_reg
          do ilum = 1, region(ireg)%nlum
            soft = lscal(ireg)%lum(ilum)%meas%wyr * lscal(ireg)%lum(ilum)%precip_aa
            diff = 0.
            if (soft > 1.e-6) diff = abs((soft - lscal(ireg)%lum(ilum)%aa%wyr) / soft)
            if (diff > .01 .and. lscal(ireg)%lum(ilum)%ha > 1.e-6 .and. lscal(ireg)%lum(ilum)%prm_lim%etco < 1.e-6) then
            isim = 1
            
                lscal(ireg)%lum(ilum)%prm_prev = lscal(ireg)%lum(ilum)%prm
                lscal(ireg)%lum(ilum)%prev = lscal(ireg)%lum(ilum)%aa

                diff = lscal(ireg)%lum(ilum)%meas%wyr * lscal(ireg)%lum(ilum)%precip_aa - lscal(ireg)%lum(ilum)%aa%wyr
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
          call time_control
        end if
        
        ! adjust et using esco
        do ietco = 1, 2     !iter_ind
          isim = 0
          do ireg = 1, db_mx%lsu_reg
          do ilum = 1, region(ireg)%nlum
            !check all hru"s for proper lum
            soft = lscal(ireg)%lum(ilum)%meas%wyr * lscal(ireg)%lum(ilum)%precip_aa
            diff = 0.
            if (soft > 1.e-6) diff = abs((soft - lscal(ireg)%lum(ilum)%aa%wyr) / soft)
            if (diff > .01 .and. lscal(ireg)%lum(ilum)%ha > 1.e-6 .and. lscal(ireg)%lum(ilum)%prm_lim%etco < 0.99) then
            isim = 1

                rmeas = lscal(ireg)%lum(ilum)%meas%wyr * lscal(ireg)%lum(ilum)%precip_aa
                denom = lscal(ireg)%lum(ilum)%prev%wyr - lscal(ireg)%lum(ilum)%aa%wyr
                if (abs(denom) > 1.e-6) then
                  chg_val = - (lscal(ireg)%lum(ilum)%prm_prev%etco - lscal(ireg)%lum(ilum)%prm%etco)                  &
                    * (lscal(ireg)%lum(ilum)%aa%wyr - rmeas) / denom
                else
                  diff = lscal(ireg)%lum(ilum)%meas%wyr * lscal(ireg)%lum(ilum)%precip_aa - lscal(ireg)%lum(ilum)%aa%wyr
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
            call time_control
          end if
        
        end do      ! iesco
                    
        ! first calibrate potential et
        do ietco = 1, 2     !iter_ind
        isim = 0
        do ireg = 1, db_mx%lsu_reg
          do ilum = 1, region(ireg)%nlum
              
            soft = lscal(ireg)%lum(ilum)%meas%wyr * lscal(ireg)%lum(ilum)%precip_aa
            diff = 0.
            if (soft > 1.e-6) diff = abs((soft - lscal(ireg)%lum(ilum)%aa%wyr) / soft)
            if (diff > .01 .and. lscal(ireg)%lum(ilum)%ha > 1.e-6) then
            isim = 1
            
                lscal(ireg)%lum(ilum)%prm_prev = lscal(ireg)%lum(ilum)%prm
                lscal(ireg)%lum(ilum)%prev = lscal(ireg)%lum(ilum)%aa

                if (soft < lscal(ireg)%lum(ilum)%aa%wyr) then
                  chg_val = 1. + abs((soft - lscal(ireg)%lum(ilum)%aa%wyr) / soft)
                else
                  chg_val = 1. - abs((lscal(ireg)%lum(ilum)%aa%wyr - soft) / lscal(ireg)%lum(ilum)%aa%wyr)
                end if
                lscal(ireg)%lum(ilum)%prm_prev%petco = lscal(ireg)%lum(ilum)%prm%petco
                if (ietco == 1) then
                  lscal(ireg)%lum(ilum)%prm%petco = chg_val
                else
                  lscal(ireg)%lum(ilum)%prm%petco = lscal(ireg)%lum(ilum)%prm%petco * chg_val
                end if
                lscal(ireg)%lum(ilum)%prm_prev%petco = lscal(ireg)%lum(ilum)%aa%wyr
                
                if (lscal(ireg)%lum(ilum)%prm%petco >= ls_prms(4)%pos) then
                  chg_val = ls_prms(4)%pos
                  lscal(ireg)%lum(ilum)%prm%petco = ls_prms(4)%pos
                  lscal(ireg)%lum(ilum)%prm_lim%petco = 1.
                end if
                if (lscal(ireg)%lum(ilum)%prm%petco <= ls_prms(4)%neg) then
                  chg_val = ls_prms(4)%neg
                  lscal(ireg)%lum(ilum)%prm%petco = ls_prms(4)%neg
                  lscal(ireg)%lum(ilum)%prm_lim%petco = 1.
                end if

            !check all hru"s for proper lum
            do ihru_s = 1, region(ireg)%num_tot
              iihru = region(ireg)%num(ihru_s)
              if (lscal(ireg)%lum(ilum)%meas%name == hru(iihru)%lum_group_c .or. lscal(ireg)%lum(ilum)%meas%name == "basin") then
                !set parms for pet adjustment and run
                hru(iihru)%hyd%harg_pet = chg_val * hru(iihru)%hyd%harg_pet
                hru(iihru)%hyd%harg_pet = amin1 (hru(iihru)%hyd%harg_pet, ls_prms(4)%up)
                hru(iihru)%hyd%harg_pet = Max (hru(iihru)%hyd%harg_pet, ls_prms(4)%lo)
                hru_init(iihru)%hyd%harg_pet = hru(iihru)%hyd%harg_pet
              end if
            end do
            
            lscal(ireg)%lum(ilum)%nbyr = 0
            lscal(ireg)%lum(ilum)%precip_aa = 0.
            lscal(ireg)%lum(ilum)%aa = lscal_z
          end if
          end do
        end do
        
        !! re-initialize all objects
        call re_initialize

        ! 1st cover adjustment 
        if (isim > 0) then
          cal_sim =  " first pet adj "
          call time_control
        end if

      end do    ! petco iterations

	  return
      end subroutine calsoft_hyd_bfr_et