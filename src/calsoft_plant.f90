      subroutine calsoft_plant

      use hru_module, only : hru, hru_init
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
      use soil_module
      use plant_module
      use output_landscape_module
      
      implicit none
      
      integer :: iter_all      !          |end of loop
      integer :: iterall       !none      |counter
      integer :: isim          !          |
      integer :: ireg          !none      |counter
      integer :: ilum          !none      |counter
      integer :: iihru         !none      |counter
      integer :: ihru_s        !none      |counter
      integer :: iter_ind      !          !end of loop
      integer :: ist           !          |
      integer :: ipl           !none      |counter for plants in the hru
      integer :: nvar          !          |number of plant cal variables (1=lai_pot, 2=harv_idx)
      real :: rmeas            !          |
      real :: denom            !          |
      real :: soft             !          |
      real :: diff             !          |
      real :: chg_val          !          | 
      real :: perc_ln_func
      
      !calibrate crop yields
        iter_all = 1
        iter_ind = 1
        nvar = 1
        
      ! first check if aeration stress (no tile) is causing low yields
      isim = 0
      do ireg = 1, db_mx%plcal_reg
        do ilum = 1, plcal(ireg)%lum_num
          soft = plcal(ireg)%lum(ilum)%meas%yield
          if (soft > 1.e-6) then
          diff = (plcal(ireg)%lum(ilum)%aa%yield - soft) / soft
          !! if yields are greater than 20% low
          if (diff < -.2) then
            do ihru_s = 1, plcal(ireg)%num_tot
              iihru = plcal(ireg)%num(ihru_s)
              if (hru(iihru)%tiledrain == 0 .and. hru(iihru)%strsa > 50.) then
                isim = 1
                do ipl = 1, pcom(iihru)%npl
                  if (plcal(ireg)%lum(ilum)%meas%name == pcom(iihru)%pl(ipl)) then
                    !set perco
                    hru(iihru)%hyd%perco = 0.95
                    perc_ln_func = 1.0052 * log(-log(hru(iihru)%hyd%perco - 1.e-6)) + 5.6862
                    hru(iihru)%hyd%perco_lim = exp(-perc_ln_func)
                    hru(iihru)%hyd%perco_lim = amin1 (1., hru(iihru)%hyd%perco_lim)
                    hru_init(iihru)%hyd%perco = hru(iihru)%hyd%perco
                    hru_init(iihru)%hyd%perco_lim = hru(iihru)%hyd%perco_lim
                    end if
                end do
                hru(iihru)%strsa = 0.
              end if
            end do  !ihru_s
            
          end if    !diff > .2
          end if    !soft > 0
          
      end do    !ilum
      end do    !ireg
            
      ! perco adj
      if (isim > 0) then
        cal_sim =  " crop perco adj "
        call calsoft_plant_zero
        call time_control
      end if

      !call time_control
        
      do iterall = 1, iter_all
        ! epco adjustment
        isim = 0
        do ireg = 1, db_mx%plcal_reg
          nvar = plcal(ireg)%lum_num    ! epco is second variable
          do ilum = 1, plcal(ireg)%lum_num
            !! use actual value for epco and not change in value like other parms
            if (iterall == 1) then
              plcal(ireg)%lum(ilum)%prm%epco = pl_prms(ireg)%prm(ilum)%init_val
            end if
            
            soft = plcal(ireg)%lum(ilum)%meas%yield
            diff = 0.
            if (soft > 1.e-6) diff = abs((soft - plcal(ireg)%lum(ilum)%aa%yield) / soft)
            if (diff > .03 .and. plcal(ireg)%lum(ilum)%ha > 1.e-6) then
            isim = 1
            
                plcal(ireg)%lum(ilum)%prm_prev = plcal(ireg)%lum(ilum)%prm
                plcal(ireg)%lum(ilum)%prev = plcal(ireg)%lum(ilum)%aa

                diff = (plcal(ireg)%lum(ilum)%aa%yield - soft) / soft
                !! assume starting at 1.0 - don't see large change until 0.25 so linear decrease from 0.25
                if (diff > 0.1) then
                  chg_val = -.01 * diff + 0.06
                else
                  chg_val = 1.
                  plcal(ireg)%lum(ilum)%prm_lim%epco = 1.
                end if
                !! set upper and lower limits for following iterations
                plcal(ireg)%lum(ilum)%prm_lowlim%epco = 0.
                plcal(ireg)%lum(ilum)%prm_uplim%epco = 1.
                
                plcal(ireg)%lum(ilum)%prm_prev%epco = plcal(ireg)%lum(ilum)%prm%epco
                plcal(ireg)%lum(ilum)%prm%epco = chg_val
                plcal(ireg)%lum(ilum)%prev%yield = plcal(ireg)%lum(ilum)%aa%yield
                
                if (plcal(ireg)%lum(ilum)%prm%epco >= pl_prms(ireg)%prm(ilum+nvar)%pos) then
                  chg_val = pl_prms(ireg)%prm(ilum+nvar)%pos
                  plcal(ireg)%lum(ilum)%prm%epco = pl_prms(ireg)%prm(ilum+nvar)%pos
                  plcal(ireg)%lum(ilum)%prm_lim%epco = 1.
                end if
                if (plcal(ireg)%lum(ilum)%prm%epco <= pl_prms(ireg)%prm(ilum+nvar)%neg) then
                  chg_val = pl_prms(ireg)%prm(ilum+nvar)%neg
                  plcal(ireg)%lum(ilum)%prm%epco = pl_prms(ireg)%prm(ilum+nvar)%neg
                  plcal(ireg)%lum(ilum)%prm_lim%epco = 1.
                end if

            !! re-initialize all objects
            call re_initialize

            do ihru_s = 1, plcal(ireg)%num_tot
              iihru = plcal(ireg)%num(ihru_s)
              do ipl = 1, pcom(iihru)%npl
                if (plcal(ireg)%lum(ilum)%meas%name == pcom(iihru)%pl(ipl)) then
                  !set epco
                  pcom(iihru)%plcur(ipl)%epco = plcal(ireg)%lum(ilum)%prm%epco
                  pcom(iihru)%plcur(ipl)%epco = amin1 (pcom(iihru)%plcur(ipl)%epco, pl_prms(ireg)%prm(ilum+nvar)%up)
                  pcom(iihru)%plcur(ipl)%epco = Max (pcom(iihru)%plcur(ipl)%epco, pl_prms(ireg)%prm(ilum+nvar)%lo)
                  pcom_init(iihru)%plcur(ipl)%epco = pcom(iihru)%plcur(ipl)%epco
                end if
              end do
            end do
            
            end if
          end do
        end do
        ! 1st epco adj
        if (isim > 0) then
          cal_sim =  " 1st epco adj "
          call calsoft_plant_zero
          call time_control
        end if

          ! adjust plant growth using epco
          do ist = 1, 4   !iter_ind
          isim = 0
          do ireg = 1, db_mx%plcal_reg
          nvar = plcal(ireg)%lum_num    ! epco is second variable
          do ilum = 1, plcal(ireg)%lum_num
            soft = plcal(ireg)%lum(ilum)%meas%yield
            diff = 0.
            if (soft > 1.e-6) diff = abs((soft - plcal(ireg)%lum(ilum)%aa%yield) / soft)
            if (diff > .03 .and. plcal(ireg)%lum(ilum)%ha > 1.e-6 .or. plcal(ireg)%lum(ilum)%prm_lim%epco > 1.e-6) then
            if (plcal(ireg)%lum(ilum)%prm_lim%epco < 1.e-6) then
            isim = 1
            
                if (plcal(ireg)%lum(ilum)%aa%yield > soft) then
                  plcal(ireg)%lum(ilum)%prm_uplim%epco = plcal(ireg)%lum(ilum)%prm%epco
                else
                  plcal(ireg)%lum(ilum)%prm_lowlim%epco = plcal(ireg)%lum(ilum)%prm%epco
                end if
                plcal(ireg)%lum(ilum)%prm%epco = .333 * (plcal(ireg)%lum(ilum)%prm_uplim%epco -   &
                    plcal(ireg)%lum(ilum)%prm_lowlim%epco) + plcal(ireg)%lum(ilum)%prm_lowlim%epco

                plcal(ireg)%lum(ilum)%prm_prev%epco = plcal(ireg)%lum(ilum)%prm%epco
                plcal(ireg)%lum(ilum)%prev%yield = plcal(ireg)%lum(ilum)%aa%yield
                                
                if (plcal(ireg)%lum(ilum)%prm%epco >= pl_prms(ireg)%prm(ilum+nvar)%pos) then
                  plcal(ireg)%lum(ilum)%prm%epco = pl_prms(ireg)%prm(ilum+nvar)%pos
                  plcal(ireg)%lum(ilum)%prm_lim%epco = 1.
                end if
                if (plcal(ireg)%lum(ilum)%prm%epco <= pl_prms(ireg)%prm(ilum+nvar)%neg) then
                  plcal(ireg)%lum(ilum)%prm%epco = pl_prms(ireg)%prm(ilum+nvar)%neg
                  plcal(ireg)%lum(ilum)%prm_lim%epco = 1.
                end if
            
            !! re-initialize all objects
            call re_initialize

            do ihru_s = 1, plcal(ireg)%num_tot
              iihru = plcal(ireg)%num(ihru_s)
              do ipl = 1, pcom(iihru)%npl
                if (plcal(ireg)%lum(ilum)%meas%name == pcom(iihru)%pl(ipl)) then
                  !set epco parm
                  pcom(iihru)%plcur(ipl)%epco = plcal(ireg)%lum(ilum)%prm%epco
                  pcom(iihru)%plcur(ipl)%epco = amin1 (pcom(iihru)%plcur(ipl)%epco, pl_prms(ireg)%prm(ilum+nvar)%up)
                  pcom(iihru)%plcur(ipl)%epco = Max (pcom(iihru)%plcur(ipl)%epco, pl_prms(ireg)%prm(ilum+nvar)%lo)
                  pcom_init(iihru)%plcur(ipl)%epco = pcom(iihru)%plcur(ipl)%epco
                end if
              end do
            end do
            
            end if
            end if
            
          end do
          
        end do
        ! plant epco adjustment
        if (isim > 0) then
          cal_sim =  " epco adj "
          call calsoft_plant_zero
          call time_control
        end if
        end do      
                  
        ! pest stress adjustment
        isim = 0
        do ireg = 1, db_mx%plcal_reg
          do ilum = 1, plcal(ireg)%lum_num
            soft = plcal(ireg)%lum(ilum)%meas%yield
            diff = 0.
            if (soft > 1.e-6) diff = abs((soft - plcal(ireg)%lum(ilum)%aa%yield) / soft)
            if (diff > .03 .and. plcal(ireg)%lum(ilum)%ha > 1.e-6) then
            isim = 1
            
                plcal(ireg)%lum(ilum)%prm_prev = plcal(ireg)%lum(ilum)%prm
                plcal(ireg)%lum(ilum)%prev = plcal(ireg)%lum(ilum)%aa

                diff = (plcal(ireg)%lum(ilum)%aa%yield - soft) / soft
                chg_val = diff
                
                plcal(ireg)%lum(ilum)%prm_prev%pest_stress = plcal(ireg)%lum(ilum)%prm%pest_stress
                plcal(ireg)%lum(ilum)%prm%pest_stress = plcal(ireg)%lum(ilum)%prm%pest_stress + chg_val
                plcal(ireg)%lum(ilum)%prev%yield = plcal(ireg)%lum(ilum)%aa%yield
                
                if (plcal(ireg)%lum(ilum)%prm%pest_stress >= pl_prms(ireg)%prm(ilum)%pos) then
                  chg_val = pl_prms(ireg)%prm(ilum)%pos
                  plcal(ireg)%lum(ilum)%prm%pest_stress = pl_prms(ireg)%prm(ilum)%pos
                  plcal(ireg)%lum(ilum)%prm_lim%pest_stress = 1.
                end if
                if (plcal(ireg)%lum(ilum)%prm%pest_stress <= pl_prms(ireg)%prm(ilum)%neg) then
                  chg_val = pl_prms(ireg)%prm(ilum)%neg
                  plcal(ireg)%lum(ilum)%prm%pest_stress = pl_prms(ireg)%prm(ilum)%neg
                  plcal(ireg)%lum(ilum)%prm_lim%pest_stress = 1.
                end if

            !! re-initialize all objects
            call re_initialize

            do ihru_s = 1, plcal(ireg)%num_tot
              iihru = plcal(ireg)%num(ihru_s)
              do ipl = 1, pcom(iihru)%npl
                if (plcal(ireg)%lum(ilum)%meas%name == pcom(iihru)%pl(ipl)) then
                  !set potential lai parm
                  pcom(iihru)%plcur(ipl)%pest_stress = pcom(iihru)%plcur(ipl)%pest_stress + chg_val
                  pcom(iihru)%plcur(ipl)%pest_stress = amin1 (pcom(iihru)%plcur(ipl)%pest_stress, pl_prms(ireg)%prm(ilum)%up)
                  pcom(iihru)%plcur(ipl)%pest_stress = Max (pcom(iihru)%plcur(ipl)%pest_stress, pl_prms(ireg)%prm(ilum)%lo)
                  pcom_init(iihru)%plcur(ipl)%pest_stress = pcom(iihru)%plcur(ipl)%pest_stress
                end if
              end do
            end do
            
            end if
            
          end do
        end do
        ! 1st and only pest stress adj
        if (isim > 0) then
          cal_sim =  " pest stress adj "
          call calsoft_plant_zero
          call time_control
        end if
          

        ! 1st lai potential adjustment
        isim = 0
        do ireg = 1, db_mx%plcal_reg
          nvar = 2 * plcal(ireg)%lum_num    ! lai_pot is second variable
          do ilum = 1, plcal(ireg)%lum_num
            soft = plcal(ireg)%lum(ilum)%meas%yield
            diff = 0.
            if (soft > 1.e-6) diff = abs((soft - plcal(ireg)%lum(ilum)%aa%yield) / soft)
            if (diff > .03 .and. plcal(ireg)%lum(ilum)%ha > 1.e-6) then
            isim = 1
            
                plcal(ireg)%lum(ilum)%prm_prev = plcal(ireg)%lum(ilum)%prm
                plcal(ireg)%lum(ilum)%prev = plcal(ireg)%lum(ilum)%aa

                diff = (soft - plcal(ireg)%lum(ilum)%aa%yield) / soft
                chg_val =  diff * 5.     !assume 1 lai for every 20% difference in yield
                plcal(ireg)%lum(ilum)%prm_prev%lai_pot = plcal(ireg)%lum(ilum)%prm%lai_pot
                plcal(ireg)%lum(ilum)%prm%lai_pot = plcal(ireg)%lum(ilum)%prm%lai_pot + chg_val
                plcal(ireg)%lum(ilum)%prev%yield = plcal(ireg)%lum(ilum)%aa%yield
                
                if (plcal(ireg)%lum(ilum)%prm%lai_pot >= pl_prms(ireg)%prm(ilum+nvar)%pos) then
                  chg_val = pl_prms(ireg)%prm(ilum+nvar)%pos - plcal(ireg)%lum(ilum)%prm_prev%lai_pot
                  plcal(ireg)%lum(ilum)%prm%lai_pot = pl_prms(ireg)%prm(ilum+nvar)%pos
                  plcal(ireg)%lum(ilum)%prm_lim%lai_pot = 1.
                end if
                if (plcal(ireg)%lum(ilum)%prm%lai_pot <= pl_prms(ireg)%prm(ilum+nvar)%neg) then
                  chg_val = pl_prms(ireg)%prm(ilum+nvar)%neg - plcal(ireg)%lum(ilum)%prm_prev%lai_pot
                  plcal(ireg)%lum(ilum)%prm%lai_pot = pl_prms(ireg)%prm(ilum+nvar)%neg
                  plcal(ireg)%lum(ilum)%prm_lim%lai_pot = 1.
                end if

            !! re-initialize all objects
            call re_initialize

            do ihru_s = 1, plcal(ireg)%num_tot
              iihru = plcal(ireg)%num(ihru_s)
              do ipl = 1, pcom(iihru)%npl
                if (plcal(ireg)%lum(ilum)%meas%name == pcom(iihru)%pl(ipl)) then
                  !set potential lai parm
                  pcom(iihru)%plcur(ipl)%lai_pot = pcom(iihru)%plcur(ipl)%lai_pot + chg_val
                  pcom(iihru)%plcur(ipl)%lai_pot = amin1 (pcom(iihru)%plcur(ipl)%lai_pot, pl_prms(ireg)%prm(ilum+nvar)%up)
                  pcom(iihru)%plcur(ipl)%lai_pot = Max (pcom(iihru)%plcur(ipl)%lai_pot, pl_prms(ireg)%prm(ilum+nvar)%lo)
                  pcom_init(iihru)%plcur(ipl)%lai_pot = pcom(iihru)%plcur(ipl)%lai_pot
                end if
              end do
            end do
            
            end if
            
          end do
        end do
        
        ! 1st lai potential adj
        if (isim > 0) then
          cal_sim =  " first lai potential adj "
          call calsoft_plant_zero
          call time_control
        end if

          ! adjust plant growth using potential lai
          do ist = 1, 2    !iter_ind
          isim = 0
          do ireg = 1, db_mx%plcal_reg
          nvar = 2 * plcal(ireg)%lum_num    ! lai_pot is second variable
          do ilum = 1, plcal(ireg)%lum_num
            soft = plcal(ireg)%lum(ilum)%meas%yield
            diff = 0.
            if (soft > 1.e-6) diff = abs((soft - plcal(ireg)%lum(ilum)%aa%yield) / soft)
            if (diff > .03 .and. plcal(ireg)%lum(ilum)%ha > 1.e-6) then
            isim = 1
            
                rmeas = plcal(ireg)%lum(ilum)%meas%yield
                denom = plcal(ireg)%lum(ilum)%prev%yield - plcal(ireg)%lum(ilum)%aa%yield
                if (abs(denom) > 1.e-6) then
                  chg_val = - (plcal(ireg)%lum(ilum)%prm_prev%lai_pot - plcal(ireg)%lum(ilum)%prm%lai_pot) *                &
                              (plcal(ireg)%lum(ilum)%aa%yield - rmeas) / denom
                else
                  chg_val = diff / 5.     !assume 1 lai for every 20% difference in yield
                end if

                diff = (soft - plcal(ireg)%lum(ilum)%aa%yield) / soft
                chg_val =  diff * 5.     !assume 1 lai for every 20% difference in yield
                
                plcal(ireg)%lum(ilum)%prm_prev%lai_pot = plcal(ireg)%lum(ilum)%prm%lai_pot
                plcal(ireg)%lum(ilum)%prm%lai_pot = plcal(ireg)%lum(ilum)%prm%lai_pot + chg_val
                plcal(ireg)%lum(ilum)%prev%yield = plcal(ireg)%lum(ilum)%aa%yield
                                
                if (plcal(ireg)%lum(ilum)%prm%lai_pot >= pl_prms(ireg)%prm(ilum+nvar)%pos) then
                  plcal(ireg)%lum(ilum)%prm%lai_pot = pl_prms(ireg)%prm(ilum+nvar)%pos
                  plcal(ireg)%lum(ilum)%prm_lim%lai_pot = 1.
                end if
                if (plcal(ireg)%lum(ilum)%prm%lai_pot <= pl_prms(ireg)%prm(ilum+nvar)%neg) then
                  plcal(ireg)%lum(ilum)%prm%lai_pot = pl_prms(ireg)%prm(ilum+nvar)%neg
                  plcal(ireg)%lum(ilum)%prm_lim%lai_pot = 1.
                end if
            
            !! re-initialize all objects
            call re_initialize

            do ihru_s = 1, plcal(ireg)%num_tot
              iihru = plcal(ireg)%num(ihru_s)
              do ipl = 1, pcom(iihru)%npl
                if (plcal(ireg)%lum(ilum)%meas%name == pcom(iihru)%pl(ipl)) then
                  !set potential lai parm
                  pcom(iihru)%plcur(ipl)%lai_pot = pcom(iihru)%plcur(ipl)%lai_pot + chg_val
                  pcom(iihru)%plcur(ipl)%lai_pot = amin1 (pcom(iihru)%plcur(ipl)%lai_pot, pl_prms(ireg)%prm(ilum+nvar)%up)
                  pcom(iihru)%plcur(ipl)%lai_pot = Max (pcom(iihru)%plcur(ipl)%lai_pot, pl_prms(ireg)%prm(ilum+nvar)%lo)
                  pcom_init(iihru)%plcur(ipl)%lai_pot = pcom(iihru)%plcur(ipl)%lai_pot
                end if
              end do
            end do
            
            end if
            
          end do
          
        end do
          
        ! plant potential lai adjustment
        if (isim > 0) then
          cal_sim =  " lai potential adj "
          call calsoft_plant_zero
          call time_control
        end if
        end do      ! ist
          
          
        ! 1st plant harvest index adjustment
        isim = 0
        do ireg = 1, db_mx%plcal_reg
          nvar = 3 * plcal(ireg)%lum_num    ! harv_idx is third variable
          do ilum = 1, plcal(ireg)%lum_num
            soft = plcal(ireg)%lum(ilum)%meas%yield
            diff = 0.
            if (soft > 1.e-6) diff = abs((soft - plcal(ireg)%lum(ilum)%aa%yield) / soft)
            if (diff > .03 .and. plcal(ireg)%lum(ilum)%ha > 1.e-6) then
            isim = 1
            
                plcal(ireg)%lum(ilum)%prm_prev = plcal(ireg)%lum(ilum)%prm
                plcal(ireg)%lum(ilum)%prev = plcal(ireg)%lum(ilum)%aa

                diff = (plcal(ireg)%lum(ilum)%meas%yield - plcal(ireg)%lum(ilum)%aa%yield) / plcal(ireg)%lum(ilum)%meas%yield
                chg_val = diff / 2.     !assume frac diff over 4.
                plcal(ireg)%lum(ilum)%prm_prev%harv_idx = plcal(ireg)%lum(ilum)%prm%harv_idx
                plcal(ireg)%lum(ilum)%prm%harv_idx = plcal(ireg)%lum(ilum)%prm%harv_idx + chg_val
                plcal(ireg)%lum(ilum)%prev%yield = plcal(ireg)%lum(ilum)%aa%yield
                
                if (plcal(ireg)%lum(ilum)%prm%harv_idx >= pl_prms(ireg)%prm(ilum+nvar)%pos) then
                  chg_val = pl_prms(ireg)%prm(ilum+nvar)%pos - plcal(ireg)%lum(ilum)%prm_prev%harv_idx
                  plcal(ireg)%lum(ilum)%prm%harv_idx = pl_prms(ireg)%prm(ilum+nvar)%pos
                  plcal(ireg)%lum(ilum)%prm_lim%harv_idx = 1.
                end if
                if (plcal(ireg)%lum(ilum)%prm%harv_idx <= pl_prms(ireg)%prm(ilum+nvar)%neg) then
                  chg_val = pl_prms(ireg)%prm(ilum+nvar)%neg - plcal(ireg)%lum(ilum)%prm_prev%harv_idx
                  plcal(ireg)%lum(ilum)%prm%harv_idx = pl_prms(ireg)%prm(ilum+nvar)%neg
                  plcal(ireg)%lum(ilum)%prm_lim%harv_idx = 1.
                end if

            !! re-initialize all objects
            call re_initialize

            do ihru_s = 1, plcal(ireg)%num_tot
              iihru = plcal(ireg)%num(ihru_s)
              do ipl = 1, pcom(iihru)%npl
                if (plcal(ireg)%lum(ilum)%meas%name == pcom(iihru)%pl(ipl)) then
                  !set harvest index
                  pcom(iihru)%plcur(ipl)%harv_idx = pcom(iihru)%plcur(ipl)%harv_idx + chg_val
                  pcom(iihru)%plcur(ipl)%harv_idx = amin1 (pcom(iihru)%plcur(ipl)%harv_idx, pl_prms(ireg)%prm(ilum+nvar)%up)
                  pcom(iihru)%plcur(ipl)%harv_idx = Max (pcom(iihru)%plcur(ipl)%harv_idx, pl_prms(ireg)%prm(ilum+nvar)%lo)
                  pcom_init(iihru)%plcur(ipl)%harv_idx = pcom(iihru)%plcur(ipl)%harv_idx
                end if
              end do
            end do
            
            end if
            
          end do
        end do
        
        ! 1st plant harvest index adjustment 
        if (isim > 0) then
          cal_sim = " first harvest index adj "
          call calsoft_plant_zero
          call time_control
        end if

          ! adjust plant growth using harvest index parameter
          do ist = 1, 2     !iter_ind
          isim = 0
          do ireg = 1, db_mx%plcal_reg
          nvar = 3 * plcal(ireg)%lum_num    ! harv_idx is third variable
          do ilum = 1, plcal(ireg)%lum_num
            soft = plcal(ireg)%lum(ilum)%meas%yield
            diff = 0.
            if (soft > 1.e-6) diff = abs((soft - plcal(ireg)%lum(ilum)%aa%yield) / soft)
            if (diff > .03 .and. plcal(ireg)%lum(ilum)%ha > 1.e-6) then
            isim = 1
            
                !rmeas = plcal(ireg)%lum(ilum)%meas%yield
                !denom = plcal(ireg)%lum(ilum)%prev%yield - plcal(ireg)%lum(ilum)%aa%yield\
                !if (abs(denom) > 1.e-6) then
                !  chg_val = - (plcal(ireg)%lum(ilum)%prm_prev%harv_idx - plcal(ireg)%lum(ilum)%prm%harv_idx) *                &
                !              (plcal(ireg)%lum(ilum)%aa%yield - rmeas) / denom
                !else
                diff = (plcal(ireg)%lum(ilum)%meas%yield - plcal(ireg)%lum(ilum)%aa%yield) / plcal(ireg)%lum(ilum)%meas%yield
                  chg_val = diff / 2.
                !end if
                plcal(ireg)%lum(ilum)%prm_prev%harv_idx = plcal(ireg)%lum(ilum)%prm%harv_idx
                plcal(ireg)%lum(ilum)%prm%harv_idx = plcal(ireg)%lum(ilum)%prm%harv_idx + chg_val
                plcal(ireg)%lum(ilum)%prev%yield = plcal(ireg)%lum(ilum)%aa%yield
                                
                if (plcal(ireg)%lum(ilum)%prm%harv_idx >= pl_prms(ireg)%prm(ilum+nvar)%pos) then
                  chg_val = pl_prms(ireg)%prm(ilum+nvar)%pos - plcal(ireg)%lum(ilum)%prm_prev%harv_idx
                  plcal(ireg)%lum(ilum)%prm%harv_idx = pl_prms(ireg)%prm(ilum+nvar)%pos
                  plcal(ireg)%lum(ilum)%prm_lim%harv_idx = 1.
                end if
                if (plcal(ireg)%lum(ilum)%prm%harv_idx <= pl_prms(ireg)%prm(ilum+nvar)%neg) then
                  chg_val = pl_prms(ireg)%prm(ilum+nvar)%neg - plcal(ireg)%lum(ilum)%prm_prev%harv_idx
                  plcal(ireg)%lum(ilum)%prm%harv_idx = pl_prms(ireg)%prm(ilum+nvar)%neg
                  plcal(ireg)%lum(ilum)%prm_lim%harv_idx = 1.
                end if

            !! re-initialize all objects
            call re_initialize

            do ihru_s = 1, plcal(ireg)%num_tot
              iihru = plcal(ireg)%num(ihru_s)
              do ipl = 1, pcom(iihru)%npl
                if (plcal(ireg)%lum(ilum)%meas%name == pcom(iihru)%pl(ipl)) then
                  !set potential lai parm
                  pcom(iihru)%plcur(ipl)%harv_idx = pcom(iihru)%plcur(ipl)%harv_idx + chg_val
                  pcom(iihru)%plcur(ipl)%harv_idx = amin1 (pcom(iihru)%plcur(ipl)%harv_idx, pl_prms(ireg)%prm(ilum+nvar)%up)
                  pcom(iihru)%plcur(ipl)%harv_idx = Max (pcom(iihru)%plcur(ipl)%harv_idx, pl_prms(ireg)%prm(ilum+nvar)%lo)
                  pcom_init(iihru)%plcur(ipl)%harv_idx = pcom(iihru)%plcur(ipl)%harv_idx
                end if
              end do
            end do
            
            end if
            
          end do
        end do
          
        ! plant harvest index adjustment
        if (isim > 0) then
          cal_sim = " harvest index adj "
          call calsoft_plant_zero
          call time_control
        end if
        end do      ! ist
          
      end do    ! iter_all loop
	  return
      end subroutine calsoft_plant