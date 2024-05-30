      subroutine calsoft_chsed

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
      
      implicit none

      integer :: ical_sed        !          |
      integer :: iter_all        !          |end of loop
      integer :: iterall         !none      |counter
      integer :: isim            !          |
      integer :: ireg            !none      |counter
      integer :: iord            !none      |counter
      real :: soft               !          |
      real :: diff               !          |
      real :: chg_val            !          |
      integer :: ich_s           !none      |counter
      integer :: iich            !          |
      integer :: icov            !none      |counter
      real :: rmeas              !          |
      real :: denom              !          |
      integer :: iter_ind        !          |end of loop
      integer :: ierod           !none      |counter
      
            
      !calibrate sediment
        ical_sed = 0
        
      !calibrate hydrology
        ical_sed = 0
        iter_all = 1
        iter_ind = 1
        
      do iterall = 1, iter_all
        ! 1st cover adjustment for channel widening
        isim = 0
        do ireg = 1, db_mx%ch_reg
          do iord = 1, chcal(ireg)%ord_num
            soft = chcal(ireg)%ord(iord)%meas%chw
            diff = 0.
            if (soft > 1.e-6) diff = abs((soft - chcal(ireg)%ord(iord)%aa%chw) / soft)
            if (diff > .02 .and. chcal(ireg)%ord(iord)%length > 1.e-6 .and. chcal(ireg)%ord(iord)%prm_lim%erod < 1.e-6) then
            isim = 1
            
                chcal(ireg)%ord(iord)%prm_prev = chcal(ireg)%ord(iord)%prm
                chcal(ireg)%ord(iord)%prev = chcal(ireg)%ord(iord)%aa

                if (soft < chcal(ireg)%ord(iord)%aa%chw) then
                  chg_val = 1. / (abs((soft - chcal(ireg)%ord(iord)%aa%chw) / soft) + 1.05)
                else
                  chg_val = abs((chcal(ireg)%ord(iord)%aa%chw - soft) / chcal(ireg)%ord(iord)%aa%chw) + 1.05
                end if
                chcal(ireg)%ord(iord)%prm_prev%erod = chcal(ireg)%ord(iord)%prm%erod
                chcal(ireg)%ord(iord)%prm%erod = chg_val
                chcal(ireg)%ord(iord)%prev%chw = chcal(ireg)%ord(iord)%aa%chw
                
                if (chcal(ireg)%ord(iord)%prm%erod >= ch_prms(1)%pos) then
                  chg_val = ch_prms(1)%pos
                  chcal(ireg)%ord(iord)%prm%erod = ch_prms(1)%pos
                  chcal(ireg)%ord(iord)%prm_lim%erod = 1.
                end if
                if (chcal(ireg)%ord(iord)%prm%erod <= ch_prms(1)%neg) then
                  chg_val = ch_prms(1)%neg
                  chcal(ireg)%ord(iord)%prm%erod = ch_prms(1)%neg
                  chcal(ireg)%ord(iord)%prm_lim%erod = 1.
                end if

            !check all channels for proper order
            do ich_s = 1, chcal(ireg)%num_tot
              iich = chcal(ireg)%num(ich_s)
              if (chcal(ireg)%ord(iord)%meas%name == sd_ch(iich)%order .or. chcal(ireg)%ord(iord)%meas%name == "basin") then
                !set parms for 1st width calibration and rerun
                sd_ch(iich)%cherod = sd_ch(iich)%cherod * chg_val
                sd_ch(iich)%cherod = amin1 (sd_ch(iich)%cherod, ch_prms(1)%up)
                sd_ch(iich)%cherod = Max (sd_ch(iich)%cherod, ch_prms(1)%lo)
                sdch_init(iich)%cherod = sd_ch(iich)%cherod
              end if
            end do
            chcal(ireg)%ord(iord)%nbyr = 0
            chcal(ireg)%ord(iord)%aa = chcal_z
          end if
          end do
        end do
        
        !! re-initialize all objects
        call re_initialize

        ! 1st cover adjustment 
        if (isim > 0) then
          cal_sim =  " first chan erod adj "
          cal_adj = chg_val
          call time_control
        end if

          ! cover adjustment for channel widening
          do icov = 1, iter_ind
          isim = 0
          do ireg = 1, db_mx%ch_reg
          do iord = 1, chcal(ireg)%ord_num
            soft = chcal(ireg)%ord(iord)%meas%chw
            diff = 0.
            if (soft > 1.e-6) diff = abs((soft - chcal(ireg)%ord(iord)%aa%chw) / soft)
            if (diff > .02 .and. chcal(ireg)%ord(iord)%length > 1.e-6 .and. chcal(ireg)%ord(iord)%prm_lim%erod < 1.e-6) then
            isim = 1

                chcal(ireg)%ord(iord)%prm_prev = chcal(ireg)%ord(iord)%prm
                chcal(ireg)%ord(iord)%prev = chcal(ireg)%ord(iord)%aa

                if (soft < chcal(ireg)%ord(iord)%aa%chw) then
                  chg_val = 1. / (abs((soft - chcal(ireg)%ord(iord)%aa%chw) / soft) + 1.05)
                else
                  chg_val = abs((chcal(ireg)%ord(iord)%aa%chw - soft) / chcal(ireg)%ord(iord)%aa%chw) + 1.05
                end if
                chcal(ireg)%ord(iord)%prm_prev%erod = chcal(ireg)%ord(iord)%prm%erod
                chcal(ireg)%ord(iord)%prm%erod = chcal(ireg)%ord(iord)%prm%erod * chg_val
                chcal(ireg)%ord(iord)%prev%chw = chcal(ireg)%ord(iord)%aa%chw
                                
                if (chcal(ireg)%ord(iord)%prm%erod >= ch_prms(1)%pos) then
                  chg_val = ch_prms(1)%pos
                  chcal(ireg)%ord(iord)%prm%erod = ch_prms(1)%pos
                  chcal(ireg)%ord(iord)%prm_lim%erod = 1.
                end if
                if (chcal(ireg)%ord(iord)%prm%erod <= ch_prms(1)%neg) then
                  chg_val = ch_prms(1)%neg
                  chcal(ireg)%ord(iord)%prm%erod = ch_prms(1)%neg
                  chcal(ireg)%ord(iord)%prm_lim%erod = 1.
                end if
            
            !check all channels for proper order
            do ich_s = 1, chcal(ireg)%num_tot
              iich = chcal(ireg)%num(ich_s)
              if (chcal(ireg)%ord(iord)%meas%name == sd_ch(iich)%order .or. chcal(ireg)%ord(iord)%meas%name == "basin") then
                !set parms for width calibration and rerun
                sd_ch(iich)%cherod = sd_ch(iich)%cherod * chg_val
                sd_ch(iich)%cherod = amin1 (sd_ch(iich)%cherod, ch_prms(1)%up)
                sd_ch(iich)%cherod = Max (sd_ch(iich)%cherod, ch_prms(1)%lo)
                sdch_init(iich)%cherod = sd_ch(iich)%cherod
              end if
            end do
            chcal(ireg)%ord(iord)%nbyr = 0
            chcal(ireg)%ord(iord)%aa = chcal_z
            end if
          end do
        end do
        
        !! re-initialize all objects
        call re_initialize

        ! cover adjustment
        if (isim > 0) then
          cal_sim =  " chan erodibility adj "
          cal_adj = chg_val
          call time_control
        end if
          end do      ! icov

    !! finish - but leave code in case i need to add back      
    go to 777
          
          
        ! 1st bank shear coefficient adjustment for channel widening
        isim = 0
        do ireg = 1, db_mx%ch_reg
          do iord = 1, chcal(ireg)%ord_num
            soft = chcal(ireg)%ord(iord)%meas%chw
            diff = 0.
            if (soft > 1.e-6) diff = abs((soft - chcal(ireg)%ord(iord)%aa%chw) / soft)
            if (diff > .02 .and. chcal(ireg)%ord(iord)%length > 1.e-6 .and. chcal(ireg)%ord(iord)%prm_lim%shear_bnk < 1.e-6) then
            isim = 1
            
                chcal(ireg)%ord(iord)%prm_prev = chcal(ireg)%ord(iord)%prm
                chcal(ireg)%ord(iord)%prev = chcal(ireg)%ord(iord)%aa

                chg_val = chcal(ireg)%ord(iord)%meas%chw / (chcal(ireg)%ord(iord)%aa%chw + 1.e-6)    !assume same ratio of cover and width change
                chcal(ireg)%ord(iord)%prm_prev%shear_bnk = chcal(ireg)%ord(iord)%prm%shear_bnk
                chcal(ireg)%ord(iord)%prm%shear_bnk = chcal(ireg)%ord(iord)%prm%shear_bnk + chg_val
                chcal(ireg)%ord(iord)%prev%chw = chcal(ireg)%ord(iord)%aa%chw
                
                if (chcal(ireg)%ord(iord)%prm%shear_bnk >= ch_prms(3)%pos) then
                  chg_val = ch_prms(3)%pos - chcal(ireg)%ord(iord)%prm_prev%shear_bnk
                  chcal(ireg)%ord(iord)%prm%shear_bnk = ch_prms(3)%pos
                  chcal(ireg)%ord(iord)%prm_lim%shear_bnk = 1.
                end if
                if (chcal(ireg)%ord(iord)%prm%shear_bnk <= ch_prms(3)%neg) then
                  chg_val = ch_prms(3)%neg - chcal(ireg)%ord(iord)%prm_prev%shear_bnk
                  chcal(ireg)%ord(iord)%prm%shear_bnk = ch_prms(3)%neg
                  chcal(ireg)%ord(iord)%prm_lim%shear_bnk = 1.
                end if

            !check all channels for proper order
            do ich_s = 1, chcal(ireg)%num_tot
              iich = chcal(ireg)%num(ich_s)
              if (chcal(ireg)%ord(iord)%meas%name == sd_ch(iich)%order .or. chcal(ireg)%ord(iord)%meas%name == "basin") then
                !set parms for 1st width calibration and rerun
                sdch_init(iich)%shear_bnk = sdch_init(iich)%shear_bnk + chg_val
                sdch_init(iich)%shear_bnk = amin1 (sdch_init(iich)%shear_bnk, ch_prms(3)%up)
                sdch_init(iich)%shear_bnk = Max (sdch_init(iich)%shear_bnk, ch_prms(3)%lo)
              end if
            end do
            chcal(ireg)%ord(iord)%nbyr = 0
            chcal(ireg)%ord(iord)%aa = chcal_z
          end if
          end do
        end do
                
        !! re-initialize all objects
        call re_initialize

        ! 1st bank shear coefficient adjustment 
        if (isim > 0) then
          write (4601,*) " first bank shear coeff adj "
          call time_control
        end if

          ! bank shear coefficient adjustment for channel widening
          do icov = 1, iter_ind
          isim = 0
          do ireg = 1, db_mx%cha_reg
          do iord = 1, chcal(ireg)%ord_num
            soft = chcal(ireg)%ord(iord)%meas%chw
            diff = 0.
            if (soft > 1.e-6) diff = abs((soft - chcal(ireg)%ord(iord)%aa%chw) / soft)
            if (diff > .02 .and. chcal(ireg)%ord(iord)%length > 1.e-6 .and. chcal(ireg)%ord(iord)%prm_lim%shear_bnk < 1.e-6) then
            isim = 1
            
                rmeas = chcal(ireg)%ord(iord)%meas%chw
                denom = chcal(ireg)%ord(iord)%prev%chw - chcal(ireg)%ord(iord)%aa%chw
                if (abs(denom) > 1.e-6) then
                  chg_val = - (chcal(ireg)%ord(iord)%prm_prev%shear_bnk - chcal(ireg)%ord(iord)%prm%shear_bnk)                  &
                    * (chcal(ireg)%ord(iord)%aa%chw - rmeas) / denom
                else
                  chg_val = chcal(ireg)%ord(iord)%meas%chw / chcal(ireg)%ord(iord)%aa%chw
                end if
                chcal(ireg)%ord(iord)%prm_prev%shear_bnk = chcal(ireg)%ord(iord)%prm%shear_bnk
                chcal(ireg)%ord(iord)%prm%shear_bnk = chcal(ireg)%ord(iord)%prm%shear_bnk + chg_val
                chcal(ireg)%ord(iord)%prev%chw = chcal(ireg)%ord(iord)%aa%chw
                                
                if (chcal(ireg)%ord(iord)%prm%shear_bnk >= ch_prms(3)%pos) then
                  chg_val = ch_prms(3)%pos - chcal(ireg)%ord(iord)%prm_prev%shear_bnk
                  chcal(ireg)%ord(iord)%prm%shear_bnk = ch_prms(3)%pos
                  chcal(ireg)%ord(iord)%prm_lim%shear_bnk = 1.
                end if
                if (chcal(ireg)%ord(iord)%prm%shear_bnk <= ch_prms(3)%neg) then
                  chg_val = chcal(ireg)%ord(iord)%prm_prev%shear_bnk - ch_prms(3)%neg
                  chcal(ireg)%ord(iord)%prm%shear_bnk = ch_prms(3)%neg
                  chcal(ireg)%ord(iord)%prm_lim%shear_bnk = 1.
                end if
            
            !check all channels for proper order
            do ich_s = 1, chcal(ireg)%num_tot
              iich = chcal(ireg)%num(ich_s)
              if (chcal(ireg)%ord(iord)%meas%name == sd_ch(iich)%order .or. chcal(ireg)%ord(iord)%meas%name == "basin") then
                !set parms for width calibration and rerun
                sdch_init(iich)%shear_bnk = sdch_init(iich)%shear_bnk + chg_val
                sdch_init(iich)%shear_bnk = amin1 (sdch_init(iich)%shear_bnk, ch_prms(3)%up)
                sdch_init(iich)%shear_bnk = Max (sdch_init(iich)%shear_bnk, ch_prms(3)%lo)
              end if
            end do
            chcal(ireg)%ord(iord)%nbyr = 0
            chcal(ireg)%ord(iord)%aa = chcal_z
            end if
          end do
        end do
                
        !! re-initialize all objects
        call re_initialize

        ! bank shear coefficient adjustment
        if (isim > 0) then
          write (4601,*) " bank shear coeff adj "
          call time_control
        end if
      end do      ! icov

        ! 1st erodibility adjustment for channel downcutting
        isim = 0
        do ireg = 1, db_mx%ch_reg
          do iord = 1, chcal(ireg)%ord_num
            soft = chcal(ireg)%ord(iord)%meas%chd
            diff = 0.
            if (soft > 1.e-6) diff = abs((soft - chcal(ireg)%ord(iord)%aa%chd) / soft)
            if (diff > .02 .and. chcal(ireg)%ord(iord)%length > 1.e-6 .and. chcal(ireg)%ord(iord)%prm_lim%erod < 1.e-6) then
            isim = 1
            
                chcal(ireg)%ord(iord)%prm_prev = chcal(ireg)%ord(iord)%prm
                chcal(ireg)%ord(iord)%prev = chcal(ireg)%ord(iord)%aa

                chg_val = chcal(ireg)%ord(iord)%meas%chd / (chcal(ireg)%ord(iord)%aa%chd + 1.e-6)    !assume same ratio of cover and width change
                chcal(ireg)%ord(iord)%prm_prev%erod = chcal(ireg)%ord(iord)%prm%erod
                chcal(ireg)%ord(iord)%prm%erod = chcal(ireg)%ord(iord)%prm%erod + chg_val
                chcal(ireg)%ord(iord)%prev%chd = chcal(ireg)%ord(iord)%aa%chd
                
                if (chcal(ireg)%ord(iord)%prm%erod >= ch_prms(2)%pos) then
                  chg_val = ch_prms(2)%pos - chcal(ireg)%ord(iord)%prm_prev%erod
                  chcal(ireg)%ord(iord)%prm%erod = ch_prms(2)%pos
                  chcal(ireg)%ord(iord)%prm_lim%erod = 1.
                end if
                if (chcal(ireg)%ord(iord)%prm%erod <= ch_prms(2)%neg) then
                  chg_val = ch_prms(2)%neg - chcal(ireg)%ord(iord)%prm_prev%erod
                  chcal(ireg)%ord(iord)%prm%erod = ch_prms(2)%neg
                  chcal(ireg)%ord(iord)%prm_lim%erod = 1.
                end if

            !check all channels for proper order
            do ich_s = 1, chcal(ireg)%num_tot
              iich = chcal(ireg)%num(ich_s)
              if (chcal(ireg)%ord(iord)%meas%name == sd_ch(iich)%order .or. chcal(ireg)%ord(iord)%meas%name == "basin") then
                !set parms for 1st erodibility calibration and rerun
                sdch_init(iich)%cherod = sdch_init(iich)%cherod / chg_val
                sdch_init(iich)%cherod = amin1 (sdch_init(iich)%cherod, ch_prms(2)%up)
                sdch_init(iich)%cherod = Max (sdch_init(iich)%cherod, ch_prms(2)%lo)
              end if
            end do
            chcal(ireg)%ord(iord)%nbyr = 0
            chcal(ireg)%ord(iord)%aa = chcal_z
          end if
          end do
        end do
                
        !! re-initialize all objects
        call re_initialize

        ! 1st erodibility adjustment 
        if (isim > 0) then
          write (4601,*) " first erodibility adj "
          call time_control
        end if

          ! erodibility adjustment for channel downcutting
          do ierod = 1, iter_ind
          isim = 0
          do ireg = 1, db_mx%cha_reg
          do iord = 1, chcal(ireg)%ord_num
            soft = chcal(ireg)%ord(iord)%meas%chd
            diff = 0.
            if (soft > 1.e-6) diff = abs((soft - chcal(ireg)%ord(iord)%aa%chd) / soft)
            if (diff > .02 .and. chcal(ireg)%ord(iord)%length > 1.e-6 .and. chcal(ireg)%ord(iord)%prm_lim%erod < 1.e-6) then
            isim = 1
            
                rmeas = chcal(ireg)%ord(iord)%meas%chd
                denom = chcal(ireg)%ord(iord)%prev%chd - chcal(ireg)%ord(iord)%aa%chd
                if (abs(denom) > 1.e-6) then
                  chg_val = (chcal(ireg)%ord(iord)%prm_prev%erod - chcal(ireg)%ord(iord)%prm%erod)                  &
                    * (chcal(ireg)%ord(iord)%aa%chd - rmeas) / denom
                else
                  chg_val = chcal(ireg)%ord(iord)%meas%chd / chcal(ireg)%ord(iord)%aa%chd
                end if
                chcal(ireg)%ord(iord)%prm_prev%erod = chcal(ireg)%ord(iord)%prm%erod
                chcal(ireg)%ord(iord)%prm%erod = chcal(ireg)%ord(iord)%prm%erod + chg_val
                chcal(ireg)%ord(iord)%prev%chd = chcal(ireg)%ord(iord)%aa%chd
                                
                if (chcal(ireg)%ord(iord)%prm%erod >= ch_prms(2)%pos) then
                  chg_val = ch_prms(2)%pos - chcal(ireg)%ord(iord)%prm_prev%erod
                  chcal(ireg)%ord(iord)%prm%erod = ch_prms(2)%pos
                  chcal(ireg)%ord(iord)%prm_lim%erod = 1.
                end if
                if (chcal(ireg)%ord(iord)%prm%erod <= ch_prms(2)%neg) then
                  chg_val = chcal(ireg)%ord(iord)%prm_prev%erod - ch_prms(2)%neg
                  chcal(ireg)%ord(iord)%prm%erod = ch_prms(2)%neg
                  chcal(ireg)%ord(iord)%prm_lim%erod = 1.
                end if
            
            !check all channels for proper order
            do ich_s = 1, chcal(ireg)%num_tot
              iich = chcal(ireg)%num(ich_s)
              if (chcal(ireg)%ord(iord)%meas%name == sd_ch(iich)%order .or. chcal(ireg)%ord(iord)%meas%name == "basin") then
                !set parms for depth calibration and rerun
                sdch_init(iich)%cherod = sdch_init(iich)%cherod / chg_val
                sdch_init(iich)%cherod = amin1 (sdch_init(iich)%cherod, ch_prms(2)%up)
                sdch_init(iich)%cherod = Max (sdch_init(iich)%cherod, ch_prms(2)%lo)
              end if
            end do
            chcal(ireg)%ord(iord)%nbyr = 0
            chcal(ireg)%ord(iord)%aa = chcal_z
            end if
          end do
        end do
                
        !! re-initialize all objects
        call re_initialize

        ! erodibility adjustment
        if (isim > 0) then
          write (4601,*) " erodibility adj "
          call time_control
        end if
        end do      ! ierod
        
        ! 1st erodibility adjustment for head cut
        isim = 0
        do ireg = 1, db_mx%ch_reg
          do iord = 1, chcal(ireg)%ord_num
            soft = chcal(ireg)%ord(iord)%meas%hc
            diff = 0.
            if (soft > 1.e-6) diff = abs((soft - chcal(ireg)%ord(iord)%aa%hc) / soft)
            if (diff > .02 .and. chcal(ireg)%ord(iord)%length > 1.e-6 .and. chcal(ireg)%ord(iord)%prm_lim%hc_erod < 1.e-6) then
            isim = 1
            
                chcal(ireg)%ord(iord)%prm_prev = chcal(ireg)%ord(iord)%prm
                chcal(ireg)%ord(iord)%prev = chcal(ireg)%ord(iord)%aa

                chg_val = chcal(ireg)%ord(iord)%meas%hc / (chcal(ireg)%ord(iord)%aa%hc + 1.e-6)     !assume same ratio of cover and width change
                chcal(ireg)%ord(iord)%prm_prev%hc_erod = chcal(ireg)%ord(iord)%prm%hc_erod
                chcal(ireg)%ord(iord)%prm%hc_erod = chcal(ireg)%ord(iord)%prm%hc_erod + chg_val
                chcal(ireg)%ord(iord)%prev%hc = chcal(ireg)%ord(iord)%aa%hc
                
                if (chcal(ireg)%ord(iord)%prm%hc_erod >= ch_prms(4)%pos) then
                  chg_val = ch_prms(4)%pos - chcal(ireg)%ord(iord)%prm_prev%hc_erod
                  chcal(ireg)%ord(iord)%prm%hc_erod = ch_prms(4)%pos
                  chcal(ireg)%ord(iord)%prm_lim%hc_erod = 1.
                end if
                if (chcal(ireg)%ord(iord)%prm%hc_erod <= ch_prms(4)%neg) then
                  chg_val = ch_prms(4)%neg - chcal(ireg)%ord(iord)%prm_prev%hc_erod
                  chcal(ireg)%ord(iord)%prm%hc_erod = ch_prms(4)%neg
                  chcal(ireg)%ord(iord)%prm_lim%hc_erod = 1.
                end if

            !check all channels for proper order
            do ich_s = 1, chcal(ireg)%num_tot
              iich = chcal(ireg)%num(ich_s)
              if (chcal(ireg)%ord(iord)%meas%name == sd_ch(iich)%order .or. chcal(ireg)%ord(iord)%meas%name == "basin") then
                !if height is 0 - no head cut advance
                  sdch_init(iich)%hc_erod = sdch_init(iich)%hc_erod / chg_val
                  sdch_init(iich)%hc_erod = amin1 (sdch_init(iich)%hc_erod, ch_prms(4)%up)
                  sdch_init(iich)%hc_erod = Max (sdch_init(iich)%hc_erod, ch_prms(4)%lo)
                !end if
              end if
            end do
            chcal(ireg)%ord(iord)%nbyr = 0
            chcal(ireg)%ord(iord)%aa = chcal_z
          end if
          end do
        end do
        !initialize hru and hru_lte
                
        !! re-initialize all objects
        call re_initialize

        ! 1st erodibility adjustment 
        if (isim > 0) then
          write (4601,*) " first head cut erodibility adj "
          call time_control
        end if

          ! erodibility adjustment for head cut
          do ierod = 1, iter_ind
          isim = 0
          do ireg = 1, db_mx%cha_reg
          do iord = 1, chcal(ireg)%ord_num
            soft = chcal(ireg)%ord(iord)%meas%hc
            diff = 0.
            if (soft > 1.e-6) diff = abs((soft - chcal(ireg)%ord(iord)%aa%hc) / soft)
            if (diff > .02 .and. chcal(ireg)%ord(iord)%length > 1.e-6 .and. chcal(ireg)%ord(iord)%prm_lim%hc_erod < 1.e-6) then
            isim = 1
            
                rmeas = chcal(ireg)%ord(iord)%meas%hc
                denom = chcal(ireg)%ord(iord)%prev%hc - chcal(ireg)%ord(iord)%aa%hc
                if (abs(denom) > 1.e-6) then
                  chg_val = - (chcal(ireg)%ord(iord)%prm_prev%hc_erod - chcal(ireg)%ord(iord)%prm%hc_erod)                  &
                    * (chcal(ireg)%ord(iord)%aa%hc - rmeas) / denom
                else
                  chg_val = chcal(ireg)%ord(iord)%meas%hc / chcal(ireg)%ord(iord)%aa%hc
                end if
                chcal(ireg)%ord(iord)%prm_prev%hc_erod = chcal(ireg)%ord(iord)%prm%hc_erod
                chcal(ireg)%ord(iord)%prm%hc_erod = chcal(ireg)%ord(iord)%prm%hc_erod + chg_val
                chcal(ireg)%ord(iord)%prev%hc = chcal(ireg)%ord(iord)%aa%hc
                                
                if (chcal(ireg)%ord(iord)%prm%hc_erod >= ch_prms(4)%pos) then
                  chg_val = ch_prms(4)%pos - chcal(ireg)%ord(iord)%prm_prev%hc_erod
                  chcal(ireg)%ord(iord)%prm%hc_erod = ch_prms(4)%pos
                  chcal(ireg)%ord(iord)%prm_lim%hc_erod = 1.
                end if
                if (chcal(ireg)%ord(iord)%prm%hc_erod <= ch_prms(4)%neg) then
                  chg_val = chcal(ireg)%ord(iord)%prm_prev%hc_erod - ch_prms(4)%neg
                  chcal(ireg)%ord(iord)%prm%hc_erod = ch_prms(4)%neg
                  chcal(ireg)%ord(iord)%prm_lim%hc_erod = 1.
                end if
            
            !check all channels for proper order
            do ich_s = 1, chcal(ireg)%num_tot
              iich = chcal(ireg)%num(ich_s)
              if (chcal(ireg)%ord(iord)%meas%name == sd_ch(iich)%order .or. chcal(ireg)%ord(iord)%meas%name == "basin") then
                !if height is 0 - no head cut advance
                if (sd_ch(iich)%hc_hgt > 1.e-6) then
                  sdch_init(iich)%hc_erod = sdch_init(iich)%hc_erod / chg_val
                  sdch_init(iich)%hc_erod = amin1 (sdch_init(iich)%hc_erod, ch_prms(4)%up)
                  sdch_init(iich)%hc_erod = Max (sdch_init(iich)%hc_erod, ch_prms(4)%lo)
                end if
              end if
            end do
            chcal(ireg)%ord(iord)%nbyr = 0
            chcal(ireg)%ord(iord)%aa = chcal_z
            end if
          end do
        end do
                
        !! re-initialize all objects
        call re_initialize

        ! erodibility adjustment
        if (isim > 0) then
          write (4601,*) " head cut erodibility adj "
          call time_control
        end if
        end do      ! ierod

777    end do      ! iter
      
	  return
      end subroutine calsoft_chsed