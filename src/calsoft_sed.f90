      subroutine calsoft_sed

      use hru_module, only : hru, ihru, tconc
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
      
      implicit none
      
      integer :: isim          !          |
      integer :: ireg          !none      |counter
      integer :: ilum          !none      |counter
      integer :: iihru         !none      |counter
      integer :: ihru_s        !none      |counter
      integer :: iter          !none      |counter
      integer :: isl           !none      |counter
      real :: rmeas            !          |
      real :: denom            !          |
      real :: soft             !          |
      real :: diff             !          |
      real :: chg_val          !          | 
      real :: xm               !          |    
      real :: sin_sl           !          | 

      !calibrate sediment

        ! 1st time of concentration adjustment
        isim = 0
        do ireg = 1, db_mx%cha_reg
          do ilum = 1, region(ireg)%nlum
            soft = lscal(ireg)%lum(ilum)%meas%sed
            diff = 0.
            if (soft > 1.e-6) diff = abs((soft - lscal(ireg)%lum(ilum)%aa%sed) / soft)
            if (diff > .1 .and. lscal(ireg)%lum(ilum)%ha > 1.e-6 .and. lscal(ireg)%lum(ilum)%prm_lim%tconc < 1.e-6) then
            isim = 1
            do ihru_s = 1, region(ireg)%num_tot
              iihru = region(ireg)%num(ihru_s)
              if (lscal(ireg)%lum(ilum)%meas%name == hru(ihru)%lum_group_c) then
                !! re-initialize all objects
                call re_initialize

                !set parms for 1st sediment yield calibration and rerun
                lscal(ireg)%lum(ilum)%prm_prev = lscal(ireg)%lum(ilum)%prm
                lscal(ireg)%lum(ilum)%prev = lscal(ireg)%lum(ilum)%aa
                
                chg_val = lscal(ireg)%lum(ilum)%meas%sed / lscal(ireg)%lum(ilum)%aa%sed
                chg_val = chg_val ** 1.7857
                lscal(ireg)%lum(ilum)%prm_prev%tconc = lscal(ireg)%lum(ilum)%prm_prev%tconc
                lscal(ireg)%lum(ilum)%prm_prev%tconc = lscal(ireg)%lum(ilum)%prm_prev%tconc + chg_val
                lscal(ireg)%lum(ilum)%prev%sed = lscal(ireg)%lum(ilum)%aa%sed
                
                if (lscal(ireg)%lum(ilum)%prm_prev%tconc >= ls_prms(1)%pos) then
                  chg_val = ls_prms(6)%pos - lscal(ireg)%lum(ilum)%prm_prev%tconc
                  lscal(ireg)%lum(ilum)%prm_prev%tconc = ls_prms(6)%pos
                  lscal(ireg)%lum(ilum)%prm_lim%tconc = 1.
                end if
                if (lscal(ireg)%lum(ilum)%prm_prev%tconc <= ls_prms(6)%neg) then
                  chg_val = ls_prms(6)%pos - lscal(ireg)%lum(ilum)%prm_prev%tconc
                  lscal(ireg)%lum(ilum)%prm_prev%tconc = ls_prms(6)%neg
                  lscal(ireg)%lum(ilum)%prm_lim%tconc = 1.
                end if

                tconc(iihru) = tconc(iihru) / chg_val
                tconc(iihru) = amin1 (tconc(iihru), 1400.)
                tconc(iihru) = Max (tconc(iihru), 0.)
              end if
            end do
            lscal(ireg)%lum(ilum)%nbyr = 0
            lscal(ireg)%lum(ilum)%precip_aa = 0.
            lscal(ireg)%lum(ilum)%aa = lscal_z
          end if
          end do
        end do
        ! 1st tconc adjustment 
        if (isim > 0) call time_control
        
        do iter = 1, 2
          ! additional adjust sediment using tconc
          do isl = 1, 3
          do ireg = 1, db_mx%cha_reg
          do ilum = 1, region(ireg)%nlum
            do ihru_s = 1, region(ireg)%num_tot
              iihru = region(ireg)%num(ihru_s)
              if (lscal(ireg)%lum(ilum)%meas%name == hru(ihru)%lum_group_c) then
                !! re-initialize all objects
                call re_initialize

                !set parms for 1st sediment yield calibration and rerun
                lscal(ireg)%lum(ilum)%prm_prev = lscal(ireg)%lum(ilum)%prm
                lscal(ireg)%lum(ilum)%prev = lscal(ireg)%lum(ilum)%aa
                
                rmeas = lscal(ireg)%lum(ilum)%meas%sed
                chg_val = - (lscal(ireg)%lum(ilum)%prm_prev%tconc - lscal(ireg)%lum(ilum)%prm_prev%tconc)                  &
                            * (lscal(ireg)%lum(ilum)%aa%sed - rmeas) / (lscal(ireg)%lum(ilum)%prev%sed - rmeas)
                chg_val = amin1 (chg_val, ls_prms(6)%pos)
                chg_val = Max (chg_val, ls_prms(6)%neg)
                lscal(ireg)%lum(ilum)%prm%tconc = chg_val
                if (chg_val > .001) then
                tconc(iihru) = tconc(iihru) / chg_val
                tconc(iihru) = amin1 (tconc(iihru), 1400.)
                tconc(iihru) = Max (tconc(iihru), 0.)
                end if
              end if
            end do
            lscal(ireg)%lum(ilum)%nbyr = 0
            lscal(ireg)%lum(ilum)%precip_aa = 0.
            lscal(ireg)%lum(ilum)%aa = lscal_z
          end do
        end do
        ! tc adjustment 
        call time_control
        end do      ! tc
          
        ! 1st slope adjustment
        do ireg = 1, db_mx%cha_reg
          do ilum = 1, region(ireg)%nlum
              !check all hru"s for proper lum
              do iihru = 1, sp_ob%hru
                !set parms for 1st slope calibration and rerun
                if (lscal(ireg)%lum(ilum)%meas%name == hru(ihru)%lum_group_c) then
                  !! re-initialize all objects
                  call re_initialize

                  !set parms for 1st sediment yield calibration and rerun
                  lscal(ireg)%lum(ilum)%prm_prev = lscal(ireg)%lum(ilum)%prm
                  lscal(ireg)%lum(ilum)%prev = lscal(ireg)%lum(ilum)%aa
 
                  denom = lscal(ireg)%lum(ilum)%prev%srr - lscal(ireg)%lum(ilum)%aa%srr
                  if (abs(denom) > 1.e-6) then
                    chg_val = lscal(ireg)%lum(ilum)%meas%sed / lscal(ireg)%lum(ilum)%aa%sed
                  else
                    chg_val = diff / 200.
                  end if
                  
                  chg_val = amin1 (chg_val, ls_prms(5)%pos)
                  chg_val = Max (chg_val, ls_prms(5)%neg)
                  lscal(ireg)%lum(ilum)%prm%slope = chg_val
                  
                  hru(iihru)%topo%slope = hru(iihru)%topo%slope - chg_val
                  hru(iihru)%topo%slope = amin1 (hru(iihru)%topo%slope, 2.)
                  hru(iihru)%topo%slope = Max (hru(iihru)%topo%slope, .0000001)
                  xm = 0.6 * (1. - Exp(-35.835 * hru(iihru)%topo%slope))    
                  sin_sl = Sin(Atan(hru(iihru)%topo%slope))
                  hru(iihru)%lumv%usle_ls = (hru(iihru)%topo%slope / 22.128) ** xm * (65.41 * sin_sl * sin_sl + 4.56 * sin_sl +.065)
                  hru(iihru)%lumv%usle_mult = soil(iihru)%phys(1)%rock * soil(iihru)%usle_k * hru(iihru)%lumv%usle_p * &
                    hru(iihru)%lumv%usle_ls * 11.8
                end if
              end do
            lscal(ireg)%lum(ilum)%nbyr = 0
            lscal(ireg)%lum(ilum)%precip_aa = 0.
            lscal(ireg)%lum(ilum)%aa = lscal_z
          end do
        end do
        ! 1st tc adjustment 
        call time_control
        
        ! adjust sediment using slope and slope length
        do isl = 1, 2
          do ireg = 1, db_mx%cha_reg
          do ilum = 1, region(ireg)%nlum
              !check all hru"s for proper lum
              do iihru = 1, sp_ob%hru
                !set parms for 1st slope calibration and rerun
                if (lscal(ireg)%lum(ilum)%meas%name == hru(ihru)%lum_group_c) then
                  !! re-initialize all objects
                  call re_initialize

                  !set parms for 1st sediment yield calibration and rerun
                  lscal(ireg)%lum(ilum)%prm_prev = lscal(ireg)%lum(ilum)%prm
                  lscal(ireg)%lum(ilum)%prev = lscal(ireg)%lum(ilum)%aa
                
                  rmeas = lscal(ireg)%lum(ilum)%meas%sed
                  chg_val = - (lscal(ireg)%lum(ilum)%prm_prev%slope - lscal(ireg)%lum(ilum)%prm_prev%slope)                  &
                            * (lscal(ireg)%lum(ilum)%aa%sed - rmeas) / (lscal(ireg)%lum(ilum)%prev%sed - rmeas)
                  chg_val = amin1 (chg_val, ls_prms(5)%pos)
                  chg_val = Max (chg_val, ls_prms(5)%neg)
                  lscal(ireg)%lum(ilum)%prm%slope = chg_val
                  
                  hru(iihru)%topo%slope = hru(iihru)%topo%slope - chg_val
                  hru(iihru)%topo%slope = amin1 (hru(iihru)%topo%slope, 2.)
                  hru(iihru)%topo%slope = Max (hru(iihru)%topo%slope, .0000001)
                  xm = 0.6 * (1. - Exp(-35.835 * hru(iihru)%topo%slope))    
                  sin_sl = Sin(Atan(hru(iihru)%topo%slope))
                  hru(iihru)%lumv%usle_ls = (hru(iihru)%topo%slope / 22.128) ** xm * (65.41 * sin_sl * sin_sl + 4.56 * sin_sl +.065)
                  hru(iihru)%lumv%usle_mult = soil(iihru)%phys(1)%rock * soil(iihru)%usle_k * hru(iihru)%lumv%usle_p * &
                    hru(iihru)%lumv%usle_ls * 11.8
                end if
              end do
            lscal(ireg)%lum(ilum)%nbyr = 0
            lscal(ireg)%lum(ilum)%precip_aa = 0.
            lscal(ireg)%lum(ilum)%aa = lscal_z
          end do
          end do
          ! slope adjustment 
          call time_control
          ! if within uncertainty limits (in each lum) - go on to next variable
        
        end do      ! isl
        end do      ! iter
      
	  return
      end subroutine calsoft_sed