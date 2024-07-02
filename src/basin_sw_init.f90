      subroutine basin_sw_init
      
      use time_module
      use hydrograph_module
      use calibration_data_module
      use output_landscape_module
      use basin_module
      use maximum_data_module
      use soil_module
      use hru_module, only : hru
      
      implicit none

      integer :: ihru     !none      |counter
      integer :: iihru    !          |
      integer :: ilsu     !none      |counter
      integer :: ielem    !          |
      real :: const       !          |constant used for rate, days, etc
            
      ! initialize hru soil and snow water
      do ihru = 1, sp_ob%hru
        hwb_d(ihru)%sw_init = soil(ihru)%sw         !store initial soil water
        hwb_m(ihru)%sw_init = soil(ihru)%sw
        hwb_y(ihru)%sw_init = soil(ihru)%sw
        hwb_a(ihru)%sw_init = soil(ihru)%sw
        hwb_d(ihru)%sno_init = hru(ihru)%sno_mm     !store initial snow water
        hwb_m(ihru)%sno_init = hru(ihru)%sno_mm
        hwb_y(ihru)%sno_init = hru(ihru)%sno_mm
        hwb_a(ihru)%sno_init = hru(ihru)%sno_mm
      end do
      
        ! summing subbasin output for the basin
        bwb_d%sw_init = 0.
        bwb_d%sno_init = 0.
        do ihru = 1, sp_ob%hru
          iihru = lsu_elem(ihru)%obtypno
          if (lsu_elem(iihru)%bsn_frac > 1.e-12) then
            const = lsu_elem(iihru)%bsn_frac
            if (lsu_elem(iihru)%obtyp == "hru") then
              bwb_d%sw_init = bwb_d%sw_init + hwb_d(iihru)%sw_init * const
              bwb_d%sno_init = bwb_d%sno_init + hwb_d(iihru)%sno_init * const
            end if
          end if
        end do
        bwb_m%sw_init = bwb_d%sw_init
        bwb_m%sno_init = bwb_d%sno_init
        bwb_y%sw_init = bwb_d%sw_init
        bwb_y%sno_init = bwb_d%sno_init
        bwb_a%sw_init = bwb_d%sw_init
        bwb_a%sno_init = bwb_d%sno_init
          
        ! or if it is not routed and not in a subbasin
        do ihru = 1, sp_ob%hru_lte
          iihru = lsu_elem(ihru)%obtypno
          if (lsu_elem(iihru)%bsn_frac > 1.e-12) then
            const = lsu_elem(iihru)%bsn_frac
            if (lsu_elem(iihru)%obtyp == "hlt") then
              !const = lsu_elem(iihru)%bsn_frac
              bwb_d%sw_init = bwb_d%sw_init + hltwb_d(iihru)%sw_init * const
              bwb_d%sno_init = bwb_d%sno_init + hltwb_d(iihru)%sno_init * const
            end if 
          end if
        end do

      do ilsu = 1, db_mx%lsu_out
        ! summing HRU output for the landscape units
        do ielem = 1, lsu_out(ilsu)%num_tot
          ihru = lsu_out(ilsu)%num(ielem)
          if (lsu_elem(ihru)%ru_frac > 1.e-9) then
            const = lsu_elem(ihru)%ru_frac
            if (lsu_elem(ihru)%obtyp == "hru") then
              ruwb_d(ilsu)%sw_init = ruwb_d(ilsu)%sw_init + hwb_d(ihru)%sw_init * const
              ruwb_d(ilsu)%sno_init = ruwb_d(ilsu)%sno_init + hwb_d(ihru)%sno_init * const
            end if
            ! summing HRU_LTE output
            if (lsu_elem(ihru)%obtyp == "hlt") then
              ruwb_d(ilsu)%sw_init = ruwb_d(ilsu)%sw_init + hltwb_d(ihru)%sw_init * const
              ruwb_d(ilsu)%sno_init = ruwb_d(ilsu)%sno_init + hltwb_d(ihru)%sno_init * const
            end if
          end if
        end do    !ielem
        ruwb_d(ilsu)%sw_init = ruwb_d(ilsu)%sw_init
        ruwb_d(ilsu)%sno_init = ruwb_d(ilsu)%sno_init
        ruwb_m(ilsu)%sw_init = ruwb_d(ilsu)%sw_init
        ruwb_m(ilsu)%sno_init = ruwb_d(ilsu)%sno_init
        ruwb_y(ilsu)%sw_init = ruwb_d(ilsu)%sw_init
        ruwb_y(ilsu)%sno_init = ruwb_d(ilsu)%sno_init
        ruwb_a(ilsu)%sw_init = ruwb_d(ilsu)%sw_init
        ruwb_a(ilsu)%sno_init = ruwb_d(ilsu)%sno_init
      end do      !ilsu
        
      end subroutine basin_sw_init