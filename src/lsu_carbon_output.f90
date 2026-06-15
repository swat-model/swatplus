      subroutine lsu_carbon_output

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    area-weighted LSU aggregations of three HRU-level carbon families:
!!      lsu_carb_gl_*    (gain/loss; soil + residue + plant flux)
!!      lsu_scf_*        (HRU C transformations)
!!      lsu_plc_stat_*   (plant carbon state, snapshot per timestep)
!!    Per-layer carbon families are NOT aggregated (different HRUs have different soil profiles).
!!    Called once per day from command.f90 AFTER the HRU loop has populated hsc_d / hrc_d / hpc_d / hscf_d.

      use time_module
      use basin_module
      use maximum_data_module
      use calibration_data_module
      use hydrograph_module
      use carbon_module
      use plant_module
      use organic_mineral_mass_module
      use output_landscape_module

      implicit none

      integer :: ilsu = 0
      integer :: ielem = 0
      integer :: ihru = 0
      integer :: iob = 0
      real :: const = 0.
      real :: lsu_plt_c = 0.

      if (db_mx%lsu_out <= 0) return

      !! zero daily accumulators
      do ilsu = 1, db_mx%lsu_out
        lsc_d(ilsu) = hscz
        lrc_d(ilsu) = hrcz
        lpc_d(ilsu) = hpcz
        lscf_d(ilsu) = hscfz
      end do

      !! per-LSU aggregation and print
      do ilsu = 1, db_mx%lsu_out
        iob = sp_ob1%ru + ilsu - 1
        lsu_plt_c = 0.

        !! sum HRU contributions weighted by ru_frac
        do ielem = 1, lsu_out(ilsu)%num_tot
          ihru = lsu_out(ilsu)%num(ielem)
          if (lsu_elem(ihru)%ru_frac > 1.e-9 .and. lsu_elem(ihru)%obtyp == "hru") then
            const = lsu_elem(ihru)%ru_frac
            lsc_d(ilsu) = lsc_d(ilsu) + hsc_d(ihru) * const
            lrc_d(ilsu) = lrc_d(ilsu) + hrc_d(ihru) * const
            lpc_d(ilsu) = lpc_d(ilsu) + hpc_d(ihru) * const
            lscf_d(ilsu) = lscf_d(ilsu) + hscf_d(ihru) * const
            lsu_plt_c = lsu_plt_c + pl_mass(ihru)%tot_com%c * const
          end if
        end do

        !! roll daily into monthly
        lsc_m(ilsu) = lsc_m(ilsu) + lsc_d(ilsu)
        lrc_m(ilsu) = lrc_m(ilsu) + lrc_d(ilsu)
        lpc_m(ilsu) = lpc_m(ilsu) + lpc_d(ilsu)
        lscf_m(ilsu) = lscf_m(ilsu) + lscf_d(ilsu)

        !! daily print
        if (pco%cb_gl_lsu%d == "y") then
          write (4750,*) time%day, time%mo, time%day_mo, time%yrc, ilsu, ob(iob)%gis_id, ob(iob)%name, lsc_d(ilsu), lrc_d(ilsu), lpc_d(ilsu)
          if (pco%csvout == "y") write (4754,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, ilsu, ob(iob)%gis_id, ob(iob)%name, lsc_d(ilsu), lrc_d(ilsu), lpc_d(ilsu)
        end if
        if (pco%cb_trf_lsu%d == "y") then
          write (4758,*) time%day, time%mo, time%day_mo, time%yrc, ilsu, ob(iob)%gis_id, ob(iob)%name, lscf_d(ilsu)
          if (pco%csvout == "y") write (4762,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, ilsu, ob(iob)%gis_id, ob(iob)%name, lscf_d(ilsu)
        end if
        if (pco%cb_plt_lsu%d == "y") then
          write (4766,*) time%day, time%mo, time%day_mo, time%yrc, ilsu, ob(iob)%gis_id, ob(iob)%name, lsu_plt_c
          if (pco%csvout == "y") write (4770,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, ilsu, ob(iob)%gis_id, ob(iob)%name, lsu_plt_c
        end if

        !! end of month
        if (time%end_mo == 1) then
          lsc_y(ilsu) = lsc_y(ilsu) + lsc_m(ilsu)
          lrc_y(ilsu) = lrc_y(ilsu) + lrc_m(ilsu)
          lpc_y(ilsu) = lpc_y(ilsu) + lpc_m(ilsu)
          lscf_y(ilsu) = lscf_y(ilsu) + lscf_m(ilsu)

          if (pco%cb_gl_lsu%m == "y") then
            write (4751,*) time%day, time%mo, time%day_mo, time%yrc, ilsu, ob(iob)%gis_id, ob(iob)%name, lsc_m(ilsu), lrc_m(ilsu), lpc_m(ilsu)
            if (pco%csvout == "y") write (4755,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, ilsu, ob(iob)%gis_id, ob(iob)%name, lsc_m(ilsu), lrc_m(ilsu), lpc_m(ilsu)
          end if
          if (pco%cb_trf_lsu%m == "y") then
            write (4759,*) time%day, time%mo, time%day_mo, time%yrc, ilsu, ob(iob)%gis_id, ob(iob)%name, lscf_m(ilsu)
            if (pco%csvout == "y") write (4763,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, ilsu, ob(iob)%gis_id, ob(iob)%name, lscf_m(ilsu)
          end if
          if (pco%cb_plt_lsu%m == "y") then
            write (4767,*) time%day, time%mo, time%day_mo, time%yrc, ilsu, ob(iob)%gis_id, ob(iob)%name, lsu_plt_c
            if (pco%csvout == "y") write (4771,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, ilsu, ob(iob)%gis_id, ob(iob)%name, lsu_plt_c
          end if

          lsc_m(ilsu) = hscz
          lrc_m(ilsu) = hrcz
          lpc_m(ilsu) = hpcz
          lscf_m(ilsu) = hscfz
        end if

        !! end of year
        if (time%end_yr == 1) then
          lsc_a(ilsu) = lsc_a(ilsu) + lsc_y(ilsu)
          lrc_a(ilsu) = lrc_a(ilsu) + lrc_y(ilsu)
          lpc_a(ilsu) = lpc_a(ilsu) + lpc_y(ilsu)
          lscf_a(ilsu) = lscf_a(ilsu) + lscf_y(ilsu)

          if (pco%cb_gl_lsu%y == "y") then
            write (4752,*) time%day, time%mo, time%day_mo, time%yrc, ilsu, ob(iob)%gis_id, ob(iob)%name, lsc_y(ilsu), lrc_y(ilsu), lpc_y(ilsu)
            if (pco%csvout == "y") write (4756,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, ilsu, ob(iob)%gis_id, ob(iob)%name, lsc_y(ilsu), lrc_y(ilsu), lpc_y(ilsu)
          end if
          if (pco%cb_trf_lsu%y == "y") then
            write (4760,*) time%day, time%mo, time%day_mo, time%yrc, ilsu, ob(iob)%gis_id, ob(iob)%name, lscf_y(ilsu)
            if (pco%csvout == "y") write (4764,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, ilsu, ob(iob)%gis_id, ob(iob)%name, lscf_y(ilsu)
          end if
          if (pco%cb_plt_lsu%y == "y") then
            write (4768,*) time%day, time%mo, time%day_mo, time%yrc, ilsu, ob(iob)%gis_id, ob(iob)%name, lsu_plt_c
            if (pco%csvout == "y") write (4772,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, ilsu, ob(iob)%gis_id, ob(iob)%name, lsu_plt_c
          end if

          lsc_y(ilsu) = hscz
          lrc_y(ilsu) = hrcz
          lpc_y(ilsu) = hpcz
          lscf_y(ilsu) = hscfz
        end if

        !! average annual
        if (time%end_sim == 1) then
          if (pco%cb_gl_lsu%a == "y" .or. pco%cb_trf_lsu%a == "y" .or. pco%cb_plt_lsu%a == "y") then
            lsc_a(ilsu) = lsc_a(ilsu) / time%yrs_prt
            lrc_a(ilsu) = lrc_a(ilsu) / time%yrs_prt
            lpc_a(ilsu) = lpc_a(ilsu) / time%yrs_prt
            lscf_a(ilsu) = lscf_a(ilsu) / time%yrs_prt
            if (pco%cb_gl_lsu%a == "y") then
              write (4753,*) time%day, time%mo, time%day_mo, time%yrc, ilsu, ob(iob)%gis_id, ob(iob)%name, lsc_a(ilsu), lrc_a(ilsu), lpc_a(ilsu)
              if (pco%csvout == "y") write (4757,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, ilsu, ob(iob)%gis_id, ob(iob)%name, lsc_a(ilsu), lrc_a(ilsu), lpc_a(ilsu)
            end if
            if (pco%cb_trf_lsu%a == "y") then
              write (4761,*) time%day, time%mo, time%day_mo, time%yrc, ilsu, ob(iob)%gis_id, ob(iob)%name, lscf_a(ilsu)
              if (pco%csvout == "y") write (4765,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, ilsu, ob(iob)%gis_id, ob(iob)%name, lscf_a(ilsu)
            end if
            if (pco%cb_plt_lsu%a == "y") then
              write (4769,*) time%day, time%mo, time%day_mo, time%yrc, ilsu, ob(iob)%gis_id, ob(iob)%name, lsu_plt_c
              if (pco%csvout == "y") write (4773,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, ilsu, ob(iob)%gis_id, ob(iob)%name, lsu_plt_c
            end if
          end if
        end if

      end do  ! ilsu

      return
      end subroutine lsu_carbon_output
