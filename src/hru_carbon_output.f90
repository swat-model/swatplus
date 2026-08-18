      subroutine hru_carbon_output (ihru)

      use plant_module
      use plant_data_module
      use time_module
      use basin_module
      use output_landscape_module
      use hydrograph_module, only : sp_ob1, ob
      use organic_mineral_mass_module
      use soil_module
      use carbon_module
      
      implicit none
      
      integer, intent (in) :: ihru             !            |
      integer :: j = 0
      integer :: iob = 0
                         
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine outputs HRU variables on daily, monthly and annual time steps

      j = ihru
      
      iob = sp_ob1%hru + j - 1   !!!!!! added for new output write
          
      hsc_m(j) = hsc_m(j) + hsc_d(j)
      hrc_m(j) = hrc_m(j) + hrc_d(j)
      hpc_m(j) = hpc_m(j) + hpc_d(j) 
      hscf_m(j) = hscf_m(j) + hscf_d(j)
             
      !! hru_carb_gl gated by pco%cb_gl_hru; hru_scf gated by pco%cb_trf_hru.
      !! daily print
      if (pco%cb_gl_hru%d == "y") then
        write (4520,*) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hsc_d(j), hrc_d(j), hpc_d(j)
        if (pco%csvout == "y") write (4524,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hsc_d(j), hrc_d(j), hpc_d(j)
      end if
      if (pco%cb_trf_hru%d == "y") then
        write (4550,*) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hscf_d(j)
        if (pco%csvout == "y") write (4554,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hscf_d(j)
      end if

      !! check end of month
      if (time%end_mo == 1) then
        hsc_y(j) = hsc_y(j) + hsc_m(j)
        hrc_y(j) = hrc_y(j) + hrc_m(j)
        hpc_y(j) = hpc_y(j) + hpc_m(j)
        hscf_y(j) = hscf_y(j) + hscf_m(j)

        if (pco%cb_gl_hru%m == "y") then
          write (4521,*) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hsc_m(j), hrc_m(j), hpc_m(j)
          if (pco%csvout == "y") write (4525,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hsc_m(j), hrc_m(j), hpc_m(j)
        end if
        if (pco%cb_trf_hru%m == "y") then
          write (4551,*) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hscf_m(j)
          if (pco%csvout == "y") write (4555,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hscf_m(j)
        end if
        hsc_m(j) = hscz
        hrc_m(j) = hrcz
        hpc_m(j) = hpcz
        hscf_m(j) = hscfz
      end if      ! if end_mo

      !! check end of year
      if (time%end_yr == 1) then
        hsc_a(j) = hsc_a(j) + hsc_y(j)
        hrc_a(j) = hrc_a(j) + hrc_y(j)
        hpc_a(j) = hpc_a(j) + hpc_y(j)
        hscf_a(j) = hscf_a(j) + hscf_y(j)

        if (pco%cb_gl_hru%y == "y") then
          write (4522,*) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hsc_y(j), hrc_y(j), hpc_y(j)
          if (pco%csvout == "y") write (4526,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hsc_y(j), hrc_y(j), hpc_y(j)
        end if
        if (pco%cb_trf_hru%y == "y") then
          write (4552,*) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hscf_y(j)
          if (pco%csvout == "y") write (4556,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hscf_y(j)
        end if
        hsc_y(j) = hscz
        hrc_y(j) = hrcz
        hpc_y(j) = hpcz
        hscf_y(j) = hscfz
      end if      ! if end_yr

      !! average annual print
      if (time%end_sim == 1) then
        if (pco%cb_gl_hru%a == "y" .or. pco%cb_trf_hru%a == "y") then
          !! divide accumulators by years once
          hsc_a(j) = hsc_a(j) / time%yrs_prt
          hrc_a(j) = hrc_a(j) / time%yrs_prt
          hpc_a(j) = hpc_a(j) / time%yrs_prt
          hscf_a(j) = hscf_a(j) / time%yrs_prt
          if (pco%cb_gl_hru%a == "y") then
            write (4523,*) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hsc_a(j), hrc_a(j), hpc_a(j)
            if (pco%csvout == "y") write (4527,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hsc_a(j), hrc_a(j), hpc_a(j)
          end if
          if (pco%cb_trf_hru%a == "y") then
            write (4553,*) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hscf_a(j)
            if (pco%csvout == "y") write (4557,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hscf_a(j)
          end if
          hsc_a(j) = hscz
          hrc_a(j) = hrcz
          hpc_a(j) = hpcz
          hscf_a(j) = hscfz
        end if
      end if     ! if end_sim

      return
      
!*** tu Wunused-label: 100   format (4i6,2i8,2x,a,40f12.3)
!*** tu Wunused-label: 101   format (4i6,2i8,2x,a,24f12.3)
!*** tu Wunused-label: 102   format (4i6,2i8,2x,a,24f12.3)
!*** tu Wunused-label: 103   format (4i6,i8,4x,a,5x,4f12.3)
!*** tu Wunused-label: 104   format (4i6,2i8,2x,a8,4f12.3,23f17.3)
!*** tu Wunused-label: 105   format (4i6,2i8,2x,a8,8f17.3)
!*** tu Wunused-label: 106   format (4i6,2i8,2x,a8,29f17.3)
       
      end subroutine hru_carbon_output