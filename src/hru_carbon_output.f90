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
      use hru_module, only : hru
      
      implicit none
      
      integer, intent (in) :: ihru             !            |
      integer :: j
      integer :: iob
                         
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine outputs HRU variables on daily, monthly and annual time steps

      j = ihru
      
      iob = sp_ob1%hru + j - 1   !!!!!! added for new output write
          
      hsc_m(j) = hsc_m(j) + hsc_d(j)
      hrc_m(j) = hrc_m(j) + hrc_d(j)
      hpc_m(j) = hpc_m(j) + hpc_d(j) 
      hscf_m(j) = hscf_m(j) + hscf_d(j)
             
      !! daily print
      if (pco%nb_hru%d == "y") then
        write (4520,*) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hsc_d(j)    !! soil carbon gain/loss
        write (4530,*) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hrc_d(j)    !! residue carbon gain/loss
        write (4540,*) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hpc_d(j)    !! plant carbon gain/loss
        write (4550,*) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hscf_d(j)   !! soil transformations

        if (pco%csvout == "y") then
          write (4524,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hsc_d(j)    !! soil carbon gain/loss
          write (4534,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hrc_d(j)    !! residue carbon gain/loss
          write (4544,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hpc_d(j)    !! plant carbon gain/loss
          write (4554,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hscf_d(j)   !! soil transformations

        end if
      end if
         
      !! check end of month
      if (time%end_mo == 1) then
        hsc_y(j) = hsc_y(j) + hsc_m(j)
        hrc_y(j) = hrc_y(j) + hrc_m(j)
        hpc_y(j) = hpc_y(j) + hpc_m(j)
        hscf_y(j) = hscf_y(j) + hscf_m(j)
          
        !! monthly print
        if (pco%nb_hru%m == "y") then
          write (4521,*) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hsc_m(j)    !! soil carbon gain/loss
          write (4531,*) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hrc_m(j)    !! residue carbon gain/loss
          write (4541,*) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hpc_m(j)    !! plant carbon gain/loss
          write (4551,*) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hscf_m(j)     !! soil transformations

          if (pco%csvout == "y") then
            write (4525,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hsc_m(j)    !! soil carbon gain/loss
            write (4535,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hrc_m(j)    !! residue carbon gain/loss
            write (4545,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hpc_m(j)    !! plant carbon gain/loss
            write (4555,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hscf_m(j)   !! soil transformations

          end if
           
          hsc_m(j) = hscz
          hrc_m(j) = hrcz
          hpc_m(j) = hpcz
          hscf_m(j) = hscfz
        end if
      end if      ! if end_mo 
        
      !! check end of year
      if (time%end_yr == 1) then
        hsc_a(j) = hsc_a(j) + hsc_y(j)
        hrc_a(j) = hrc_a(j) + hrc_y(j)
        hpc_a(j) = hpc_a(j) + hpc_y(j)
        hscf_a(j) = hscf_a(j) + hscf_y(j)
        
        !! yearly print
        if (pco%nb_hru%y == "y") then
          write (4522,*) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hsc_y(j)    !! soil carbon gain/loss
          write (4532,*) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hrc_y(j)    !! residue carbon gain/loss
          write (4542,*) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hpc_y(j)    !! residue carbon gain/loss
          write (4552,*) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hscf_y(j)   !! soil transformations

          if (pco%csvout == "y") then
          write (4526,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hsc_y(j)    !! soil carbon gain/loss
          write (4536,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hrc_y(j)    !! residue carbon gain/loss
          write (4546,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hpc_y(j)    !! plant carbon gain/loss
          write (4556,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hscf_y(j)   !! soil transformations

          end if
        end if
      end if      ! if end_yr
        
      !! average annual print
      if (time%end_sim == 1) then
        if (pco%nb_hru%a == "y") then 
          hsc_a(j) = hsc_a(j) / time%yrs_prt 
          hrc_a(j) = hrc_a(j) / time%yrs_prt 
          hpc_a(j) = hpc_a(j) / time%yrs_prt 
          hscf_a(j) = hscf_a(j) / time%yrs_prt
          write (4523,*) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hsc_a(j)    !! soil carbon gain/loss
          write (4533,*) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hrc_a(j)    !! residue carbon gain/loss
          write (4543,*) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hpc_a(j)    !! plant carbon gain/loss
          write (4553,*) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hscf_a(j)   !! soil transformations

          if (pco%csvout == "y") then 
            write (4527,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hsc_a(j)    !! soil carbon gain/loss
            write (4537,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hrc_a(j)    !! residue carbon gain/loss
            write (4547,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hpc_a(j)    !! plant carbon gain/loss
            write (4557,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hscf_a(j)   !! soil transformations

          end if
          hsc_a(j) = hscz
          hrc_a(j) = hrcz 
          hpc_a(j) = hpcz
          hscf_a(j) = hscfz
        end if
      end if     ! if end_sim

      return
      
100   format (4i6,2i8,2x,a,40f12.3)
101   format (4i6,2i8,2x,a,24f12.3)
102   format (4i6,2i8,2x,a,24f12.3)
103   format (4i6,i8,4x,a,5x,4f12.3)
104   format (4i6,2i8,2x,a8,4f12.3,23f17.3)
105   format (4i6,2i8,2x,a8,8f17.3)
106   format (4i6,2i8,2x,a8,29f17.3)
       
      end subroutine hru_carbon_output