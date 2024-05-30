      subroutine res_cs_output(j) !rtb cs
    
      use output_ls_pesticide_module
      use res_pesticide_module
      use res_cs_module
      use plant_module
      use plant_data_module
      use time_module
      use basin_module
      use output_landscape_module
      use constituent_mass_module
      use hydrograph_module, only : sp_ob1, ob
      
      implicit none
      
      integer :: ics                          !            |
      integer :: j
      integer :: iob
      real :: const
                         
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine outputs constituent mass in reservoirs

      iob = sp_ob1%res + j - 1
          
      !add daily values to monthly values
      do ics = 1, cs_db%num_cs
        rescs_m(j)%cs(ics)%inflow = rescs_m(j)%cs(ics)%inflow + rescs_d(j)%cs(ics)%inflow
        rescs_m(j)%cs(ics)%outflow = rescs_m(j)%cs(ics)%outflow + rescs_d(j)%cs(ics)%outflow
        rescs_m(j)%cs(ics)%seep = rescs_m(j)%cs(ics)%seep + rescs_d(j)%cs(ics)%seep
        rescs_m(j)%cs(ics)%settle = rescs_m(j)%cs(ics)%settle + rescs_d(j)%cs(ics)%settle
        rescs_m(j)%cs(ics)%rctn = rescs_m(j)%cs(ics)%rctn + rescs_d(j)%cs(ics)%rctn
        rescs_m(j)%cs(ics)%prod = rescs_m(j)%cs(ics)%prod + rescs_d(j)%cs(ics)%prod
        rescs_m(j)%cs(ics)%fert = rescs_m(j)%cs(ics)%fert + rescs_d(j)%cs(ics)%fert
        rescs_m(j)%cs(ics)%irrig = rescs_m(j)%cs(ics)%irrig + rescs_d(j)%cs(ics)%irrig
        rescs_m(j)%cs(ics)%div = rescs_m(j)%cs(ics)%div + rescs_d(j)%cs(ics)%div
        rescs_m(j)%cs(ics)%mass = rescs_m(j)%cs(ics)%mass + rescs_d(j)%cs(ics)%mass
        rescs_m(j)%cs(ics)%conc = rescs_m(j)%cs(ics)%conc + rescs_d(j)%cs(ics)%conc
      enddo
      rescs_m(j)%cs(1)%volm = rescs_m(j)%cs(1)%volm + rescs_d(j)%cs(1)%volm

      !daily print
      if (pco%cs_res%d == "y") then
        write (6040,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, & 
                         (rescs_d(j)%cs(ics)%inflow,ics=1,cs_db%num_cs), &
                         (rescs_d(j)%cs(ics)%outflow,ics=1,cs_db%num_cs), &
                         (rescs_d(j)%cs(ics)%seep,ics=1,cs_db%num_cs), &
                         (rescs_d(j)%cs(ics)%settle,ics=1,cs_db%num_cs), &
                         (rescs_d(j)%cs(ics)%rctn,ics=1,cs_db%num_cs), &
                         (rescs_d(j)%cs(ics)%prod,ics=1,cs_db%num_cs), &
                         (rescs_d(j)%cs(ics)%fert,ics=1,cs_db%num_cs), &
                         (rescs_d(j)%cs(ics)%irrig,ics=1,cs_db%num_cs), &
                         (rescs_d(j)%cs(ics)%div,ics=1,cs_db%num_cs), &
                         (rescs_d(j)%cs(ics)%mass,ics=1,cs_db%num_cs), &
                         (rescs_d(j)%cs(ics)%conc,ics=1,cs_db%num_cs), &
                          rescs_d(j)%cs(1)%volm
        if (pco%csvout == "y") then
          write (6041,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, & 
                                        (rescs_d(j)%cs(ics)%inflow,ics=1,cs_db%num_cs), &
                                        (rescs_d(j)%cs(ics)%outflow,ics=1,cs_db%num_cs), &
                                        (rescs_d(j)%cs(ics)%seep,ics=1,cs_db%num_cs), &
                                        (rescs_d(j)%cs(ics)%settle,ics=1,cs_db%num_cs), &
                                        (rescs_d(j)%cs(ics)%rctn,ics=1,cs_db%num_cs), &
                                        (rescs_d(j)%cs(ics)%prod,ics=1,cs_db%num_cs), &
                                        (rescs_d(j)%cs(ics)%fert,ics=1,cs_db%num_cs), &
                                        (rescs_d(j)%cs(ics)%irrig,ics=1,cs_db%num_cs), &
                                        (rescs_d(j)%cs(ics)%div,ics=1,cs_db%num_cs), &
                                        (rescs_d(j)%cs(ics)%mass,ics=1,cs_db%num_cs), &
                                        (rescs_d(j)%cs(ics)%conc,ics=1,cs_db%num_cs), &
                                         rescs_d(j)%cs(1)%volm
        endif
      endif

      !monthly print
      if (time%end_mo == 1) then
        !add monthly values to yearly values
        do ics = 1, cs_db%num_cs
          rescs_y(j)%cs(ics)%inflow = rescs_y(j)%cs(ics)%inflow + rescs_m(j)%cs(ics)%inflow
          rescs_y(j)%cs(ics)%outflow = rescs_y(j)%cs(ics)%outflow + rescs_m(j)%cs(ics)%outflow
          rescs_y(j)%cs(ics)%seep = rescs_y(j)%cs(ics)%seep + rescs_m(j)%cs(ics)%seep
          rescs_y(j)%cs(ics)%settle = rescs_y(j)%cs(ics)%settle + rescs_m(j)%cs(ics)%settle
          rescs_y(j)%cs(ics)%rctn = rescs_y(j)%cs(ics)%rctn + rescs_m(j)%cs(ics)%rctn
          rescs_y(j)%cs(ics)%prod = rescs_y(j)%cs(ics)%prod + rescs_m(j)%cs(ics)%prod
          rescs_y(j)%cs(ics)%fert = rescs_y(j)%cs(ics)%fert + rescs_m(j)%cs(ics)%fert
          rescs_y(j)%cs(ics)%irrig = rescs_y(j)%cs(ics)%irrig + rescs_m(j)%cs(ics)%irrig
          rescs_y(j)%cs(ics)%div = rescs_y(j)%cs(ics)%div + rescs_m(j)%cs(ics)%div
          rescs_y(j)%cs(ics)%mass = rescs_y(j)%cs(ics)%mass + rescs_m(j)%cs(ics)%mass
          rescs_y(j)%cs(ics)%conc = rescs_y(j)%cs(ics)%conc + rescs_m(j)%cs(ics)%conc
        enddo
        rescs_y(j)%cs(1)%volm = rescs_y(j)%cs(1)%volm + rescs_m(j)%cs(1)%volm
        const = float (ndays(time%mo + 1) - ndays(time%mo))
        do ics=1,cs_db%num_cs !average mass and concentration
          rescs_m(j)%cs(ics)%mass = rescs_m(j)%cs(ics)%mass / const
          rescs_m(j)%cs(ics)%conc = rescs_m(j)%cs(ics)%conc / const
        enddo
        rescs_m(j)%cs(1)%volm = rescs_m(j)%cs(1)%volm / const
        if (pco%cs_res%m == "y") then
          write (6042,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, & 
                           (rescs_m(j)%cs(ics)%inflow,ics=1,cs_db%num_cs), &
                           (rescs_m(j)%cs(ics)%outflow,ics=1,cs_db%num_cs), &
                           (rescs_m(j)%cs(ics)%seep,ics=1,cs_db%num_cs), &
                           (rescs_m(j)%cs(ics)%settle,ics=1,cs_db%num_cs), &
                           (rescs_m(j)%cs(ics)%rctn,ics=1,cs_db%num_cs), &
                           (rescs_m(j)%cs(ics)%prod,ics=1,cs_db%num_cs), &
                           (rescs_m(j)%cs(ics)%fert,ics=1,cs_db%num_cs), &
                           (rescs_m(j)%cs(ics)%irrig,ics=1,cs_db%num_cs), &
                           (rescs_m(j)%cs(ics)%div,ics=1,cs_db%num_cs), &
                           (rescs_m(j)%cs(ics)%mass,ics=1,cs_db%num_cs), &
                           (rescs_m(j)%cs(ics)%conc,ics=1,cs_db%num_cs), &
                            rescs_m(j)%cs(1)%volm
          if (pco%csvout == "y") then
            write (6043,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, & 
                                          (rescs_m(j)%cs(ics)%inflow,ics=1,cs_db%num_cs), &
                                          (rescs_m(j)%cs(ics)%outflow,ics=1,cs_db%num_cs), &
                                          (rescs_m(j)%cs(ics)%seep,ics=1,cs_db%num_cs), &
                                          (rescs_m(j)%cs(ics)%settle,ics=1,cs_db%num_cs), &
                                          (rescs_m(j)%cs(ics)%rctn,ics=1,cs_db%num_cs), &
                                          (rescs_m(j)%cs(ics)%prod,ics=1,cs_db%num_cs), &
                                          (rescs_m(j)%cs(ics)%fert,ics=1,cs_db%num_cs), &
                                          (rescs_m(j)%cs(ics)%irrig,ics=1,cs_db%num_cs), &
                                          (rescs_m(j)%cs(ics)%div,ics=1,cs_db%num_cs), &
                                          (rescs_m(j)%cs(ics)%mass,ics=1,cs_db%num_cs), &
                                          (rescs_m(j)%cs(ics)%conc,ics=1,cs_db%num_cs), &
                                           rescs_m(j)%cs(1)%volm
          endif
        endif
        !zero out
        do ics = 1, cs_db%num_cs
          rescs_m(j)%cs(ics)%inflow = 0.
          rescs_m(j)%cs(ics)%outflow = 0.
          rescs_m(j)%cs(ics)%seep = 0.
          rescs_m(j)%cs(ics)%settle = 0.
          rescs_m(j)%cs(ics)%rctn = 0.
          rescs_m(j)%cs(ics)%prod = 0.
          rescs_m(j)%cs(ics)%fert = 0.
          rescs_m(j)%cs(ics)%irrig = 0.
          rescs_m(j)%cs(ics)%div = 0.
          rescs_m(j)%cs(ics)%mass = 0.
          rescs_m(j)%cs(ics)%conc = 0.
        enddo
        rescs_m(j)%cs(1)%volm = 0.
      endif
      
      !yearly print
      if (time%end_yr == 1) then
        !add yearly values to total values
        do ics = 1, cs_db%num_cs
          rescs_a(j)%cs(ics)%inflow = rescs_a(j)%cs(ics)%inflow + rescs_y(j)%cs(ics)%inflow
          rescs_a(j)%cs(ics)%outflow = rescs_a(j)%cs(ics)%outflow + rescs_y(j)%cs(ics)%outflow
          rescs_a(j)%cs(ics)%seep = rescs_a(j)%cs(ics)%seep + rescs_y(j)%cs(ics)%seep
          rescs_a(j)%cs(ics)%settle = rescs_a(j)%cs(ics)%settle + rescs_y(j)%cs(ics)%settle
          rescs_a(j)%cs(ics)%rctn = rescs_a(j)%cs(ics)%rctn + rescs_y(j)%cs(ics)%rctn
          rescs_a(j)%cs(ics)%prod = rescs_a(j)%cs(ics)%prod + rescs_y(j)%cs(ics)%prod
          rescs_a(j)%cs(ics)%fert = rescs_a(j)%cs(ics)%fert + rescs_y(j)%cs(ics)%fert
          rescs_a(j)%cs(ics)%irrig = rescs_a(j)%cs(ics)%irrig + rescs_y(j)%cs(ics)%irrig
          rescs_a(j)%cs(ics)%div = rescs_a(j)%cs(ics)%div + rescs_y(j)%cs(ics)%div
          rescs_a(j)%cs(ics)%mass = rescs_a(j)%cs(ics)%mass + rescs_y(j)%cs(ics)%mass
          rescs_a(j)%cs(ics)%conc = rescs_a(j)%cs(ics)%conc + rescs_y(j)%cs(ics)%conc
        enddo
        rescs_a(j)%cs(1)%volm = rescs_a(j)%cs(1)%volm + rescs_y(j)%cs(1)%volm
        const = time%day_end_yr
        do ics=1,cs_db%num_cs !average mass and concentration
          rescs_y(j)%cs(ics)%mass = rescs_y(j)%cs(ics)%mass / const
          rescs_y(j)%cs(ics)%conc = rescs_y(j)%cs(ics)%conc / const
        enddo
        rescs_y(j)%cs(1)%volm = rescs_y(j)%cs(1)%volm / const
        if (pco%cs_res%y == "y") then
          write (6044,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, & 
                           (rescs_y(j)%cs(ics)%inflow,ics=1,cs_db%num_cs), &
                           (rescs_y(j)%cs(ics)%outflow,ics=1,cs_db%num_cs), &
                           (rescs_y(j)%cs(ics)%seep,ics=1,cs_db%num_cs), &
                           (rescs_y(j)%cs(ics)%settle,ics=1,cs_db%num_cs), &
                           (rescs_y(j)%cs(ics)%rctn,ics=1,cs_db%num_cs), &
                           (rescs_y(j)%cs(ics)%prod,ics=1,cs_db%num_cs), &
                           (rescs_y(j)%cs(ics)%fert,ics=1,cs_db%num_cs), &
                           (rescs_y(j)%cs(ics)%irrig,ics=1,cs_db%num_cs), &
                           (rescs_y(j)%cs(ics)%div,ics=1,cs_db%num_cs), &
                           (rescs_y(j)%cs(ics)%mass,ics=1,cs_db%num_cs), &
                           (rescs_y(j)%cs(ics)%conc,ics=1,cs_db%num_cs), &
                            rescs_y(j)%cs(1)%volm
          if (pco%csvout == "y") then
            write (6045,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, & 
                                          (rescs_y(j)%cs(ics)%inflow,ics=1,cs_db%num_cs), &
                                          (rescs_y(j)%cs(ics)%outflow,ics=1,cs_db%num_cs), &
                                          (rescs_y(j)%cs(ics)%seep,ics=1,cs_db%num_cs), &
                                          (rescs_y(j)%cs(ics)%settle,ics=1,cs_db%num_cs), &
                                          (rescs_y(j)%cs(ics)%rctn,ics=1,cs_db%num_cs), &
                                          (rescs_y(j)%cs(ics)%prod,ics=1,cs_db%num_cs), &
                                          (rescs_y(j)%cs(ics)%fert,ics=1,cs_db%num_cs), &
                                          (rescs_y(j)%cs(ics)%irrig,ics=1,cs_db%num_cs), &
                                          (rescs_y(j)%cs(ics)%div,ics=1,cs_db%num_cs), &
                                          (rescs_y(j)%cs(ics)%mass,ics=1,cs_db%num_cs), &
                                          (rescs_y(j)%cs(ics)%conc,ics=1,cs_db%num_cs), &
                                           rescs_y(j)%cs(1)%volm
          endif
        endif
        !zero out
        do ics = 1, cs_db%num_cs
          rescs_y(j)%cs(ics)%inflow = 0.
          rescs_y(j)%cs(ics)%outflow = 0.
          rescs_y(j)%cs(ics)%seep = 0.
          rescs_y(j)%cs(ics)%settle = 0.
          rescs_y(j)%cs(ics)%rctn = 0.
          rescs_y(j)%cs(ics)%prod = 0.
          rescs_y(j)%cs(ics)%fert = 0.
          rescs_y(j)%cs(ics)%irrig = 0.
          rescs_y(j)%cs(ics)%div = 0.
          rescs_y(j)%cs(ics)%mass = 0.
          rescs_y(j)%cs(ics)%conc = 0.
        enddo
        rescs_y(j)%cs(1)%volm = 0.
      endif
      
      !average annual print
      if (time%end_sim == 1 .and. pco%cs_res%a == "y") then
        !calculate average annual values
        do ics = 1, cs_db%num_cs
          rescs_a(j)%cs(ics)%inflow = rescs_a(j)%cs(ics)%inflow / time%nbyr
          rescs_a(j)%cs(ics)%outflow = rescs_a(j)%cs(ics)%outflow / time%nbyr 
          rescs_a(j)%cs(ics)%seep = rescs_a(j)%cs(ics)%seep / time%nbyr 
          rescs_a(j)%cs(ics)%settle = rescs_a(j)%cs(ics)%settle / time%nbyr 
          rescs_a(j)%cs(ics)%rctn = rescs_a(j)%cs(ics)%rctn / time%nbyr 
          rescs_a(j)%cs(ics)%prod = rescs_a(j)%cs(ics)%prod / time%nbyr 
          rescs_a(j)%cs(ics)%fert = rescs_a(j)%cs(ics)%fert / time%nbyr 
          rescs_a(j)%cs(ics)%irrig = rescs_a(j)%cs(ics)%irrig / time%nbyr
          rescs_a(j)%cs(ics)%div = rescs_a(j)%cs(ics)%div / time%nbyr
          rescs_a(j)%cs(ics)%mass = rescs_a(j)%cs(ics)%mass / time%nbyr 
          rescs_a(j)%cs(ics)%conc = rescs_a(j)%cs(ics)%conc / time%nbyr
        enddo
        rescs_a(j)%cs(1)%volm = rescs_a(j)%cs(1)%volm / time%nbyr
        write (6046,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, & 
                         (rescs_a(j)%cs(ics)%inflow,ics=1,cs_db%num_cs), &
                         (rescs_a(j)%cs(ics)%outflow,ics=1,cs_db%num_cs), &
                         (rescs_a(j)%cs(ics)%seep,ics=1,cs_db%num_cs), &
                         (rescs_a(j)%cs(ics)%settle,ics=1,cs_db%num_cs), &
                         (rescs_a(j)%cs(ics)%rctn,ics=1,cs_db%num_cs), &
                         (rescs_a(j)%cs(ics)%prod,ics=1,cs_db%num_cs), &
                         (rescs_a(j)%cs(ics)%fert,ics=1,cs_db%num_cs), &
                         (rescs_a(j)%cs(ics)%irrig,ics=1,cs_db%num_cs), &
                         (rescs_a(j)%cs(ics)%div,ics=1,cs_db%num_cs), &
                         (rescs_a(j)%cs(ics)%mass,ics=1,cs_db%num_cs), &
                         (rescs_a(j)%cs(ics)%conc,ics=1,cs_db%num_cs), &
                          rescs_a(j)%cs(1)%volm
        if (pco%csvout == "y") then
          write (6047,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, & 
                                        (rescs_a(j)%cs(ics)%inflow,ics=1,cs_db%num_cs), &
                                        (rescs_a(j)%cs(ics)%outflow,ics=1,cs_db%num_cs), &
                                        (rescs_a(j)%cs(ics)%seep,ics=1,cs_db%num_cs), &
                                        (rescs_a(j)%cs(ics)%settle,ics=1,cs_db%num_cs), &
                                        (rescs_a(j)%cs(ics)%rctn,ics=1,cs_db%num_cs), &
                                        (rescs_a(j)%cs(ics)%prod,ics=1,cs_db%num_cs), &
                                        (rescs_a(j)%cs(ics)%fert,ics=1,cs_db%num_cs), &
                                        (rescs_a(j)%cs(ics)%irrig,ics=1,cs_db%num_cs), &
                                        (rescs_a(j)%cs(ics)%div,ics=1,cs_db%num_cs), &
                                        (rescs_a(j)%cs(ics)%mass,ics=1,cs_db%num_cs), &
                                        (rescs_a(j)%cs(ics)%conc,ics=1,cs_db%num_cs), &
                                         rescs_a(j)%cs(1)%volm
        endif
      endif

      return
      
      
100   format (4i6,2i8,500e15.4)      

      end subroutine res_cs_output