      subroutine wet_cs_output(j) !rtb cs
    
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
      
      integer :: ics
      integer :: j
      integer :: iob
      real :: const
                         
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine outputs constituent mass in wetlands (by HRU)
      
      !objective number of HRU
      iob = sp_ob1%hru + j - 1
          
      !add daily values to monthly values
      do ics = 1, cs_db%num_cs
        wetcs_m(j)%cs(ics)%inflow = wetcs_m(j)%cs(ics)%inflow + wetcs_d(j)%cs(ics)%inflow
        wetcs_m(j)%cs(ics)%outflow = wetcs_m(j)%cs(ics)%outflow + wetcs_d(j)%cs(ics)%outflow
        wetcs_m(j)%cs(ics)%seep = wetcs_m(j)%cs(ics)%seep + wetcs_d(j)%cs(ics)%seep
        wetcs_m(j)%cs(ics)%settle = wetcs_m(j)%cs(ics)%settle + wetcs_d(j)%cs(ics)%settle
        wetcs_m(j)%cs(ics)%rctn = wetcs_m(j)%cs(ics)%rctn + wetcs_d(j)%cs(ics)%rctn
        wetcs_m(j)%cs(ics)%prod = wetcs_m(j)%cs(ics)%prod + wetcs_d(j)%cs(ics)%prod
        wetcs_m(j)%cs(ics)%fert = wetcs_m(j)%cs(ics)%fert + wetcs_d(j)%cs(ics)%fert
        wetcs_m(j)%cs(ics)%irrig = wetcs_m(j)%cs(ics)%irrig + wetcs_d(j)%cs(ics)%irrig
        wetcs_m(j)%cs(ics)%mass = wetcs_m(j)%cs(ics)%mass + wetcs_d(j)%cs(ics)%mass
        wetcs_m(j)%cs(ics)%conc = wetcs_m(j)%cs(ics)%conc + wetcs_d(j)%cs(ics)%conc
      enddo
      wetcs_m(j)%cs(1)%volm = wetcs_m(j)%cs(1)%volm + wetcs_d(j)%cs(1)%volm
      
      !daily print
      if (pco%cs_res%d == "y") then
        write (6090,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, & 
                         (wetcs_d(j)%cs(ics)%inflow,ics=1,cs_db%num_cs), &
                         (wetcs_d(j)%cs(ics)%outflow,ics=1,cs_db%num_cs), &
                         (wetcs_d(j)%cs(ics)%seep,ics=1,cs_db%num_cs), &
                         (wetcs_d(j)%cs(ics)%settle,ics=1,cs_db%num_cs), &
                         (wetcs_d(j)%cs(ics)%rctn,ics=1,cs_db%num_cs), &
                         (wetcs_d(j)%cs(ics)%prod,ics=1,cs_db%num_cs), &
                         (wetcs_d(j)%cs(ics)%fert,ics=1,cs_db%num_cs), &
                         (wetcs_d(j)%cs(ics)%irrig,ics=1,cs_db%num_cs), &
                         (wetcs_d(j)%cs(ics)%mass,ics=1,cs_db%num_cs), &
                         (wetcs_d(j)%cs(ics)%conc,ics=1,cs_db%num_cs), &
                          wetcs_d(j)%cs(1)%volm
        if (pco%csvout == "y") then
          write (6091,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, & 
                                        (wetcs_d(j)%cs(ics)%inflow,ics=1,cs_db%num_cs), &
                                        (wetcs_d(j)%cs(ics)%outflow,ics=1,cs_db%num_cs), &
                                        (wetcs_d(j)%cs(ics)%seep,ics=1,cs_db%num_cs), &
                                        (wetcs_d(j)%cs(ics)%settle,ics=1,cs_db%num_cs), &
                                        (wetcs_d(j)%cs(ics)%rctn,ics=1,cs_db%num_cs), &
                                        (wetcs_d(j)%cs(ics)%prod,ics=1,cs_db%num_cs), &
                                        (wetcs_d(j)%cs(ics)%fert,ics=1,cs_db%num_cs), &
                                        (wetcs_d(j)%cs(ics)%irrig,ics=1,cs_db%num_cs), &
                                        (wetcs_d(j)%cs(ics)%mass,ics=1,cs_db%num_cs), &
                                        (wetcs_d(j)%cs(ics)%conc,ics=1,cs_db%num_cs), &
                                         wetcs_d(j)%cs(1)%volm
        endif
      endif

      !monthly print
      if (time%end_mo == 1) then
        !add monthly values to yearly values
        do ics = 1, cs_db%num_cs
          wetcs_y(j)%cs(ics)%inflow = wetcs_y(j)%cs(ics)%inflow + wetcs_m(j)%cs(ics)%inflow
          wetcs_y(j)%cs(ics)%outflow = wetcs_y(j)%cs(ics)%outflow + wetcs_m(j)%cs(ics)%outflow
          wetcs_y(j)%cs(ics)%seep = wetcs_y(j)%cs(ics)%seep + wetcs_m(j)%cs(ics)%seep
          wetcs_y(j)%cs(ics)%settle = wetcs_y(j)%cs(ics)%settle + wetcs_m(j)%cs(ics)%settle
          wetcs_y(j)%cs(ics)%rctn = wetcs_y(j)%cs(ics)%rctn + wetcs_m(j)%cs(ics)%rctn
          wetcs_y(j)%cs(ics)%prod = wetcs_y(j)%cs(ics)%prod + wetcs_m(j)%cs(ics)%prod
          wetcs_y(j)%cs(ics)%fert = wetcs_y(j)%cs(ics)%fert + wetcs_m(j)%cs(ics)%fert
          wetcs_y(j)%cs(ics)%irrig = wetcs_y(j)%cs(ics)%irrig + wetcs_m(j)%cs(ics)%irrig
          wetcs_y(j)%cs(ics)%mass = wetcs_y(j)%cs(ics)%mass + wetcs_m(j)%cs(ics)%mass
          wetcs_y(j)%cs(ics)%conc = wetcs_y(j)%cs(ics)%conc + wetcs_m(j)%cs(ics)%conc
        enddo
        wetcs_y(j)%cs(1)%volm = wetcs_y(j)%cs(1)%volm + wetcs_m(j)%cs(1)%volm
        const = float (ndays(time%mo + 1) - ndays(time%mo))
        do ics=1,cs_db%num_cs !average mass and concentration
          wetcs_m(j)%cs(ics)%mass = wetcs_m(j)%cs(ics)%mass / const
          wetcs_m(j)%cs(ics)%conc = wetcs_m(j)%cs(ics)%conc / const
        enddo
        wetcs_m(j)%cs(1)%volm = wetcs_m(j)%cs(1)%volm / const
        if (pco%cs_res%m == "y") then
          write (6092,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, & 
                           (wetcs_m(j)%cs(ics)%inflow,ics=1,cs_db%num_cs), &
                           (wetcs_m(j)%cs(ics)%outflow,ics=1,cs_db%num_cs), &
                           (wetcs_m(j)%cs(ics)%seep,ics=1,cs_db%num_cs), &
                           (wetcs_m(j)%cs(ics)%settle,ics=1,cs_db%num_cs), &
                           (wetcs_m(j)%cs(ics)%rctn,ics=1,cs_db%num_cs), &
                           (wetcs_m(j)%cs(ics)%prod,ics=1,cs_db%num_cs), &
                           (wetcs_m(j)%cs(ics)%fert,ics=1,cs_db%num_cs), &
                           (wetcs_m(j)%cs(ics)%irrig,ics=1,cs_db%num_cs), &
                           (wetcs_m(j)%cs(ics)%mass,ics=1,cs_db%num_cs), &
                           (wetcs_m(j)%cs(ics)%conc,ics=1,cs_db%num_cs), &
                            wetcs_m(j)%cs(1)%volm
          if (pco%csvout == "y") then
            write (6093,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, & 
                                          (wetcs_m(j)%cs(ics)%inflow,ics=1,cs_db%num_cs), &
                                          (wetcs_m(j)%cs(ics)%outflow,ics=1,cs_db%num_cs), &
                                          (wetcs_m(j)%cs(ics)%seep,ics=1,cs_db%num_cs), &
                                          (wetcs_m(j)%cs(ics)%settle,ics=1,cs_db%num_cs), &
                                          (wetcs_m(j)%cs(ics)%rctn,ics=1,cs_db%num_cs), &
                                          (wetcs_m(j)%cs(ics)%prod,ics=1,cs_db%num_cs), &
                                          (wetcs_m(j)%cs(ics)%fert,ics=1,cs_db%num_cs), &
                                          (wetcs_m(j)%cs(ics)%irrig,ics=1,cs_db%num_cs), &
                                          (wetcs_m(j)%cs(ics)%mass,ics=1,cs_db%num_cs), &
                                          (wetcs_m(j)%cs(ics)%conc,ics=1,cs_db%num_cs), &
                                           wetcs_m(j)%cs(1)%volm
          endif
        endif
        !zero out
        do ics = 1, cs_db%num_cs
          wetcs_m(j)%cs(ics)%inflow = 0.
          wetcs_m(j)%cs(ics)%outflow = 0.
          wetcs_m(j)%cs(ics)%seep = 0.
          wetcs_m(j)%cs(ics)%settle = 0.
          wetcs_m(j)%cs(ics)%rctn = 0.
          wetcs_m(j)%cs(ics)%prod = 0.
          wetcs_m(j)%cs(ics)%fert = 0.
          wetcs_m(j)%cs(ics)%irrig = 0.
          wetcs_m(j)%cs(ics)%mass = 0.
          wetcs_m(j)%cs(ics)%conc = 0.
        enddo
        wetcs_m(j)%cs(1)%volm = 0.
      endif
      
      !yearly print
      if (time%end_yr == 1) then
        !add yearly values to total values
        do ics = 1, cs_db%num_cs
          wetcs_a(j)%cs(ics)%inflow = wetcs_a(j)%cs(ics)%inflow + wetcs_y(j)%cs(ics)%inflow
          wetcs_a(j)%cs(ics)%outflow = wetcs_a(j)%cs(ics)%outflow + wetcs_y(j)%cs(ics)%outflow
          wetcs_a(j)%cs(ics)%seep = wetcs_a(j)%cs(ics)%seep + wetcs_y(j)%cs(ics)%seep
          wetcs_a(j)%cs(ics)%settle = wetcs_a(j)%cs(ics)%settle + wetcs_y(j)%cs(ics)%settle
          wetcs_a(j)%cs(ics)%rctn = wetcs_a(j)%cs(ics)%rctn + wetcs_y(j)%cs(ics)%rctn
          wetcs_a(j)%cs(ics)%prod = wetcs_a(j)%cs(ics)%prod + wetcs_y(j)%cs(ics)%prod
          wetcs_a(j)%cs(ics)%fert = wetcs_a(j)%cs(ics)%fert + wetcs_y(j)%cs(ics)%fert
          wetcs_a(j)%cs(ics)%irrig = wetcs_a(j)%cs(ics)%irrig + wetcs_y(j)%cs(ics)%irrig
          wetcs_a(j)%cs(ics)%mass = wetcs_a(j)%cs(ics)%mass + wetcs_y(j)%cs(ics)%mass
          wetcs_a(j)%cs(ics)%conc = wetcs_a(j)%cs(ics)%conc + wetcs_y(j)%cs(ics)%conc
        enddo
        wetcs_a(j)%cs(1)%volm = wetcs_a(j)%cs(1)%volm + wetcs_y(j)%cs(1)%volm
        const = time%day_end_yr
        do ics=1,cs_db%num_cs !average mass and concentration
          wetcs_y(j)%cs(ics)%mass = wetcs_y(j)%cs(ics)%mass / const
          wetcs_y(j)%cs(ics)%conc = wetcs_y(j)%cs(ics)%conc / const
        enddo
        wetcs_y(j)%cs(1)%volm = wetcs_y(j)%cs(1)%volm / const
        if (pco%cs_res%y == "y") then
          write (6094,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, & 
                           (wetcs_y(j)%cs(ics)%inflow,ics=1,cs_db%num_cs), &
                           (wetcs_y(j)%cs(ics)%outflow,ics=1,cs_db%num_cs), &
                           (wetcs_y(j)%cs(ics)%seep,ics=1,cs_db%num_cs), &
                           (wetcs_y(j)%cs(ics)%settle,ics=1,cs_db%num_cs), &
                           (wetcs_y(j)%cs(ics)%rctn,ics=1,cs_db%num_cs), &
                           (wetcs_y(j)%cs(ics)%prod,ics=1,cs_db%num_cs), &
                           (wetcs_y(j)%cs(ics)%fert,ics=1,cs_db%num_cs), &
                           (wetcs_y(j)%cs(ics)%irrig,ics=1,cs_db%num_cs), &
                           (wetcs_y(j)%cs(ics)%mass,ics=1,cs_db%num_cs), &
                           (wetcs_y(j)%cs(ics)%conc,ics=1,cs_db%num_cs), &
                            wetcs_y(j)%cs(1)%volm
          if (pco%csvout == "y") then
            write (6095,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, & 
                                          (wetcs_y(j)%cs(ics)%inflow,ics=1,cs_db%num_cs), &
                                          (wetcs_y(j)%cs(ics)%outflow,ics=1,cs_db%num_cs), &
                                          (wetcs_y(j)%cs(ics)%seep,ics=1,cs_db%num_cs), &
                                          (wetcs_y(j)%cs(ics)%settle,ics=1,cs_db%num_cs), &
                                          (wetcs_y(j)%cs(ics)%rctn,ics=1,cs_db%num_cs), &
                                          (wetcs_y(j)%cs(ics)%prod,ics=1,cs_db%num_cs), &
                                          (wetcs_y(j)%cs(ics)%fert,ics=1,cs_db%num_cs), &
                                          (wetcs_y(j)%cs(ics)%irrig,ics=1,cs_db%num_cs), &
                                          (wetcs_y(j)%cs(ics)%mass,ics=1,cs_db%num_cs), &
                                          (wetcs_y(j)%cs(ics)%conc,ics=1,cs_db%num_cs), &
                                           wetcs_y(j)%cs(1)%volm
          endif
        endif
        !zero out
        do ics = 1, cs_db%num_cs
          wetcs_y(j)%cs(ics)%inflow = 0.
          wetcs_y(j)%cs(ics)%outflow = 0.
          wetcs_y(j)%cs(ics)%seep = 0.
          wetcs_y(j)%cs(ics)%settle = 0.
          wetcs_y(j)%cs(ics)%rctn = 0.
          wetcs_y(j)%cs(ics)%prod = 0.
          wetcs_y(j)%cs(ics)%fert = 0.
          wetcs_y(j)%cs(ics)%irrig = 0.
          wetcs_y(j)%cs(ics)%mass = 0.
          wetcs_y(j)%cs(ics)%conc = 0.
        enddo
        wetcs_y(j)%cs(1)%volm = 0.
      endif
      
      !average annual print
      if (time%end_sim == 1 .and. pco%cs_res%a == "y") then
        !calculate average annual values
        do ics = 1, cs_db%num_cs
          wetcs_a(j)%cs(ics)%inflow = wetcs_a(j)%cs(ics)%inflow / time%nbyr
          wetcs_a(j)%cs(ics)%outflow = wetcs_a(j)%cs(ics)%outflow / time%nbyr 
          wetcs_a(j)%cs(ics)%seep = wetcs_a(j)%cs(ics)%seep / time%nbyr 
          wetcs_a(j)%cs(ics)%settle = wetcs_a(j)%cs(ics)%settle / time%nbyr 
          wetcs_a(j)%cs(ics)%rctn = wetcs_a(j)%cs(ics)%rctn / time%nbyr 
          wetcs_a(j)%cs(ics)%prod = wetcs_a(j)%cs(ics)%prod / time%nbyr 
          wetcs_a(j)%cs(ics)%fert = wetcs_a(j)%cs(ics)%fert / time%nbyr 
          wetcs_a(j)%cs(ics)%irrig = wetcs_a(j)%cs(ics)%irrig / time%nbyr
          wetcs_a(j)%cs(ics)%mass = wetcs_a(j)%cs(ics)%mass / time%nbyr 
          wetcs_a(j)%cs(ics)%conc = wetcs_a(j)%cs(ics)%conc / time%nbyr
        enddo
        wetcs_a(j)%cs(1)%volm = wetcs_a(j)%cs(1)%volm / time%nbyr
        write (6096,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, & 
                         (wetcs_a(j)%cs(ics)%inflow,ics=1,cs_db%num_cs), &
                         (wetcs_a(j)%cs(ics)%outflow,ics=1,cs_db%num_cs), &
                         (wetcs_a(j)%cs(ics)%seep,ics=1,cs_db%num_cs), &
                         (wetcs_a(j)%cs(ics)%settle,ics=1,cs_db%num_cs), &
                         (wetcs_a(j)%cs(ics)%rctn,ics=1,cs_db%num_cs), &
                         (wetcs_a(j)%cs(ics)%prod,ics=1,cs_db%num_cs), &
                         (wetcs_a(j)%cs(ics)%fert,ics=1,cs_db%num_cs), &
                         (wetcs_a(j)%cs(ics)%irrig,ics=1,cs_db%num_cs), &
                         (wetcs_a(j)%cs(ics)%mass,ics=1,cs_db%num_cs), &
                         (wetcs_a(j)%cs(ics)%conc,ics=1,cs_db%num_cs), &
                          wetcs_a(j)%cs(1)%volm
        if (pco%csvout == "y") then
          write (6097,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, & 
                                        (wetcs_a(j)%cs(ics)%inflow,ics=1,cs_db%num_cs), &
                                        (wetcs_a(j)%cs(ics)%outflow,ics=1,cs_db%num_cs), &
                                        (wetcs_a(j)%cs(ics)%seep,ics=1,cs_db%num_cs), &
                                        (wetcs_a(j)%cs(ics)%settle,ics=1,cs_db%num_cs), &
                                        (wetcs_a(j)%cs(ics)%rctn,ics=1,cs_db%num_cs), &
                                        (wetcs_a(j)%cs(ics)%prod,ics=1,cs_db%num_cs), &
                                        (wetcs_a(j)%cs(ics)%fert,ics=1,cs_db%num_cs), &
                                        (wetcs_a(j)%cs(ics)%irrig,ics=1,cs_db%num_cs), &
                                        (wetcs_a(j)%cs(ics)%mass,ics=1,cs_db%num_cs), &
                                        (wetcs_a(j)%cs(ics)%conc,ics=1,cs_db%num_cs), &
                                         wetcs_a(j)%cs(1)%volm
        endif
      endif

      return
      
100   format (4i6,2i8,500e15.4)      

      end subroutine wet_cs_output