      subroutine res_salt_output(j) !rtb salt
    
      use output_ls_pesticide_module
      use res_pesticide_module
      use res_salt_module
      use plant_module
      use plant_data_module
      use time_module
      use basin_module
      use output_landscape_module
      use constituent_mass_module
      use hydrograph_module, only : sp_ob1, ob
      
      implicit none
      
      integer :: isalt                         !            |
      integer :: j
      integer :: iob
      real :: const
                         
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine outputs salt mass in reservoirs

      iob = sp_ob1%res + j - 1

      !add daily values to monthly values
      do isalt = 1, cs_db%num_salts
        ressalt_m(j)%salt(isalt)%inflow = ressalt_m(j)%salt(isalt)%inflow + ressalt_d(j)%salt(isalt)%inflow
        ressalt_m(j)%salt(isalt)%outflow = ressalt_m(j)%salt(isalt)%outflow + ressalt_d(j)%salt(isalt)%outflow
        ressalt_m(j)%salt(isalt)%seep = ressalt_m(j)%salt(isalt)%seep + ressalt_d(j)%salt(isalt)%seep
        ressalt_m(j)%salt(isalt)%fert = ressalt_m(j)%salt(isalt)%fert + ressalt_d(j)%salt(isalt)%fert
        ressalt_m(j)%salt(isalt)%irrig = ressalt_m(j)%salt(isalt)%irrig + ressalt_d(j)%salt(isalt)%irrig
        ressalt_m(j)%salt(isalt)%div = ressalt_m(j)%salt(isalt)%div + ressalt_d(j)%salt(isalt)%div
        ressalt_m(j)%salt(isalt)%mass = ressalt_m(j)%salt(isalt)%mass + ressalt_d(j)%salt(isalt)%mass
        ressalt_m(j)%salt(isalt)%conc = ressalt_m(j)%salt(isalt)%conc + ressalt_d(j)%salt(isalt)%conc
      enddo
      ressalt_m(j)%salt(1)%volm = ressalt_m(j)%salt(1)%volm + ressalt_d(j)%salt(1)%volm

      !daily print
      if (pco%salt_res%d == "y") then
        write (5040,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, & 
                         (ressalt_d(j)%salt(isalt)%inflow,isalt=1,cs_db%num_salts), &
                         (ressalt_d(j)%salt(isalt)%outflow,isalt=1,cs_db%num_salts), &
                         (ressalt_d(j)%salt(isalt)%seep,isalt=1,cs_db%num_salts), &
                         (ressalt_d(j)%salt(isalt)%fert,isalt=1,cs_db%num_salts), &
                         (ressalt_d(j)%salt(isalt)%irrig,isalt=1,cs_db%num_salts), &
                         (ressalt_d(j)%salt(isalt)%div,isalt=1,cs_db%num_salts), &
                         (ressalt_d(j)%salt(isalt)%mass,isalt=1,cs_db%num_salts), &
                         (ressalt_d(j)%salt(isalt)%conc,isalt=1,cs_db%num_salts), &
                          ressalt_d(j)%salt(1)%volm
        if (pco%csvout == "y") then
          write (5041,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, & 
                                        (ressalt_d(j)%salt(isalt)%inflow,isalt=1,cs_db%num_salts), &
                                        (ressalt_d(j)%salt(isalt)%outflow,isalt=1,cs_db%num_salts), &
                                        (ressalt_d(j)%salt(isalt)%seep,isalt=1,cs_db%num_salts), &
                                        (ressalt_d(j)%salt(isalt)%fert,isalt=1,cs_db%num_salts), &
                                        (ressalt_d(j)%salt(isalt)%irrig,isalt=1,cs_db%num_salts), &
                                        (ressalt_d(j)%salt(isalt)%div,isalt=1,cs_db%num_salts), &
                                        (ressalt_d(j)%salt(isalt)%mass,isalt=1,cs_db%num_salts), &
                                        (ressalt_d(j)%salt(isalt)%conc,isalt=1,cs_db%num_salts), &
                                         ressalt_d(j)%salt(1)%volm
        endif
      endif

      !monthly print
      if (time%end_mo == 1) then
        !add monthly values to yearly values
        do isalt = 1, cs_db%num_salts
          ressalt_y(j)%salt(isalt)%inflow = ressalt_y(j)%salt(isalt)%inflow + ressalt_m(j)%salt(isalt)%inflow
          ressalt_y(j)%salt(isalt)%outflow = ressalt_y(j)%salt(isalt)%outflow + ressalt_m(j)%salt(isalt)%outflow
          ressalt_y(j)%salt(isalt)%seep = ressalt_y(j)%salt(isalt)%seep + ressalt_m(j)%salt(isalt)%seep
          ressalt_y(j)%salt(isalt)%fert = ressalt_y(j)%salt(isalt)%fert + ressalt_m(j)%salt(isalt)%fert
          ressalt_y(j)%salt(isalt)%irrig = ressalt_y(j)%salt(isalt)%irrig + ressalt_m(j)%salt(isalt)%irrig
          ressalt_y(j)%salt(isalt)%div = ressalt_y(j)%salt(isalt)%div + ressalt_m(j)%salt(isalt)%div
          ressalt_y(j)%salt(isalt)%mass = ressalt_y(j)%salt(isalt)%mass + ressalt_m(j)%salt(isalt)%mass
          ressalt_y(j)%salt(isalt)%conc = ressalt_y(j)%salt(isalt)%conc + ressalt_m(j)%salt(isalt)%conc
        enddo
        ressalt_y(j)%salt(1)%volm = ressalt_y(j)%salt(1)%volm + ressalt_m(j)%salt(1)%volm
        const = float (ndays(time%mo + 1) - ndays(time%mo))
        do isalt=1,cs_db%num_salts !average mass and concentration
          ressalt_m(j)%salt(isalt)%mass = ressalt_m(j)%salt(isalt)%mass / const
          ressalt_m(j)%salt(isalt)%conc = ressalt_m(j)%salt(isalt)%conc / const
        enddo
        ressalt_m(j)%salt(1)%volm = ressalt_m(j)%salt(1)%volm / const
        if (pco%salt_res%m == "y") then
          write (5042,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, & 
                           (ressalt_m(j)%salt(isalt)%inflow,isalt=1,cs_db%num_salts), &
                           (ressalt_m(j)%salt(isalt)%outflow,isalt=1,cs_db%num_salts), &
                           (ressalt_m(j)%salt(isalt)%seep,isalt=1,cs_db%num_salts), &
                           (ressalt_m(j)%salt(isalt)%fert,isalt=1,cs_db%num_salts), &
                           (ressalt_m(j)%salt(isalt)%irrig,isalt=1,cs_db%num_salts), &
                           (ressalt_m(j)%salt(isalt)%div,isalt=1,cs_db%num_salts), &
                           (ressalt_m(j)%salt(isalt)%mass,isalt=1,cs_db%num_salts), &
                           (ressalt_m(j)%salt(isalt)%conc,isalt=1,cs_db%num_salts), &
                            ressalt_m(j)%salt(1)%volm
          if (pco%csvout == "y") then
            write (5043,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, & 
                                          (ressalt_m(j)%salt(isalt)%inflow,isalt=1,cs_db%num_salts), &
                                          (ressalt_m(j)%salt(isalt)%outflow,isalt=1,cs_db%num_salts), &
                                          (ressalt_m(j)%salt(isalt)%seep,isalt=1,cs_db%num_salts), &
                                          (ressalt_m(j)%salt(isalt)%fert,isalt=1,cs_db%num_salts), &
                                          (ressalt_m(j)%salt(isalt)%irrig,isalt=1,cs_db%num_salts), &
                                          (ressalt_m(j)%salt(isalt)%div,isalt=1,cs_db%num_salts), &
                                          (ressalt_m(j)%salt(isalt)%mass,isalt=1,cs_db%num_salts), &
                                          (ressalt_m(j)%salt(isalt)%conc,isalt=1,cs_db%num_salts), &
                                           ressalt_m(j)%salt(1)%volm
          endif
        endif
        !zero out
        do isalt = 1, cs_db%num_salts
          ressalt_m(j)%salt(isalt)%inflow = 0.
          ressalt_m(j)%salt(isalt)%outflow = 0.
          ressalt_m(j)%salt(isalt)%seep = 0.
          ressalt_m(j)%salt(isalt)%fert = 0.
          ressalt_m(j)%salt(isalt)%irrig = 0.
          ressalt_m(j)%salt(isalt)%div = 0.
          ressalt_m(j)%salt(isalt)%mass = 0.
          ressalt_m(j)%salt(isalt)%conc = 0.
        enddo
        ressalt_m(j)%salt(1)%volm = 0.
      endif
      
      !yearly print
      if (time%end_yr == 1) then
        !add yearly values to total values
        do isalt = 1, cs_db%num_salts
          ressalt_a(j)%salt(isalt)%inflow = ressalt_a(j)%salt(isalt)%inflow + ressalt_y(j)%salt(isalt)%inflow
          ressalt_a(j)%salt(isalt)%outflow = ressalt_a(j)%salt(isalt)%outflow + ressalt_y(j)%salt(isalt)%outflow
          ressalt_a(j)%salt(isalt)%seep = ressalt_a(j)%salt(isalt)%seep + ressalt_y(j)%salt(isalt)%seep
          ressalt_a(j)%salt(isalt)%fert = ressalt_a(j)%salt(isalt)%fert + ressalt_y(j)%salt(isalt)%fert
          ressalt_a(j)%salt(isalt)%irrig = ressalt_a(j)%salt(isalt)%irrig + ressalt_y(j)%salt(isalt)%irrig
          ressalt_a(j)%salt(isalt)%div = ressalt_a(j)%salt(isalt)%div + ressalt_y(j)%salt(isalt)%div
          ressalt_a(j)%salt(isalt)%mass = ressalt_a(j)%salt(isalt)%mass + ressalt_y(j)%salt(isalt)%mass
          ressalt_a(j)%salt(isalt)%conc = ressalt_a(j)%salt(isalt)%conc + ressalt_y(j)%salt(isalt)%conc
        enddo
        ressalt_a(j)%salt(1)%volm = ressalt_a(j)%salt(1)%volm + ressalt_y(j)%salt(1)%volm
        const = time%day_end_yr
        do isalt=1,cs_db%num_salts !average mass and concentration
          ressalt_y(j)%salt(isalt)%mass = ressalt_y(j)%salt(isalt)%mass / const
          ressalt_y(j)%salt(isalt)%conc = ressalt_y(j)%salt(isalt)%conc / const
        enddo
        ressalt_y(j)%salt(1)%volm = ressalt_y(j)%salt(1)%volm / const
        if (pco%salt_res%y == "y") then
          write (5044,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, & 
                           (ressalt_y(j)%salt(isalt)%inflow,isalt=1,cs_db%num_salts), &
                           (ressalt_y(j)%salt(isalt)%outflow,isalt=1,cs_db%num_salts), &
                           (ressalt_y(j)%salt(isalt)%seep,isalt=1,cs_db%num_salts), &
                           (ressalt_y(j)%salt(isalt)%fert,isalt=1,cs_db%num_salts), &
                           (ressalt_y(j)%salt(isalt)%irrig,isalt=1,cs_db%num_salts), &
                           (ressalt_y(j)%salt(isalt)%div,isalt=1,cs_db%num_salts), &
                           (ressalt_y(j)%salt(isalt)%mass,isalt=1,cs_db%num_salts), &
                           (ressalt_y(j)%salt(isalt)%conc,isalt=1,cs_db%num_salts), &
                            ressalt_y(j)%salt(1)%volm
          if (pco%csvout == "y") then
            write (5045,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, & 
                                          (ressalt_y(j)%salt(isalt)%inflow,isalt=1,cs_db%num_salts), &
                                          (ressalt_y(j)%salt(isalt)%outflow,isalt=1,cs_db%num_salts), &
                                          (ressalt_y(j)%salt(isalt)%seep,isalt=1,cs_db%num_salts), &
                                          (ressalt_y(j)%salt(isalt)%fert,isalt=1,cs_db%num_salts), &
                                          (ressalt_y(j)%salt(isalt)%irrig,isalt=1,cs_db%num_salts), &
                                          (ressalt_y(j)%salt(isalt)%div,isalt=1,cs_db%num_salts), &
                                          (ressalt_y(j)%salt(isalt)%mass,isalt=1,cs_db%num_salts), &
                                          (ressalt_y(j)%salt(isalt)%conc,isalt=1,cs_db%num_salts), &
                                           ressalt_y(j)%salt(1)%volm
          endif
        endif
        !zero out
        do isalt = 1, cs_db%num_salts
          ressalt_y(j)%salt(isalt)%inflow = 0.
          ressalt_y(j)%salt(isalt)%outflow = 0.
          ressalt_y(j)%salt(isalt)%seep = 0.
          ressalt_y(j)%salt(isalt)%fert = 0.
          ressalt_y(j)%salt(isalt)%irrig = 0.
          ressalt_y(j)%salt(isalt)%div = 0.
          ressalt_y(j)%salt(isalt)%mass = 0.
          ressalt_y(j)%salt(isalt)%conc = 0.
        enddo
        ressalt_y(j)%salt(1)%volm = 0.
      endif
      
      !average annual print
      if (time%end_sim == 1 .and. pco%salt_res%a == "y") then
        !calculate average annual values
        do isalt = 1, cs_db%num_salts
          ressalt_a(j)%salt(isalt)%inflow = ressalt_a(j)%salt(isalt)%inflow / time%nbyr
          ressalt_a(j)%salt(isalt)%outflow = ressalt_a(j)%salt(isalt)%outflow / time%nbyr 
          ressalt_a(j)%salt(isalt)%seep = ressalt_a(j)%salt(isalt)%seep / time%nbyr 
          ressalt_a(j)%salt(isalt)%fert = ressalt_a(j)%salt(isalt)%fert / time%nbyr 
          ressalt_a(j)%salt(isalt)%irrig = ressalt_a(j)%salt(isalt)%irrig / time%nbyr
          ressalt_a(j)%salt(isalt)%div = ressalt_a(j)%salt(isalt)%div / time%nbyr
          ressalt_a(j)%salt(isalt)%mass = ressalt_a(j)%salt(isalt)%mass / time%nbyr 
          ressalt_a(j)%salt(isalt)%conc = ressalt_a(j)%salt(isalt)%conc / time%nbyr
        enddo
        ressalt_a(j)%salt(1)%volm = ressalt_a(j)%salt(1)%volm / time%nbyr
        write (5046,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, & 
                         (ressalt_a(j)%salt(isalt)%inflow,isalt=1,cs_db%num_salts), &
                         (ressalt_a(j)%salt(isalt)%outflow,isalt=1,cs_db%num_salts), &
                         (ressalt_a(j)%salt(isalt)%seep,isalt=1,cs_db%num_salts), &
                         (ressalt_a(j)%salt(isalt)%fert,isalt=1,cs_db%num_salts), &
                         (ressalt_a(j)%salt(isalt)%irrig,isalt=1,cs_db%num_salts), &
                         (ressalt_a(j)%salt(isalt)%div,isalt=1,cs_db%num_salts), &
                         (ressalt_a(j)%salt(isalt)%mass,isalt=1,cs_db%num_salts), &
                         (ressalt_a(j)%salt(isalt)%conc,isalt=1,cs_db%num_salts), &
                          ressalt_a(j)%salt(1)%volm
        if (pco%csvout == "y") then
          write (5047,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, & 
                                        (ressalt_a(j)%salt(isalt)%inflow,isalt=1,cs_db%num_salts), &
                                        (ressalt_a(j)%salt(isalt)%outflow,isalt=1,cs_db%num_salts), &
                                        (ressalt_a(j)%salt(isalt)%seep,isalt=1,cs_db%num_salts), &
                                        (ressalt_a(j)%salt(isalt)%fert,isalt=1,cs_db%num_salts), &
                                        (ressalt_a(j)%salt(isalt)%irrig,isalt=1,cs_db%num_salts), &
                                        (ressalt_a(j)%salt(isalt)%div,isalt=1,cs_db%num_salts), &
                                        (ressalt_a(j)%salt(isalt)%mass,isalt=1,cs_db%num_salts), &
                                        (ressalt_a(j)%salt(isalt)%conc,isalt=1,cs_db%num_salts), &
                                         ressalt_a(j)%salt(1)%volm
        endif
      endif

      return
         
100   format (4i6,2i8,500e15.7)      

      end subroutine res_salt_output