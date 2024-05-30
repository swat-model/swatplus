      subroutine ch_cs_output(jrch) !rtb cs
    
      use output_ls_pesticide_module
      use ch_cs_module
      use plant_module
      use plant_data_module
      use time_module
      use basin_module
      use output_landscape_module
      use constituent_mass_module
      use hydrograph_module, only : sp_ob1, ob
      
      implicit none
      
      integer, intent (in) :: jrch             !            |
      integer :: ics                           !            |
      integer :: iru
      integer :: iob
      integer :: dum
      integer :: n
      real :: const
      
                         
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine outputs constituent mass in channels
      
      iru = jrch  !!! nbs
      
      iob = sp_ob1%chandeg + iru - 1
      
      !add daily values to monthly values
      do ics = 1, cs_db%num_cs
        chcs_m(iru)%cs(ics)%tot_in = chcs_m(iru)%cs(ics)%tot_in + chcs_d(iru)%cs(ics)%tot_in
        chcs_m(iru)%cs(ics)%gw_in = chcs_m(iru)%cs(ics)%gw_in + chcs_d(iru)%cs(ics)%gw_in
        chcs_m(iru)%cs(ics)%tot_out = chcs_m(iru)%cs(ics)%tot_out + chcs_d(iru)%cs(ics)%tot_out
        chcs_m(iru)%cs(ics)%seep = chcs_m(iru)%cs(ics)%seep + chcs_d(iru)%cs(ics)%seep
        chcs_m(iru)%cs(ics)%irr = chcs_m(iru)%cs(ics)%irr + chcs_d(iru)%cs(ics)%irr
        chcs_m(iru)%cs(ics)%div = chcs_m(iru)%cs(ics)%div + chcs_d(iru)%cs(ics)%div
        chcs_m(iru)%cs(ics)%water = chcs_m(iru)%cs(ics)%water + chcs_d(iru)%cs(ics)%water
        chcs_m(iru)%cs(ics)%conc = chcs_m(iru)%cs(ics)%conc + chcs_d(iru)%cs(ics)%conc
      enddo
      
      !daily print
      if (pco%cs_chn%d == "y") then
        write (6030,100) time%day, time%mo, time%day_mo, time%yrc, iru, ob(iob)%gis_id, & 
                         (chcs_d(iru)%cs(ics)%tot_in,ics=1,cs_db%num_cs), &
                         (chcs_d(iru)%cs(ics)%gw_in,ics=1,cs_db%num_cs), &
                         (chcs_d(iru)%cs(ics)%tot_out,ics=1,cs_db%num_cs), &
                         (chcs_d(iru)%cs(ics)%seep,ics=1,cs_db%num_cs), &
                         (chcs_d(iru)%cs(ics)%irr,ics=1,cs_db%num_cs), &
                         (chcs_d(iru)%cs(ics)%div,ics=1,cs_db%num_cs), &
                         (chcs_d(iru)%cs(ics)%water,ics=1,cs_db%num_cs), &
                         (chcs_d(iru)%cs(ics)%conc,ics=1,cs_db%num_cs)
        if (pco%csvout == "y") then
          write (6031,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, iru, ob(iob)%gis_id, & 
                                        (chcs_d(iru)%cs(ics)%tot_in,ics=1,cs_db%num_cs), &
                                        (chcs_d(iru)%cs(ics)%gw_in,ics=1,cs_db%num_cs), &
                                        (chcs_d(iru)%cs(ics)%tot_out,ics=1,cs_db%num_cs), &
                                        (chcs_d(iru)%cs(ics)%seep,ics=1,cs_db%num_cs), &
                                        (chcs_d(iru)%cs(ics)%irr,ics=1,cs_db%num_cs), &
                                        (chcs_d(iru)%cs(ics)%div,ics=1,cs_db%num_cs), &
                                        (chcs_d(iru)%cs(ics)%water,ics=1,cs_db%num_cs), &
                                        (chcs_d(iru)%cs(ics)%conc,ics=1,cs_db%num_cs)
        endif
      endif

      !monthly print
      if (time%end_mo == 1) then

        !add monthly values to yearly values
        do ics = 1, cs_db%num_cs
          chcs_y(iru)%cs(ics)%tot_in = chcs_y(iru)%cs(ics)%tot_in + chcs_m(iru)%cs(ics)%tot_in
          chcs_y(iru)%cs(ics)%gw_in = chcs_y(iru)%cs(ics)%gw_in + chcs_m(iru)%cs(ics)%gw_in
          chcs_y(iru)%cs(ics)%tot_out = chcs_y(iru)%cs(ics)%tot_out + chcs_m(iru)%cs(ics)%tot_out
          chcs_y(iru)%cs(ics)%seep = chcs_y(iru)%cs(ics)%seep + chcs_m(iru)%cs(ics)%seep
          chcs_y(iru)%cs(ics)%irr = chcs_y(iru)%cs(ics)%irr + chcs_m(iru)%cs(ics)%irr
          chcs_y(iru)%cs(ics)%div = chcs_y(iru)%cs(ics)%div + chcs_m(iru)%cs(ics)%div
          chcs_y(iru)%cs(ics)%water = chcs_y(iru)%cs(ics)%water + chcs_m(iru)%cs(ics)%water
          chcs_y(iru)%cs(ics)%conc = chcs_y(iru)%cs(ics)%conc + chcs_m(iru)%cs(ics)%conc
        enddo
        const = float (ndays(time%mo + 1) - ndays(time%mo))
        do ics=1,cs_db%num_cs !average mass and concentration
          chcs_m(iru)%cs(ics)%water = chcs_m(iru)%cs(ics)%water / const
          chcs_m(iru)%cs(ics)%conc = chcs_m(iru)%cs(ics)%conc / const
        enddo
        if (pco%cs_chn%m == "y") then
          write (6032,100) time%day, time%mo, time%day_mo, time%yrc, iru, ob(iob)%gis_id, & 
                           (chcs_m(iru)%cs(ics)%tot_in,ics=1,cs_db%num_cs), &
                           (chcs_m(iru)%cs(ics)%gw_in,ics=1,cs_db%num_cs), &
                           (chcs_m(iru)%cs(ics)%tot_out,ics=1,cs_db%num_cs), &
                           (chcs_m(iru)%cs(ics)%seep,ics=1,cs_db%num_cs), &
                           (chcs_m(iru)%cs(ics)%irr,ics=1,cs_db%num_cs), &
                           (chcs_m(iru)%cs(ics)%div,ics=1,cs_db%num_cs), &
                           (chcs_m(iru)%cs(ics)%water,ics=1,cs_db%num_cs), &
                           (chcs_m(iru)%cs(ics)%conc,ics=1,cs_db%num_cs)
          if (pco%csvout == "y") then
            write (6033,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, iru, ob(iob)%gis_id, & 
                                          (chcs_m(iru)%cs(ics)%tot_in,ics=1,cs_db%num_cs), &
                                          (chcs_m(iru)%cs(ics)%gw_in,ics=1,cs_db%num_cs), &
                                          (chcs_m(iru)%cs(ics)%tot_out,ics=1,cs_db%num_cs), &
                                          (chcs_m(iru)%cs(ics)%seep,ics=1,cs_db%num_cs), &
                                          (chcs_m(iru)%cs(ics)%irr,ics=1,cs_db%num_cs), &
                                          (chcs_m(iru)%cs(ics)%div,ics=1,cs_db%num_cs), &
                                          (chcs_m(iru)%cs(ics)%water,ics=1,cs_db%num_cs), &
                                          (chcs_m(iru)%cs(ics)%conc,ics=1,cs_db%num_cs)
          endif
        endif
        !zero out
        do ics = 1, cs_db%num_cs
          chcs_m(iru)%cs(ics)%tot_in = 0.
          chcs_m(iru)%cs(ics)%gw_in = 0.
          chcs_m(iru)%cs(ics)%tot_out = 0.
          chcs_m(iru)%cs(ics)%seep = 0.
          chcs_m(iru)%cs(ics)%irr = 0.
          chcs_m(iru)%cs(ics)%div = 0.
          chcs_m(iru)%cs(ics)%water = 0.
          chcs_m(iru)%cs(ics)%conc = 0.
        enddo
      endif
      
      !yearly print
      if (time%end_yr == 1) then
        !add yearly values to total values
        do ics = 1, cs_db%num_cs
          chcs_a(iru)%cs(ics)%tot_in = chcs_a(iru)%cs(ics)%tot_in + chcs_y(iru)%cs(ics)%tot_in
          chcs_a(iru)%cs(ics)%gw_in = chcs_a(iru)%cs(ics)%gw_in + chcs_y(iru)%cs(ics)%gw_in
          chcs_a(iru)%cs(ics)%tot_out = chcs_a(iru)%cs(ics)%tot_out + chcs_y(iru)%cs(ics)%tot_out
          chcs_a(iru)%cs(ics)%seep = chcs_a(iru)%cs(ics)%seep + chcs_y(iru)%cs(ics)%seep
          chcs_a(iru)%cs(ics)%irr = chcs_a(iru)%cs(ics)%irr + chcs_y(iru)%cs(ics)%irr
          chcs_a(iru)%cs(ics)%div = chcs_a(iru)%cs(ics)%div + chcs_y(iru)%cs(ics)%div
          chcs_a(iru)%cs(ics)%water = chcs_a(iru)%cs(ics)%water + chcs_y(iru)%cs(ics)%water
          chcs_a(iru)%cs(ics)%conc = chcs_a(iru)%cs(ics)%conc + chcs_y(iru)%cs(ics)%conc
        enddo
        const = time%day_end_yr
        do ics=1,cs_db%num_cs !average mass and concentration
          chcs_y(iru)%cs(ics)%water = chcs_y(iru)%cs(ics)%water / const
          chcs_y(iru)%cs(ics)%conc = chcs_y(iru)%cs(ics)%conc / const
        enddo
        if (pco%cs_chn%y == "y") then
          write (6034,100) time%day, time%mo, time%day_mo, time%yrc, iru, ob(iob)%gis_id, & 
                           (chcs_y(iru)%cs(ics)%tot_in,ics=1,cs_db%num_cs), &
                           (chcs_y(iru)%cs(ics)%gw_in,ics=1,cs_db%num_cs), &
                           (chcs_y(iru)%cs(ics)%tot_out,ics=1,cs_db%num_cs), &
                           (chcs_y(iru)%cs(ics)%seep,ics=1,cs_db%num_cs), &
                           (chcs_y(iru)%cs(ics)%irr,ics=1,cs_db%num_cs), &
                           (chcs_y(iru)%cs(ics)%div,ics=1,cs_db%num_cs), &
                           (chcs_y(iru)%cs(ics)%water,ics=1,cs_db%num_cs), &
                           (chcs_y(iru)%cs(ics)%conc,ics=1,cs_db%num_cs)
          if (pco%csvout == "y") then
            write (6035,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, iru, ob(iob)%gis_id, & 
                                          (chcs_y(iru)%cs(ics)%tot_in,ics=1,cs_db%num_cs), &
                                          (chcs_y(iru)%cs(ics)%gw_in,ics=1,cs_db%num_cs), &
                                          (chcs_y(iru)%cs(ics)%tot_out,ics=1,cs_db%num_cs), &
                                          (chcs_y(iru)%cs(ics)%seep,ics=1,cs_db%num_cs), &
                                          (chcs_y(iru)%cs(ics)%irr,ics=1,cs_db%num_cs), &
                                          (chcs_y(iru)%cs(ics)%div,ics=1,cs_db%num_cs), &
                                          (chcs_y(iru)%cs(ics)%water,ics=1,cs_db%num_cs), &
                                          (chcs_y(iru)%cs(ics)%conc,ics=1,cs_db%num_cs)
          endif
        endif
        !zero out
        do ics = 1, cs_db%num_cs
          chcs_y(iru)%cs(ics)%tot_in = 0.
          chcs_y(iru)%cs(ics)%gw_in = 0.
          chcs_y(iru)%cs(ics)%tot_out = 0.
          chcs_y(iru)%cs(ics)%seep = 0.
          chcs_y(iru)%cs(ics)%irr = 0.
          chcs_y(iru)%cs(ics)%div = 0.
          chcs_y(iru)%cs(ics)%water = 0.
          chcs_y(iru)%cs(ics)%conc = 0.
        enddo
      endif
      
      !average annual print
      if (time%end_sim == 1 .and. pco%cs_chn%a == "y") then
        !calculate average annual values
        do ics = 1, cs_db%num_cs
          chcs_a(iru)%cs(ics)%tot_in = chcs_a(iru)%cs(ics)%tot_in / time%nbyr
          chcs_a(iru)%cs(ics)%gw_in = chcs_a(iru)%cs(ics)%gw_in / time%nbyr
          chcs_a(iru)%cs(ics)%tot_out = chcs_a(iru)%cs(ics)%tot_out / time%nbyr 
          chcs_a(iru)%cs(ics)%seep = chcs_a(iru)%cs(ics)%seep / time%nbyr 
          chcs_a(iru)%cs(ics)%irr = chcs_a(iru)%cs(ics)%irr / time%nbyr 
          chcs_a(iru)%cs(ics)%div = chcs_a(iru)%cs(ics)%div / time%nbyr 
          chcs_a(iru)%cs(ics)%water = chcs_a(iru)%cs(ics)%water / time%nbyr 
          chcs_a(iru)%cs(ics)%conc = chcs_a(iru)%cs(ics)%conc / time%nbyr
        enddo
        write (6036,100) time%day, time%mo, time%day_mo, time%yrc, iru, ob(iob)%gis_id, & 
                         (chcs_a(iru)%cs(ics)%tot_in,ics=1,cs_db%num_cs), &
                         (chcs_a(iru)%cs(ics)%gw_in,ics=1,cs_db%num_cs), &
                         (chcs_a(iru)%cs(ics)%tot_out,ics=1,cs_db%num_cs), &
                         (chcs_a(iru)%cs(ics)%seep,ics=1,cs_db%num_cs), &
                         (chcs_a(iru)%cs(ics)%irr,ics=1,cs_db%num_cs), &
                         (chcs_a(iru)%cs(ics)%div,ics=1,cs_db%num_cs), &
                         (chcs_a(iru)%cs(ics)%water,ics=1,cs_db%num_cs), &
                         (chcs_a(iru)%cs(ics)%conc,ics=1,cs_db%num_cs)
        if (pco%csvout == "y") then
          write (6037,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, iru, ob(iob)%gis_id, & 
                                        (chcs_a(iru)%cs(ics)%tot_in,ics=1,cs_db%num_cs), &
                                        (chcs_a(iru)%cs(ics)%gw_in,ics=1,cs_db%num_cs), &
                                        (chcs_a(iru)%cs(ics)%tot_out,ics=1,cs_db%num_cs), &
                                        (chcs_a(iru)%cs(ics)%seep,ics=1,cs_db%num_cs), &
                                        (chcs_a(iru)%cs(ics)%irr,ics=1,cs_db%num_cs), &
                                        (chcs_a(iru)%cs(ics)%div,ics=1,cs_db%num_cs), &
                                        (chcs_a(iru)%cs(ics)%water,ics=1,cs_db%num_cs), &
                                        (chcs_a(iru)%cs(ics)%conc,ics=1,cs_db%num_cs)
        endif
      endif

      
      return
      
100   format (4i6,2i8,500e18.7)      

      end subroutine ch_cs_output !rtb cs