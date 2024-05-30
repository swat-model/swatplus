      subroutine ch_salt_output(jrch) !rtb salt
    
      use output_ls_pesticide_module
      use ch_salt_module
      use plant_module
      use plant_data_module
      use time_module
      use basin_module
      use output_landscape_module
      use constituent_mass_module
      use hydrograph_module, only : sp_ob1, ob
      
      implicit none
      
      integer, intent (in) :: jrch             !            |
      integer :: isalt                         !            |
      integer :: iru
      integer :: iob
      real :: const
                         
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine outputs salt mass in channels

      iru = jrch  !!! nbs
      
      iob = sp_ob1%chandeg + iru - 1
          
      !add daily values to monthly values
      do isalt = 1, cs_db%num_salts
        chsalt_m(iru)%salt(isalt)%tot_in = chsalt_m(iru)%salt(isalt)%tot_in + chsalt_d(iru)%salt(isalt)%tot_in
        chsalt_m(iru)%salt(isalt)%gw_in = chsalt_m(iru)%salt(isalt)%gw_in + chsalt_d(iru)%salt(isalt)%gw_in
        chsalt_m(iru)%salt(isalt)%tot_out = chsalt_m(iru)%salt(isalt)%tot_out + chsalt_d(iru)%salt(isalt)%tot_out
        chsalt_m(iru)%salt(isalt)%seep = chsalt_m(iru)%salt(isalt)%seep + chsalt_d(iru)%salt(isalt)%seep
        chsalt_m(iru)%salt(isalt)%irr = chsalt_m(iru)%salt(isalt)%irr + chsalt_d(iru)%salt(isalt)%irr
        chsalt_m(iru)%salt(isalt)%div = chsalt_m(iru)%salt(isalt)%div + chsalt_d(iru)%salt(isalt)%div
        chsalt_m(iru)%salt(isalt)%water = chsalt_m(iru)%salt(isalt)%water + chsalt_d(iru)%salt(isalt)%water
        chsalt_m(iru)%salt(isalt)%conc = chsalt_m(iru)%salt(isalt)%conc + chsalt_d(iru)%salt(isalt)%conc
      enddo
      
      !daily print
      if (pco%salt_chn%d == "y") then
        write (5030,100) time%day, time%mo, time%day_mo, time%yrc, iru, ob(iob)%gis_id, & 
                         (chsalt_d(iru)%salt(isalt)%tot_in,isalt=1,cs_db%num_salts), &
                         (chsalt_d(iru)%salt(isalt)%gw_in,isalt=1,cs_db%num_salts), &
                         (chsalt_d(iru)%salt(isalt)%tot_out,isalt=1,cs_db%num_salts), &
                         (chsalt_d(iru)%salt(isalt)%seep,isalt=1,cs_db%num_salts), &
                         (chsalt_d(iru)%salt(isalt)%irr,isalt=1,cs_db%num_salts), &
                         (chsalt_d(iru)%salt(isalt)%div,isalt=1,cs_db%num_salts), &
                         (chsalt_d(iru)%salt(isalt)%water,isalt=1,cs_db%num_salts), &
                         (chsalt_d(iru)%salt(isalt)%conc,isalt=1,cs_db%num_salts)
        if (pco%csvout == "y") then
          write (5031,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, iru, ob(iob)%gis_id, & 
                                        (chsalt_d(iru)%salt(isalt)%tot_in,isalt=1,cs_db%num_salts), &
                                        (chsalt_d(iru)%salt(isalt)%gw_in,isalt=1,cs_db%num_salts), &
                                        (chsalt_d(iru)%salt(isalt)%tot_out,isalt=1,cs_db%num_salts), &
                                        (chsalt_d(iru)%salt(isalt)%seep,isalt=1,cs_db%num_salts), &
                                        (chsalt_d(iru)%salt(isalt)%irr,isalt=1,cs_db%num_salts), &
                                        (chsalt_d(iru)%salt(isalt)%div,isalt=1,cs_db%num_salts), &
                                        (chsalt_d(iru)%salt(isalt)%water,isalt=1,cs_db%num_salts), &
                                        (chsalt_d(iru)%salt(isalt)%conc,isalt=1,cs_db%num_salts)
        endif
      endif

      !monthly print
      if (time%end_mo == 1) then

        !add monthly values to yearly values
        do isalt = 1, cs_db%num_salts
          chsalt_y(iru)%salt(isalt)%tot_in = chsalt_y(iru)%salt(isalt)%tot_in + chsalt_m(iru)%salt(isalt)%tot_in
          chsalt_y(iru)%salt(isalt)%gw_in = chsalt_y(iru)%salt(isalt)%gw_in + chsalt_m(iru)%salt(isalt)%gw_in
          chsalt_y(iru)%salt(isalt)%tot_out = chsalt_y(iru)%salt(isalt)%tot_out + chsalt_m(iru)%salt(isalt)%tot_out
          chsalt_y(iru)%salt(isalt)%seep = chsalt_y(iru)%salt(isalt)%seep + chsalt_m(iru)%salt(isalt)%seep
          chsalt_y(iru)%salt(isalt)%irr = chsalt_y(iru)%salt(isalt)%irr + chsalt_m(iru)%salt(isalt)%irr
          chsalt_y(iru)%salt(isalt)%div = chsalt_y(iru)%salt(isalt)%div + chsalt_m(iru)%salt(isalt)%div
          chsalt_y(iru)%salt(isalt)%water = chsalt_y(iru)%salt(isalt)%water + chsalt_m(iru)%salt(isalt)%water
          chsalt_y(iru)%salt(isalt)%conc = chsalt_y(iru)%salt(isalt)%conc + chsalt_m(iru)%salt(isalt)%conc
        enddo
        const = float (ndays(time%mo + 1) - ndays(time%mo))
        do isalt=1,cs_db%num_salts !average mass and concentration
          chsalt_m(iru)%salt(isalt)%water = chsalt_m(iru)%salt(isalt)%water / const
          chsalt_m(iru)%salt(isalt)%conc = chsalt_m(iru)%salt(isalt)%conc / const
        enddo
        if (pco%salt_chn%m == "y") then
          write (5032,100) time%day, time%mo, time%day_mo, time%yrc, iru, ob(iob)%gis_id, & 
                           (chsalt_m(iru)%salt(isalt)%tot_in,isalt=1,cs_db%num_salts), &
                           (chsalt_m(iru)%salt(isalt)%gw_in,isalt=1,cs_db%num_salts), &
                           (chsalt_m(iru)%salt(isalt)%tot_out,isalt=1,cs_db%num_salts), &
                           (chsalt_m(iru)%salt(isalt)%seep,isalt=1,cs_db%num_salts), &
                           (chsalt_m(iru)%salt(isalt)%irr,isalt=1,cs_db%num_salts), &
                           (chsalt_m(iru)%salt(isalt)%div,isalt=1,cs_db%num_salts), &
                           (chsalt_m(iru)%salt(isalt)%water,isalt=1,cs_db%num_salts), &
                           (chsalt_m(iru)%salt(isalt)%conc,isalt=1,cs_db%num_salts)
          if (pco%csvout == "y") then
            write (5033,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, iru, ob(iob)%gis_id, & 
                                          (chsalt_m(iru)%salt(isalt)%tot_in,isalt=1,cs_db%num_salts), &
                                          (chsalt_m(iru)%salt(isalt)%gw_in,isalt=1,cs_db%num_salts), &
                                          (chsalt_m(iru)%salt(isalt)%tot_out,isalt=1,cs_db%num_salts), &
                                          (chsalt_m(iru)%salt(isalt)%seep,isalt=1,cs_db%num_salts), &
                                          (chsalt_m(iru)%salt(isalt)%irr,isalt=1,cs_db%num_salts), &
                                          (chsalt_m(iru)%salt(isalt)%div,isalt=1,cs_db%num_salts), &
                                          (chsalt_m(iru)%salt(isalt)%water,isalt=1,cs_db%num_salts), &
                                          (chsalt_m(iru)%salt(isalt)%conc,isalt=1,cs_db%num_salts)
          endif
        endif
        !zero out
        do isalt = 1, cs_db%num_salts
          chsalt_m(iru)%salt(isalt)%tot_in = 0.
          chsalt_m(iru)%salt(isalt)%gw_in = 0.
          chsalt_m(iru)%salt(isalt)%tot_out = 0.
          chsalt_m(iru)%salt(isalt)%seep = 0.
          chsalt_m(iru)%salt(isalt)%irr = 0.
          chsalt_m(iru)%salt(isalt)%div = 0.
          chsalt_m(iru)%salt(isalt)%water = 0.
          chsalt_m(iru)%salt(isalt)%conc = 0.
        enddo
      endif
      
      !yearly print
      if (time%end_yr == 1) then
        !add yearly values to total values
        do isalt = 1, cs_db%num_salts
          chsalt_a(iru)%salt(isalt)%tot_in = chsalt_a(iru)%salt(isalt)%tot_in + chsalt_y(iru)%salt(isalt)%tot_in
          chsalt_a(iru)%salt(isalt)%gw_in = chsalt_a(iru)%salt(isalt)%gw_in + chsalt_y(iru)%salt(isalt)%gw_in
          chsalt_a(iru)%salt(isalt)%tot_out = chsalt_a(iru)%salt(isalt)%tot_out + chsalt_y(iru)%salt(isalt)%tot_out
          chsalt_a(iru)%salt(isalt)%seep = chsalt_a(iru)%salt(isalt)%seep + chsalt_y(iru)%salt(isalt)%seep
          chsalt_a(iru)%salt(isalt)%irr = chsalt_a(iru)%salt(isalt)%irr + chsalt_y(iru)%salt(isalt)%irr
          chsalt_a(iru)%salt(isalt)%div = chsalt_a(iru)%salt(isalt)%div + chsalt_y(iru)%salt(isalt)%div
          chsalt_a(iru)%salt(isalt)%water = chsalt_a(iru)%salt(isalt)%water + chsalt_y(iru)%salt(isalt)%water
          chsalt_a(iru)%salt(isalt)%conc = chsalt_a(iru)%salt(isalt)%conc + chsalt_y(iru)%salt(isalt)%conc
        enddo
        const = time%day_end_yr
        do isalt=1,cs_db%num_salts !average mass and concentration
          chsalt_y(iru)%salt(isalt)%water = chsalt_y(iru)%salt(isalt)%water / const
          chsalt_y(iru)%salt(isalt)%conc = chsalt_y(iru)%salt(isalt)%conc / const
        enddo
        if (pco%salt_chn%y == "y") then
          write (5034,100) time%day, time%mo, time%day_mo, time%yrc, iru, ob(iob)%gis_id, & 
                           (chsalt_y(iru)%salt(isalt)%tot_in,isalt=1,cs_db%num_salts), &
                           (chsalt_y(iru)%salt(isalt)%gw_in,isalt=1,cs_db%num_salts), &
                           (chsalt_y(iru)%salt(isalt)%tot_out,isalt=1,cs_db%num_salts), &
                           (chsalt_y(iru)%salt(isalt)%seep,isalt=1,cs_db%num_salts), &
                           (chsalt_y(iru)%salt(isalt)%irr,isalt=1,cs_db%num_salts), &
                           (chsalt_y(iru)%salt(isalt)%div,isalt=1,cs_db%num_salts), &
                           (chsalt_y(iru)%salt(isalt)%water,isalt=1,cs_db%num_salts), &
                           (chsalt_y(iru)%salt(isalt)%conc,isalt=1,cs_db%num_salts)
          if (pco%csvout == "y") then
            write (5035,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, iru, ob(iob)%gis_id, & 
                                          (chsalt_y(iru)%salt(isalt)%tot_in,isalt=1,cs_db%num_salts), &
                                          (chsalt_y(iru)%salt(isalt)%gw_in,isalt=1,cs_db%num_salts), &
                                          (chsalt_y(iru)%salt(isalt)%tot_out,isalt=1,cs_db%num_salts), &
                                          (chsalt_y(iru)%salt(isalt)%seep,isalt=1,cs_db%num_salts), &
                                          (chsalt_y(iru)%salt(isalt)%irr,isalt=1,cs_db%num_salts), &
                                          (chsalt_y(iru)%salt(isalt)%div,isalt=1,cs_db%num_salts), &
                                          (chsalt_y(iru)%salt(isalt)%water,isalt=1,cs_db%num_salts), &
                                          (chsalt_y(iru)%salt(isalt)%conc,isalt=1,cs_db%num_salts)
          endif
        endif
        !zero out
        do isalt = 1, cs_db%num_salts
          chsalt_y(iru)%salt(isalt)%tot_in = 0.
          chsalt_y(iru)%salt(isalt)%gw_in = 0.
          chsalt_y(iru)%salt(isalt)%tot_out = 0.
          chsalt_y(iru)%salt(isalt)%seep = 0.
          chsalt_y(iru)%salt(isalt)%irr = 0.
          chsalt_y(iru)%salt(isalt)%div = 0.
          chsalt_y(iru)%salt(isalt)%water = 0.
          chsalt_y(iru)%salt(isalt)%conc = 0.
        enddo
      endif
      
      !average annual print
      if (time%end_sim == 1 .and. pco%salt_chn%a == "y") then
        !calculate average annual values
        do isalt = 1, cs_db%num_salts
          chsalt_a(iru)%salt(isalt)%tot_in = chsalt_a(iru)%salt(isalt)%tot_in / time%nbyr
          chsalt_a(iru)%salt(isalt)%gw_in = chsalt_a(iru)%salt(isalt)%gw_in / time%nbyr
          chsalt_a(iru)%salt(isalt)%tot_out = chsalt_a(iru)%salt(isalt)%tot_out / time%nbyr 
          chsalt_a(iru)%salt(isalt)%seep = chsalt_a(iru)%salt(isalt)%seep / time%nbyr 
          chsalt_a(iru)%salt(isalt)%irr = chsalt_a(iru)%salt(isalt)%irr / time%nbyr 
          chsalt_a(iru)%salt(isalt)%div = chsalt_a(iru)%salt(isalt)%div / time%nbyr 
          chsalt_a(iru)%salt(isalt)%water = chsalt_a(iru)%salt(isalt)%water / time%nbyr 
          chsalt_a(iru)%salt(isalt)%conc = chsalt_a(iru)%salt(isalt)%conc / time%nbyr
        enddo
        write (5036,100) time%day, time%mo, time%day_mo, time%yrc, iru, ob(iob)%gis_id, & 
                         (chsalt_a(iru)%salt(isalt)%tot_in,isalt=1,cs_db%num_salts), &
                         (chsalt_a(iru)%salt(isalt)%gw_in,isalt=1,cs_db%num_salts), &
                         (chsalt_a(iru)%salt(isalt)%tot_out,isalt=1,cs_db%num_salts), &
                         (chsalt_a(iru)%salt(isalt)%seep,isalt=1,cs_db%num_salts), &
                         (chsalt_a(iru)%salt(isalt)%irr,isalt=1,cs_db%num_salts), &
                         (chsalt_a(iru)%salt(isalt)%div,isalt=1,cs_db%num_salts), &
                         (chsalt_a(iru)%salt(isalt)%water,isalt=1,cs_db%num_salts), &
                         (chsalt_a(iru)%salt(isalt)%conc,isalt=1,cs_db%num_salts)
        if (pco%csvout == "y") then
          write (5037,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, iru, ob(iob)%gis_id, & 
                                        (chsalt_a(iru)%salt(isalt)%tot_in,isalt=1,cs_db%num_salts), &
                                        (chsalt_a(iru)%salt(isalt)%gw_in,isalt=1,cs_db%num_salts), &
                                        (chsalt_a(iru)%salt(isalt)%tot_out,isalt=1,cs_db%num_salts), &
                                        (chsalt_a(iru)%salt(isalt)%seep,isalt=1,cs_db%num_salts), &
                                        (chsalt_a(iru)%salt(isalt)%irr,isalt=1,cs_db%num_salts), &
                                        (chsalt_a(iru)%salt(isalt)%div,isalt=1,cs_db%num_salts), &
                                        (chsalt_a(iru)%salt(isalt)%water,isalt=1,cs_db%num_salts), &
                                        (chsalt_a(iru)%salt(isalt)%conc,isalt=1,cs_db%num_salts)
        endif
      endif

      
      return
      
100   format (4i6,2i8,5000e18.7)      

      end subroutine ch_salt_output