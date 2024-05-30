      subroutine aqu_salt_output(iaq)
    
      use time_module
      use basin_module
      use aquifer_module
      use hydrograph_module, only : ob, sp_ob1
      use salt_aquifer
      use constituent_mass_module
      
      implicit none
      
      integer, intent (in) :: iaq        !             |
      real :: const                      !             |constant used for rate, days, etc
      integer :: iob                     !             |
      integer :: isalt
                         
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine outputs salt mass loadings and concentrations in aquifers
      
      iob = sp_ob1%aqu + iaq - 1
          
      !add daily values to monthly values
      do isalt=1,cs_db%num_salts
        asaltb_m(iaq)%salt(isalt)%rchrg = asaltb_m(iaq)%salt(isalt)%rchrg + asaltb_d(iaq)%salt(isalt)%rchrg
        asaltb_m(iaq)%salt(isalt)%seep = asaltb_m(iaq)%salt(isalt)%seep + asaltb_d(iaq)%salt(isalt)%seep
        asaltb_m(iaq)%salt(isalt)%saltgw = asaltb_m(iaq)%salt(isalt)%saltgw + asaltb_d(iaq)%salt(isalt)%saltgw
        asaltb_m(iaq)%salt(isalt)%irr = asaltb_m(iaq)%salt(isalt)%irr + asaltb_d(iaq)%salt(isalt)%irr
        asaltb_m(iaq)%salt(isalt)%div = asaltb_m(iaq)%salt(isalt)%div + asaltb_d(iaq)%salt(isalt)%div
        asaltb_m(iaq)%salt(isalt)%mass = asaltb_m(iaq)%salt(isalt)%mass + asaltb_d(iaq)%salt(isalt)%mass
        asaltb_m(iaq)%salt(isalt)%conc = asaltb_m(iaq)%salt(isalt)%conc + asaltb_d(iaq)%salt(isalt)%conc
      enddo
      asaltb_m(iaq)%salt(1)%diss = asaltb_m(iaq)%salt(1)%diss + asaltb_d(iaq)%salt(1)%diss
      
      !daily print
      if (pco%salt_aqu%d == "y") then
        write (5060,100) time%day, time%mo, time%day_mo, time%yrc, iaq, ob(iob)%gis_id, & 
                         (asaltb_d(iaq)%salt(isalt)%saltgw,isalt=1,cs_db%num_salts), &
                         (asaltb_d(iaq)%salt(isalt)%rchrg,isalt=1,cs_db%num_salts), &
                         (asaltb_d(iaq)%salt(isalt)%seep,isalt=1,cs_db%num_salts), &
                         (asaltb_d(iaq)%salt(isalt)%irr,isalt=1,cs_db%num_salts), &
                         (asaltb_d(iaq)%salt(isalt)%div,isalt=1,cs_db%num_salts), &
                         (asaltb_d(iaq)%salt(isalt)%mass,isalt=1,cs_db%num_salts), &
                         (asaltb_d(iaq)%salt(isalt)%conc,isalt=1,cs_db%num_salts), &
                          asaltb_d(iaq)%salt(1)%diss
        if (pco%csvout == "y") then
          write (5061,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, iaq, ob(iob)%gis_id, & 
																		    (asaltb_d(iaq)%salt(isalt)%saltgw,isalt=1,cs_db%num_salts), &
                                        (asaltb_d(iaq)%salt(isalt)%rchrg,isalt=1,cs_db%num_salts), &
                                        (asaltb_d(iaq)%salt(isalt)%seep,isalt=1,cs_db%num_salts), &
                                        (asaltb_d(iaq)%salt(isalt)%irr,isalt=1,cs_db%num_salts), &
                                        (asaltb_d(iaq)%salt(isalt)%div,isalt=1,cs_db%num_salts), &
                                        (asaltb_d(iaq)%salt(isalt)%mass,isalt=1,cs_db%num_salts), &
                                        (asaltb_d(iaq)%salt(isalt)%conc,isalt=1,cs_db%num_salts), &
                                         asaltb_d(iaq)%salt(1)%diss
        endif
      endif

      !monthly print
      if (time%end_mo == 1) then

        !add monthly values to yearly values
        do isalt = 1, cs_db%num_salts
          asaltb_y(iaq)%salt(isalt)%rchrg = asaltb_y(iaq)%salt(isalt)%rchrg + asaltb_m(iaq)%salt(isalt)%rchrg
          asaltb_y(iaq)%salt(isalt)%seep = asaltb_y(iaq)%salt(isalt)%seep + asaltb_m(iaq)%salt(isalt)%seep
          asaltb_y(iaq)%salt(isalt)%saltgw = asaltb_y(iaq)%salt(isalt)%saltgw + asaltb_m(iaq)%salt(isalt)%saltgw
          asaltb_y(iaq)%salt(isalt)%irr = asaltb_y(iaq)%salt(isalt)%irr + asaltb_m(iaq)%salt(isalt)%irr
          asaltb_y(iaq)%salt(isalt)%div = asaltb_y(iaq)%salt(isalt)%div + asaltb_m(iaq)%salt(isalt)%div
          asaltb_y(iaq)%salt(isalt)%mass = asaltb_y(iaq)%salt(isalt)%mass + asaltb_m(iaq)%salt(isalt)%mass
          asaltb_y(iaq)%salt(isalt)%conc = asaltb_y(iaq)%salt(isalt)%conc + asaltb_m(iaq)%salt(isalt)%conc
        enddo
        asaltb_y(iaq)%salt(1)%diss = asaltb_y(iaq)%salt(1)%diss + asaltb_m(iaq)%salt(1)%diss
        const = float (ndays(time%mo + 1) - ndays(time%mo))
        do isalt=1,cs_db%num_salts !average mass and concentration
          asaltb_m(iaq)%salt(isalt)%mass = asaltb_m(iaq)%salt(isalt)%mass / const
          asaltb_m(iaq)%salt(isalt)%conc = asaltb_m(iaq)%salt(isalt)%conc / const
        enddo
        if (pco%salt_aqu%m == "y") then
          write (5062,100) time%day, time%mo, time%day_mo, time%yrc, iaq, ob(iob)%gis_id, & 
                           (asaltb_m(iaq)%salt(isalt)%saltgw,isalt=1,cs_db%num_salts), &
                           (asaltb_m(iaq)%salt(isalt)%rchrg,isalt=1,cs_db%num_salts), &
                           (asaltb_m(iaq)%salt(isalt)%seep,isalt=1,cs_db%num_salts), &
                           (asaltb_m(iaq)%salt(isalt)%irr,isalt=1,cs_db%num_salts), &
                           (asaltb_m(iaq)%salt(isalt)%div,isalt=1,cs_db%num_salts), &
                           (asaltb_m(iaq)%salt(isalt)%mass,isalt=1,cs_db%num_salts), &
                           (asaltb_m(iaq)%salt(isalt)%conc,isalt=1,cs_db%num_salts), &
                            asaltb_m(iaq)%salt(1)%diss
          if (pco%csvout == "y") then
            write (5063,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, iaq, ob(iob)%gis_id, & 
                                          (asaltb_m(iaq)%salt(isalt)%saltgw,isalt=1,cs_db%num_salts), &
                                          (asaltb_m(iaq)%salt(isalt)%rchrg,isalt=1,cs_db%num_salts), &
                                          (asaltb_m(iaq)%salt(isalt)%seep,isalt=1,cs_db%num_salts), &
                                          (asaltb_m(iaq)%salt(isalt)%irr,isalt=1,cs_db%num_salts), &
                                          (asaltb_m(iaq)%salt(isalt)%div,isalt=1,cs_db%num_salts), &
                                          (asaltb_m(iaq)%salt(isalt)%mass,isalt=1,cs_db%num_salts), &
                                          (asaltb_m(iaq)%salt(isalt)%conc,isalt=1,cs_db%num_salts), &
                                           asaltb_m(iaq)%salt(1)%diss
          endif
        endif
        !zero out
        do isalt = 1, cs_db%num_salts
          asaltb_m(iaq)%salt(isalt)%saltgw = 0.
          asaltb_m(iaq)%salt(isalt)%rchrg = 0.
          asaltb_m(iaq)%salt(isalt)%seep = 0.
          asaltb_m(iaq)%salt(isalt)%irr = 0.
          asaltb_m(iaq)%salt(isalt)%div = 0.
          asaltb_m(iaq)%salt(isalt)%mass = 0.
          asaltb_m(iaq)%salt(isalt)%conc = 0.
          asaltb_m(iaq)%salt(1)%diss = 0.
        enddo
      endif
      
      !yearly print
      if (time%end_yr == 1) then
        !add yearly values to total values
        do isalt = 1, cs_db%num_salts
          asaltb_a(iaq)%salt(isalt)%rchrg = asaltb_a(iaq)%salt(isalt)%rchrg + asaltb_y(iaq)%salt(isalt)%rchrg
          asaltb_a(iaq)%salt(isalt)%seep = asaltb_a(iaq)%salt(isalt)%seep + asaltb_y(iaq)%salt(isalt)%seep
          asaltb_a(iaq)%salt(isalt)%saltgw = asaltb_a(iaq)%salt(isalt)%saltgw + asaltb_y(iaq)%salt(isalt)%saltgw
          asaltb_a(iaq)%salt(isalt)%irr = asaltb_a(iaq)%salt(isalt)%irr + asaltb_y(iaq)%salt(isalt)%irr
          asaltb_a(iaq)%salt(isalt)%div = asaltb_a(iaq)%salt(isalt)%div + asaltb_y(iaq)%salt(isalt)%div
          asaltb_a(iaq)%salt(isalt)%mass = asaltb_a(iaq)%salt(isalt)%mass + asaltb_y(iaq)%salt(isalt)%mass
          asaltb_a(iaq)%salt(isalt)%conc = asaltb_a(iaq)%salt(isalt)%conc + asaltb_y(iaq)%salt(isalt)%conc
        enddo
        asaltb_a(iaq)%salt(1)%diss = asaltb_a(iaq)%salt(1)%diss + asaltb_y(iaq)%salt(1)%diss
        const = time%day_end_yr
        do isalt=1,cs_db%num_salts !average mass concentration
          asaltb_y(iaq)%salt(isalt)%mass = asaltb_y(iaq)%salt(isalt)%mass / const
          asaltb_y(iaq)%salt(isalt)%conc = asaltb_y(iaq)%salt(isalt)%conc / const
        enddo
        if (pco%salt_aqu%y == "y") then
          write (5064,100) time%day, time%mo, time%day_mo, time%yrc, iaq, ob(iob)%gis_id, & 
                           (asaltb_y(iaq)%salt(isalt)%saltgw,isalt=1,cs_db%num_salts), &
                           (asaltb_y(iaq)%salt(isalt)%rchrg,isalt=1,cs_db%num_salts), &
                           (asaltb_y(iaq)%salt(isalt)%seep,isalt=1,cs_db%num_salts), &
                           (asaltb_y(iaq)%salt(isalt)%irr,isalt=1,cs_db%num_salts), &
                           (asaltb_y(iaq)%salt(isalt)%div,isalt=1,cs_db%num_salts), &
                           (asaltb_y(iaq)%salt(isalt)%mass,isalt=1,cs_db%num_salts), &
                           (asaltb_y(iaq)%salt(isalt)%conc,isalt=1,cs_db%num_salts), &
                            asaltb_y(iaq)%salt(1)%diss
          if (pco%csvout == "y") then
            write (5065,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, iaq, ob(iob)%gis_id, & 
                                          (asaltb_y(iaq)%salt(isalt)%saltgw,isalt=1,cs_db%num_salts), &
                                          (asaltb_y(iaq)%salt(isalt)%rchrg,isalt=1,cs_db%num_salts), &
                                          (asaltb_y(iaq)%salt(isalt)%seep,isalt=1,cs_db%num_salts), &
                                          (asaltb_y(iaq)%salt(isalt)%irr,isalt=1,cs_db%num_salts), &
                                          (asaltb_y(iaq)%salt(isalt)%div,isalt=1,cs_db%num_salts), &
                                          (asaltb_y(iaq)%salt(isalt)%mass,isalt=1,cs_db%num_salts), &
                                          (asaltb_y(iaq)%salt(isalt)%conc,isalt=1,cs_db%num_salts), &
                                           asaltb_y(iaq)%salt(1)%diss
          endif
        endif
        !zero out
        do isalt = 1, cs_db%num_salts
          asaltb_y(iaq)%salt(isalt)%saltgw = 0.
          asaltb_y(iaq)%salt(isalt)%rchrg = 0.
          asaltb_y(iaq)%salt(isalt)%seep = 0.
          asaltb_y(iaq)%salt(isalt)%irr = 0.
          asaltb_y(iaq)%salt(isalt)%div = 0.
          asaltb_y(iaq)%salt(isalt)%mass = 0.
          asaltb_y(iaq)%salt(isalt)%conc = 0.
          asaltb_y(iaq)%salt(1)%diss = 0.
        enddo
      endif
      
      !average annual print
      if (time%end_sim == 1 .and. pco%salt_aqu%a == "y") then
        !calculate average annual values
        do isalt = 1, cs_db%num_salts
          asaltb_a(iaq)%salt(isalt)%rchrg = asaltb_a(iaq)%salt(isalt)%rchrg / time%nbyr
          asaltb_a(iaq)%salt(isalt)%seep = asaltb_a(iaq)%salt(isalt)%seep / time%nbyr
          asaltb_a(iaq)%salt(isalt)%saltgw = asaltb_a(iaq)%salt(isalt)%saltgw / time%nbyr
          asaltb_a(iaq)%salt(isalt)%irr = asaltb_a(iaq)%salt(isalt)%irr / time%nbyr
          asaltb_a(iaq)%salt(isalt)%div = asaltb_a(iaq)%salt(isalt)%div / time%nbyr
          asaltb_a(iaq)%salt(isalt)%mass = asaltb_a(iaq)%salt(isalt)%mass / time%nbyr
          asaltb_a(iaq)%salt(isalt)%conc = asaltb_a(iaq)%salt(isalt)%conc / time%nbyr
        enddo
        write (5066,100) time%day, time%mo, time%day_mo, time%yrc, iaq, ob(iob)%gis_id, & 
                         (asaltb_a(iaq)%salt(isalt)%saltgw,isalt=1,cs_db%num_salts), &
                         (asaltb_a(iaq)%salt(isalt)%rchrg,isalt=1,cs_db%num_salts), &
                         (asaltb_a(iaq)%salt(isalt)%seep,isalt=1,cs_db%num_salts), &
                         (asaltb_a(iaq)%salt(isalt)%irr,isalt=1,cs_db%num_salts), &
                         (asaltb_a(iaq)%salt(isalt)%div,isalt=1,cs_db%num_salts), &
                         (asaltb_a(iaq)%salt(isalt)%mass,isalt=1,cs_db%num_salts), &
                         (asaltb_a(iaq)%salt(isalt)%conc,isalt=1,cs_db%num_salts), &
                          asaltb_a(iaq)%salt(1)%diss
        if (pco%csvout == "y") then
          write (5067,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, iaq, ob(iob)%gis_id, & 
                                        (asaltb_a(iaq)%salt(isalt)%saltgw,isalt=1,cs_db%num_salts), &
                                        (asaltb_a(iaq)%salt(isalt)%rchrg,isalt=1,cs_db%num_salts), &
                                        (asaltb_a(iaq)%salt(isalt)%seep,isalt=1,cs_db%num_salts), &
                                        (asaltb_a(iaq)%salt(isalt)%irr,isalt=1,cs_db%num_salts), &
                                        (asaltb_a(iaq)%salt(isalt)%div,isalt=1,cs_db%num_salts), &
                                        (asaltb_a(iaq)%salt(isalt)%mass,isalt=1,cs_db%num_salts), &
                                        (asaltb_a(iaq)%salt(isalt)%conc,isalt=1,cs_db%num_salts), &
                                         asaltb_a(iaq)%salt(1)%diss
        endif
      endif

      
      return
      
100   format (4i6,2i8,500e18.7)      

      end subroutine aqu_salt_output