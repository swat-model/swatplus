      subroutine aqu_cs_output(iaq) !rtb cs
    
      use time_module
      use basin_module
      use aquifer_module
      use hydrograph_module, only : ob, sp_ob1
      use cs_aquifer
      use constituent_mass_module
      
      implicit none
      
      integer, intent (in) :: iaq        !             |
      real :: const                      !             |constant used for rate, days, etc
      integer :: iob                     !             |
      integer :: ics
                         
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine outputs constituent mass loadings and concentrations in aquifers

      
      iob = sp_ob1%aqu + iaq - 1
          
      !add daily values to monthly values
      do ics=1,cs_db%num_cs
        acsb_m(iaq)%cs(ics)%csgw = acsb_m(iaq)%cs(ics)%csgw + acsb_d(iaq)%cs(ics)%csgw
        acsb_m(iaq)%cs(ics)%rchrg = acsb_m(iaq)%cs(ics)%rchrg + acsb_d(iaq)%cs(ics)%rchrg
        acsb_m(iaq)%cs(ics)%seep = acsb_m(iaq)%cs(ics)%seep + acsb_d(iaq)%cs(ics)%seep
        acsb_m(iaq)%cs(ics)%irr = acsb_m(iaq)%cs(ics)%irr + acsb_d(iaq)%cs(ics)%irr
        acsb_m(iaq)%cs(ics)%div = acsb_m(iaq)%cs(ics)%div + acsb_d(iaq)%cs(ics)%div
        acsb_m(iaq)%cs(ics)%sorb = acsb_m(iaq)%cs(ics)%sorb + acsb_d(iaq)%cs(ics)%sorb
        acsb_m(iaq)%cs(ics)%rctn = acsb_m(iaq)%cs(ics)%rctn + acsb_d(iaq)%cs(ics)%rctn
        acsb_m(iaq)%cs(ics)%mass = acsb_m(iaq)%cs(ics)%mass + acsb_d(iaq)%cs(ics)%mass
        acsb_m(iaq)%cs(ics)%conc = acsb_m(iaq)%cs(ics)%conc + acsb_d(iaq)%cs(ics)%conc
        acsb_m(iaq)%cs(ics)%srbd = acsb_m(iaq)%cs(ics)%srbd + acsb_d(iaq)%cs(ics)%srbd
      enddo
      
      !daily print
      if (pco%cs_aqu%d == "y") then
        write (6060,100) time%day, time%mo, time%day_mo, time%yrc, iaq, ob(iob)%gis_id, & 
                         (acsb_d(iaq)%cs(ics)%csgw,ics=1,cs_db%num_cs), &
                         (acsb_d(iaq)%cs(ics)%rchrg,ics=1,cs_db%num_cs), &
                         (acsb_d(iaq)%cs(ics)%seep,ics=1,cs_db%num_cs), &
                         (acsb_d(iaq)%cs(ics)%irr,ics=1,cs_db%num_cs), &
                         (acsb_d(iaq)%cs(ics)%div,ics=1,cs_db%num_cs), &
                         (acsb_d(iaq)%cs(ics)%sorb,ics=1,cs_db%num_cs), &
                         (acsb_d(iaq)%cs(ics)%rctn,ics=1,cs_db%num_cs), &
                         (acsb_d(iaq)%cs(ics)%mass,ics=1,cs_db%num_cs), &
                         (acsb_d(iaq)%cs(ics)%conc,ics=1,cs_db%num_cs), &
                         (acsb_d(iaq)%cs(ics)%srbd,ics=1,cs_db%num_cs)
        if (pco%csvout == "y") then
          write (6061,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, iaq, ob(iob)%gis_id, & 
																		   (acsb_d(iaq)%cs(ics)%csgw,ics=1,cs_db%num_cs), &
                                       (acsb_d(iaq)%cs(ics)%rchrg,ics=1,cs_db%num_cs), &
                                       (acsb_d(iaq)%cs(ics)%seep,ics=1,cs_db%num_cs), &
                                       (acsb_d(iaq)%cs(ics)%irr,ics=1,cs_db%num_cs), &
                                       (acsb_d(iaq)%cs(ics)%div,ics=1,cs_db%num_cs), &
                                       (acsb_d(iaq)%cs(ics)%sorb,ics=1,cs_db%num_cs), &
                                       (acsb_d(iaq)%cs(ics)%rctn,ics=1,cs_db%num_cs), &
                                       (acsb_d(iaq)%cs(ics)%mass,ics=1,cs_db%num_cs), &
                                       (acsb_d(iaq)%cs(ics)%conc,ics=1,cs_db%num_cs), &
                                       (acsb_d(iaq)%cs(ics)%srbd,ics=1,cs_db%num_cs)
        endif
      endif

      !monthly print
      if (time%end_mo == 1) then

        !add monthly values to yearly values
        do ics=1,cs_db%num_cs
          acsb_y(iaq)%cs(ics)%csgw = acsb_y(iaq)%cs(ics)%csgw + acsb_m(iaq)%cs(ics)%csgw
          acsb_y(iaq)%cs(ics)%rchrg = acsb_y(iaq)%cs(ics)%rchrg + acsb_m(iaq)%cs(ics)%rchrg
          acsb_y(iaq)%cs(ics)%seep = acsb_y(iaq)%cs(ics)%seep + acsb_m(iaq)%cs(ics)%seep
          acsb_y(iaq)%cs(ics)%irr = acsb_y(iaq)%cs(ics)%irr + acsb_m(iaq)%cs(ics)%irr
          acsb_y(iaq)%cs(ics)%div = acsb_y(iaq)%cs(ics)%div + acsb_m(iaq)%cs(ics)%div
          acsb_y(iaq)%cs(ics)%sorb = acsb_y(iaq)%cs(ics)%sorb + acsb_m(iaq)%cs(ics)%sorb
          acsb_y(iaq)%cs(ics)%rctn = acsb_y(iaq)%cs(ics)%rctn + acsb_m(iaq)%cs(ics)%rctn
          acsb_y(iaq)%cs(ics)%mass = acsb_y(iaq)%cs(ics)%mass + acsb_m(iaq)%cs(ics)%mass
          acsb_y(iaq)%cs(ics)%conc = acsb_y(iaq)%cs(ics)%conc + acsb_m(iaq)%cs(ics)%conc
          acsb_y(iaq)%cs(ics)%srbd = acsb_y(iaq)%cs(ics)%srbd + acsb_m(iaq)%cs(ics)%srbd
        enddo
        const = float (ndays(time%mo + 1) - ndays(time%mo))
        do ics=1,cs_db%num_cs !average mass, concentration and sorbed mass
          acsb_m(iaq)%cs(ics)%mass = acsb_m(iaq)%cs(ics)%mass / const
          acsb_m(iaq)%cs(ics)%conc = acsb_m(iaq)%cs(ics)%conc / const
          acsb_m(iaq)%cs(ics)%srbd = acsb_m(iaq)%cs(ics)%srbd / const
        enddo
        if (pco%cs_aqu%m == "y") then
          write (6062,100) time%day, time%mo, time%day_mo, time%yrc, iaq, ob(iob)%gis_id, & 
                           (acsb_m(iaq)%cs(ics)%csgw,ics=1,cs_db%num_cs), &
                           (acsb_m(iaq)%cs(ics)%rchrg,ics=1,cs_db%num_cs), &
                           (acsb_m(iaq)%cs(ics)%seep,ics=1,cs_db%num_cs), &
                           (acsb_m(iaq)%cs(ics)%irr,ics=1,cs_db%num_cs), &
                           (acsb_m(iaq)%cs(ics)%div,ics=1,cs_db%num_cs), &
                           (acsb_m(iaq)%cs(ics)%sorb,ics=1,cs_db%num_cs), &
                           (acsb_m(iaq)%cs(ics)%rctn,ics=1,cs_db%num_cs), &
                           (acsb_m(iaq)%cs(ics)%mass,ics=1,cs_db%num_cs), &
                           (acsb_m(iaq)%cs(ics)%conc,ics=1,cs_db%num_cs), &
                           (acsb_m(iaq)%cs(ics)%srbd,ics=1,cs_db%num_cs)
          if (pco%csvout == "y") then
            write (6063,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, iaq, ob(iob)%gis_id, & 
                                         (acsb_m(iaq)%cs(ics)%csgw,ics=1,cs_db%num_cs), &
                                         (acsb_m(iaq)%cs(ics)%rchrg,ics=1,cs_db%num_cs), &
                                         (acsb_m(iaq)%cs(ics)%seep,ics=1,cs_db%num_cs), &
                                         (acsb_m(iaq)%cs(ics)%irr,ics=1,cs_db%num_cs), &
                                         (acsb_m(iaq)%cs(ics)%div,ics=1,cs_db%num_cs), &
                                         (acsb_m(iaq)%cs(ics)%sorb,ics=1,cs_db%num_cs), &
                                         (acsb_m(iaq)%cs(ics)%rctn,ics=1,cs_db%num_cs), &
                                         (acsb_m(iaq)%cs(ics)%mass,ics=1,cs_db%num_cs), &
                                         (acsb_m(iaq)%cs(ics)%conc,ics=1,cs_db%num_cs), &
                                         (acsb_m(iaq)%cs(ics)%srbd,ics=1,cs_db%num_cs)
          endif
        endif
        !zero out
        do ics=1,cs_db%num_cs
          acsb_m(iaq)%cs(ics)%csgw = 0.
          acsb_m(iaq)%cs(ics)%rchrg = 0.
          acsb_m(iaq)%cs(ics)%seep = 0.
          acsb_m(iaq)%cs(ics)%irr = 0.
          acsb_m(iaq)%cs(ics)%div = 0.
          acsb_m(iaq)%cs(ics)%sorb = 0.
          acsb_m(iaq)%cs(ics)%rctn = 0.
          acsb_m(iaq)%cs(ics)%mass = 0.
          acsb_m(iaq)%cs(ics)%conc = 0.
          acsb_m(iaq)%cs(ics)%srbd = 0.
        enddo
      endif
      
      !yearly print
      if (time%end_yr == 1) then
        !add yearly values to total values
        do ics=1,cs_db%num_cs
          acsb_a(iaq)%cs(ics)%csgw = acsb_a(iaq)%cs(ics)%csgw + acsb_y(iaq)%cs(ics)%csgw
          acsb_a(iaq)%cs(ics)%rchrg = acsb_a(iaq)%cs(ics)%rchrg + acsb_y(iaq)%cs(ics)%rchrg
          acsb_a(iaq)%cs(ics)%seep = acsb_a(iaq)%cs(ics)%seep + acsb_y(iaq)%cs(ics)%seep
          acsb_a(iaq)%cs(ics)%irr = acsb_a(iaq)%cs(ics)%irr + acsb_y(iaq)%cs(ics)%irr
          acsb_a(iaq)%cs(ics)%div = acsb_a(iaq)%cs(ics)%div + acsb_y(iaq)%cs(ics)%div
          acsb_a(iaq)%cs(ics)%sorb = acsb_a(iaq)%cs(ics)%sorb + acsb_y(iaq)%cs(ics)%sorb
          acsb_a(iaq)%cs(ics)%rctn = acsb_a(iaq)%cs(ics)%rctn + acsb_y(iaq)%cs(ics)%rctn
          acsb_a(iaq)%cs(ics)%mass = acsb_a(iaq)%cs(ics)%mass + acsb_y(iaq)%cs(ics)%mass
          acsb_a(iaq)%cs(ics)%conc = acsb_a(iaq)%cs(ics)%conc + acsb_y(iaq)%cs(ics)%conc
          acsb_a(iaq)%cs(ics)%srbd = acsb_a(iaq)%cs(ics)%srbd + acsb_y(iaq)%cs(ics)%srbd
        enddo
        const = time%day_end_yr
        do ics=1,cs_db%num_cs !average mass, concentration and sorbed mass
          acsb_y(iaq)%cs(ics)%mass = acsb_y(iaq)%cs(ics)%mass / const
          acsb_y(iaq)%cs(ics)%conc = acsb_y(iaq)%cs(ics)%conc / const
          acsb_y(iaq)%cs(ics)%srbd = acsb_y(iaq)%cs(ics)%srbd / const
        enddo
        if (pco%cs_aqu%y == "y") then
          write (6064,100) time%day, time%mo, time%day_mo, time%yrc, iaq, ob(iob)%gis_id, & 
                           (acsb_y(iaq)%cs(ics)%csgw,ics=1,cs_db%num_cs), &
                           (acsb_y(iaq)%cs(ics)%rchrg,ics=1,cs_db%num_cs), &
                           (acsb_y(iaq)%cs(ics)%seep,ics=1,cs_db%num_cs), &
                           (acsb_y(iaq)%cs(ics)%irr,ics=1,cs_db%num_cs), &
                           (acsb_y(iaq)%cs(ics)%div,ics=1,cs_db%num_cs), &
                           (acsb_y(iaq)%cs(ics)%sorb,ics=1,cs_db%num_cs), &
                           (acsb_y(iaq)%cs(ics)%rctn,ics=1,cs_db%num_cs), &
                           (acsb_y(iaq)%cs(ics)%mass,ics=1,cs_db%num_cs), &
                           (acsb_y(iaq)%cs(ics)%conc,ics=1,cs_db%num_cs), &
                           (acsb_y(iaq)%cs(ics)%srbd,ics=1,cs_db%num_cs)
          if (pco%csvout == "y") then
            write (6065,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, iaq, ob(iob)%gis_id, & 
                                         (acsb_y(iaq)%cs(ics)%csgw,ics=1,cs_db%num_cs), &
                                         (acsb_y(iaq)%cs(ics)%rchrg,ics=1,cs_db%num_cs), &
                                         (acsb_y(iaq)%cs(ics)%seep,ics=1,cs_db%num_cs), &
                                         (acsb_y(iaq)%cs(ics)%irr,ics=1,cs_db%num_cs), &
                                         (acsb_y(iaq)%cs(ics)%div,ics=1,cs_db%num_cs), &
                                         (acsb_y(iaq)%cs(ics)%sorb,ics=1,cs_db%num_cs), &
                                         (acsb_y(iaq)%cs(ics)%rctn,ics=1,cs_db%num_cs), &
                                         (acsb_y(iaq)%cs(ics)%mass,ics=1,cs_db%num_cs), &
                                         (acsb_y(iaq)%cs(ics)%conc,ics=1,cs_db%num_cs), &
                                         (acsb_y(iaq)%cs(ics)%srbd,ics=1,cs_db%num_cs)
          endif
        endif
        !zero out
        do ics=1,cs_db%num_cs
          acsb_y(iaq)%cs(ics)%csgw = 0.
          acsb_y(iaq)%cs(ics)%rchrg = 0.
          acsb_y(iaq)%cs(ics)%seep = 0.
          acsb_y(iaq)%cs(ics)%irr = 0.
          acsb_y(iaq)%cs(ics)%div = 0.
          acsb_y(iaq)%cs(ics)%sorb = 0.
          acsb_y(iaq)%cs(ics)%rctn = 0.
          acsb_y(iaq)%cs(ics)%mass = 0.
          acsb_y(iaq)%cs(ics)%conc = 0.
          acsb_y(iaq)%cs(ics)%srbd = 0.
        enddo
      endif
      
      !average annual print
      if (time%end_sim == 1 .and. pco%cs_aqu%a == "y") then
        !calculate average annual values
        do ics=1,cs_db%num_cs
          acsb_a(iaq)%cs(ics)%csgw = acsb_a(iaq)%cs(ics)%csgw / time%nbyr
          acsb_a(iaq)%cs(ics)%rchrg = acsb_a(iaq)%cs(ics)%rchrg / time%nbyr
          acsb_a(iaq)%cs(ics)%seep = acsb_a(iaq)%cs(ics)%seep / time%nbyr
          acsb_a(iaq)%cs(ics)%irr = acsb_a(iaq)%cs(ics)%irr / time%nbyr
          acsb_a(iaq)%cs(ics)%div = acsb_a(iaq)%cs(ics)%div / time%nbyr
          acsb_a(iaq)%cs(ics)%sorb = acsb_a(iaq)%cs(ics)%sorb / time%nbyr
          acsb_a(iaq)%cs(ics)%rctn = acsb_a(iaq)%cs(ics)%rctn / time%nbyr
          acsb_a(iaq)%cs(ics)%mass = acsb_a(iaq)%cs(ics)%mass / time%nbyr
          acsb_a(iaq)%cs(ics)%conc = acsb_a(iaq)%cs(ics)%conc / time%nbyr
          acsb_a(iaq)%cs(ics)%srbd = acsb_a(iaq)%cs(ics)%srbd / time%nbyr
        enddo
        write (6066,100) time%day, time%mo, time%day_mo, time%yrc, iaq, ob(iob)%gis_id, & 
                        (acsb_a(iaq)%cs(ics)%csgw,ics=1,cs_db%num_cs), &
                        (acsb_a(iaq)%cs(ics)%rchrg,ics=1,cs_db%num_cs), &
                        (acsb_a(iaq)%cs(ics)%seep,ics=1,cs_db%num_cs), &
                        (acsb_a(iaq)%cs(ics)%irr,ics=1,cs_db%num_cs), &
                        (acsb_a(iaq)%cs(ics)%div,ics=1,cs_db%num_cs), &
                        (acsb_a(iaq)%cs(ics)%sorb,ics=1,cs_db%num_cs), &
                        (acsb_a(iaq)%cs(ics)%rctn,ics=1,cs_db%num_cs), &
                        (acsb_a(iaq)%cs(ics)%mass,ics=1,cs_db%num_cs), &
                        (acsb_a(iaq)%cs(ics)%conc,ics=1,cs_db%num_cs), &
                        (acsb_a(iaq)%cs(ics)%srbd,ics=1,cs_db%num_cs)
        if (pco%csvout == "y") then
          write (6067,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, iaq, ob(iob)%gis_id, & 
                                        (acsb_a(iaq)%cs(ics)%csgw,ics=1,cs_db%num_cs), &
                                        (acsb_a(iaq)%cs(ics)%rchrg,ics=1,cs_db%num_cs), &
                                        (acsb_a(iaq)%cs(ics)%seep,ics=1,cs_db%num_cs), &
                                        (acsb_a(iaq)%cs(ics)%irr,ics=1,cs_db%num_cs), &
                                        (acsb_a(iaq)%cs(ics)%div,ics=1,cs_db%num_cs), &
                                        (acsb_a(iaq)%cs(ics)%sorb,ics=1,cs_db%num_cs), &
                                        (acsb_a(iaq)%cs(ics)%rctn,ics=1,cs_db%num_cs), &
                                        (acsb_a(iaq)%cs(ics)%mass,ics=1,cs_db%num_cs), &
                                        (acsb_a(iaq)%cs(ics)%conc,ics=1,cs_db%num_cs), &
                                        (acsb_a(iaq)%cs(ics)%srbd,ics=1,cs_db%num_cs)
        endif
      endif

      
      return
      
100   format (4i6,2i8,500e18.7)      

      end subroutine aqu_cs_output