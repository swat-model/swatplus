      subroutine hru_cs_output(ihru) !rtb cs
    
      use time_module
      use basin_module
      use hydrograph_module, only : ob, sp_ob1
      use cs_module
      use constituent_mass_module
      
      implicit none
      
      integer, intent (in) :: ihru       !             |HRU counter
      integer :: j                       !             |HRU number
      real :: const                      !             |constant used for rate, days, etc
      integer :: iob                     !             |
      integer :: ics                     !						 |constituent ion counter
                         
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine outputs constituent mass loadings and concentrations from HRUs

      j = ihru
      iob = sp_ob1%hru + j - 1
          
      !add daily values to monthly values
      do ics=1,cs_db%num_cs
        hcsb_m(j)%cs(ics)%soil = hcsb_m(j)%cs(ics)%soil + hcsb_d(j)%cs(ics)%soil
        hcsb_m(j)%cs(ics)%surq = hcsb_m(j)%cs(ics)%surq + hcsb_d(j)%cs(ics)%surq
        hcsb_m(j)%cs(ics)%sedm = hcsb_m(j)%cs(ics)%sedm + hcsb_d(j)%cs(ics)%sedm
        hcsb_m(j)%cs(ics)%latq = hcsb_m(j)%cs(ics)%latq + hcsb_d(j)%cs(ics)%latq
        hcsb_m(j)%cs(ics)%urbq = hcsb_m(j)%cs(ics)%urbq + hcsb_d(j)%cs(ics)%urbq
        hcsb_m(j)%cs(ics)%wetq = hcsb_m(j)%cs(ics)%wetq + hcsb_d(j)%cs(ics)%wetq
        hcsb_m(j)%cs(ics)%tile = hcsb_m(j)%cs(ics)%tile + hcsb_d(j)%cs(ics)%tile
        hcsb_m(j)%cs(ics)%perc = hcsb_m(j)%cs(ics)%perc + hcsb_d(j)%cs(ics)%perc
        hcsb_m(j)%cs(ics)%wtsp = hcsb_m(j)%cs(ics)%wtsp + hcsb_d(j)%cs(ics)%wtsp
        hcsb_m(j)%cs(ics)%irsw = hcsb_m(j)%cs(ics)%irsw + hcsb_d(j)%cs(ics)%irsw
        hcsb_m(j)%cs(ics)%irgw = hcsb_m(j)%cs(ics)%irgw + hcsb_d(j)%cs(ics)%irgw
        hcsb_m(j)%cs(ics)%irwo = hcsb_m(j)%cs(ics)%irwo + hcsb_d(j)%cs(ics)%irwo
        hcsb_m(j)%cs(ics)%rain = hcsb_m(j)%cs(ics)%rain + hcsb_d(j)%cs(ics)%rain
        hcsb_m(j)%cs(ics)%dryd = hcsb_m(j)%cs(ics)%dryd + hcsb_d(j)%cs(ics)%dryd
        hcsb_m(j)%cs(ics)%fert = hcsb_m(j)%cs(ics)%fert + hcsb_d(j)%cs(ics)%fert
        hcsb_m(j)%cs(ics)%uptk = hcsb_m(j)%cs(ics)%uptk + hcsb_d(j)%cs(ics)%uptk
        hcsb_m(j)%cs(ics)%rctn = hcsb_m(j)%cs(ics)%rctn + hcsb_d(j)%cs(ics)%rctn
        hcsb_m(j)%cs(ics)%sorb = hcsb_m(j)%cs(ics)%sorb + hcsb_d(j)%cs(ics)%sorb
        hcsb_m(j)%cs(ics)%conc = hcsb_m(j)%cs(ics)%conc + hcsb_d(j)%cs(ics)%conc
        hcsb_m(j)%cs(ics)%srbd = hcsb_m(j)%cs(ics)%srbd + hcsb_d(j)%cs(ics)%srbd
      enddo

      !daily print
      if (pco%cs_hru%d == "y") then
        write (6021,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, & 
                         (hcsb_d(j)%cs(ics)%soil,ics=1,cs_db%num_cs), & 
                         (hcsb_d(j)%cs(ics)%surq,ics=1,cs_db%num_cs), &
                         (hcsb_d(j)%cs(ics)%sedm,ics=1,cs_db%num_cs), &
                         (hcsb_d(j)%cs(ics)%latq,ics=1,cs_db%num_cs), &
                         (hcsb_d(j)%cs(ics)%urbq,ics=1,cs_db%num_cs), &
                         (hcsb_d(j)%cs(ics)%wetq,ics=1,cs_db%num_cs), &
                         (hcsb_d(j)%cs(ics)%tile,ics=1,cs_db%num_cs), &
                         (hcsb_d(j)%cs(ics)%perc,ics=1,cs_db%num_cs), &
                         (hcsb_d(j)%cs(ics)%wtsp,ics=1,cs_db%num_cs), &
                         (hcsb_d(j)%cs(ics)%irsw,ics=1,cs_db%num_cs), &
                         (hcsb_d(j)%cs(ics)%irgw,ics=1,cs_db%num_cs), &
                         (hcsb_d(j)%cs(ics)%irwo,ics=1,cs_db%num_cs), &
                         (hcsb_d(j)%cs(ics)%rain,ics=1,cs_db%num_cs), &
                         (hcsb_d(j)%cs(ics)%dryd,ics=1,cs_db%num_cs), &
                         (hcsb_d(j)%cs(ics)%fert,ics=1,cs_db%num_cs), &
                         (hcsb_d(j)%cs(ics)%uptk,ics=1,cs_db%num_cs), &
                         (hcsb_d(j)%cs(ics)%rctn,ics=1,cs_db%num_cs), &
                         (hcsb_d(j)%cs(ics)%sorb,ics=1,cs_db%num_cs), &
                         (hcsb_d(j)%cs(ics)%conc,ics=1,cs_db%num_cs), &
                         (hcsb_d(j)%cs(ics)%srbd,ics=1,cs_db%num_cs)
        if (pco%csvout == "y") then
          write (6022,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, & 
																		     (hcsb_d(j)%cs(ics)%soil,ics=1,cs_db%num_cs), & 
                                         (hcsb_d(j)%cs(ics)%surq,ics=1,cs_db%num_cs), &
                                         (hcsb_d(j)%cs(ics)%sedm,ics=1,cs_db%num_cs), &
                                         (hcsb_d(j)%cs(ics)%latq,ics=1,cs_db%num_cs), &
                                         (hcsb_d(j)%cs(ics)%urbq,ics=1,cs_db%num_cs), &
                                         (hcsb_d(j)%cs(ics)%wetq,ics=1,cs_db%num_cs), &
                                         (hcsb_d(j)%cs(ics)%tile,ics=1,cs_db%num_cs), &
                                         (hcsb_d(j)%cs(ics)%perc,ics=1,cs_db%num_cs), &
                                         (hcsb_d(j)%cs(ics)%wtsp,ics=1,cs_db%num_cs), &
                                         (hcsb_d(j)%cs(ics)%irsw,ics=1,cs_db%num_cs), &
                                         (hcsb_d(j)%cs(ics)%irgw,ics=1,cs_db%num_cs), &
                                         (hcsb_d(j)%cs(ics)%irwo,ics=1,cs_db%num_cs), &
                                         (hcsb_d(j)%cs(ics)%rain,ics=1,cs_db%num_cs), &
                                         (hcsb_d(j)%cs(ics)%dryd,ics=1,cs_db%num_cs), &
                                         (hcsb_d(j)%cs(ics)%fert,ics=1,cs_db%num_cs), &
                                         (hcsb_d(j)%cs(ics)%uptk,ics=1,cs_db%num_cs), &
                                         (hcsb_d(j)%cs(ics)%rctn,ics=1,cs_db%num_cs), &
                                         (hcsb_d(j)%cs(ics)%sorb,ics=1,cs_db%num_cs), &
                                         (hcsb_d(j)%cs(ics)%conc,ics=1,cs_db%num_cs), &
                                         (hcsb_d(j)%cs(ics)%srbd,ics=1,cs_db%num_cs)
        endif
      endif

      !monthly print
      if (time%end_mo == 1) then

        !add monthly values to yearly values
        do ics = 1, cs_db%num_cs
          hcsb_y(j)%cs(ics)%soil = hcsb_y(j)%cs(ics)%soil + hcsb_m(j)%cs(ics)%soil
          hcsb_y(j)%cs(ics)%surq = hcsb_y(j)%cs(ics)%surq + hcsb_m(j)%cs(ics)%surq
          hcsb_y(j)%cs(ics)%sedm = hcsb_y(j)%cs(ics)%sedm + hcsb_m(j)%cs(ics)%sedm
          hcsb_y(j)%cs(ics)%latq = hcsb_y(j)%cs(ics)%latq + hcsb_m(j)%cs(ics)%latq
          hcsb_y(j)%cs(ics)%urbq = hcsb_y(j)%cs(ics)%urbq + hcsb_m(j)%cs(ics)%urbq
          hcsb_y(j)%cs(ics)%wetq = hcsb_y(j)%cs(ics)%wetq + hcsb_m(j)%cs(ics)%wetq
          hcsb_y(j)%cs(ics)%tile = hcsb_y(j)%cs(ics)%tile + hcsb_m(j)%cs(ics)%tile
          hcsb_y(j)%cs(ics)%perc = hcsb_y(j)%cs(ics)%perc + hcsb_m(j)%cs(ics)%perc
          hcsb_y(j)%cs(ics)%wtsp = hcsb_y(j)%cs(ics)%wtsp + hcsb_m(j)%cs(ics)%wtsp
          hcsb_y(j)%cs(ics)%irsw = hcsb_y(j)%cs(ics)%irsw + hcsb_m(j)%cs(ics)%irsw
          hcsb_y(j)%cs(ics)%irgw = hcsb_y(j)%cs(ics)%irgw + hcsb_m(j)%cs(ics)%irgw
          hcsb_y(j)%cs(ics)%irwo = hcsb_y(j)%cs(ics)%irwo + hcsb_m(j)%cs(ics)%irwo
          hcsb_y(j)%cs(ics)%rain = hcsb_y(j)%cs(ics)%rain + hcsb_m(j)%cs(ics)%rain
          hcsb_y(j)%cs(ics)%dryd = hcsb_y(j)%cs(ics)%dryd + hcsb_m(j)%cs(ics)%dryd
          hcsb_y(j)%cs(ics)%fert = hcsb_y(j)%cs(ics)%fert + hcsb_m(j)%cs(ics)%fert
          hcsb_y(j)%cs(ics)%uptk = hcsb_y(j)%cs(ics)%uptk + hcsb_m(j)%cs(ics)%uptk
          hcsb_y(j)%cs(ics)%rctn = hcsb_y(j)%cs(ics)%rctn + hcsb_m(j)%cs(ics)%rctn
          hcsb_y(j)%cs(ics)%sorb = hcsb_y(j)%cs(ics)%sorb + hcsb_m(j)%cs(ics)%sorb
          hcsb_y(j)%cs(ics)%conc = hcsb_y(j)%cs(ics)%conc + hcsb_m(j)%cs(ics)%conc
          hcsb_y(j)%cs(ics)%srbd = hcsb_y(j)%cs(ics)%srbd + hcsb_m(j)%cs(ics)%srbd
        enddo
        const = float (ndays(time%mo + 1) - ndays(time%mo))
        do ics=1,cs_db%num_cs !average mass and concentration
          hcsb_m(j)%cs(ics)%soil = hcsb_m(j)%cs(ics)%soil / const
          hcsb_m(j)%cs(ics)%conc = hcsb_m(j)%cs(ics)%conc / const
          hcsb_m(j)%cs(ics)%srbd = hcsb_m(j)%cs(ics)%srbd / const
        enddo
        if (pco%cs_hru%m == "y") then
          write (6023,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, & 
                           (hcsb_m(j)%cs(ics)%soil,ics=1,cs_db%num_cs), & 
                           (hcsb_m(j)%cs(ics)%surq,ics=1,cs_db%num_cs), &
                           (hcsb_m(j)%cs(ics)%sedm,ics=1,cs_db%num_cs), &
                           (hcsb_m(j)%cs(ics)%latq,ics=1,cs_db%num_cs), &
                           (hcsb_m(j)%cs(ics)%urbq,ics=1,cs_db%num_cs), &
                           (hcsb_m(j)%cs(ics)%wetq,ics=1,cs_db%num_cs), &
                           (hcsb_m(j)%cs(ics)%tile,ics=1,cs_db%num_cs), &
                           (hcsb_m(j)%cs(ics)%perc,ics=1,cs_db%num_cs), &
                           (hcsb_m(j)%cs(ics)%wtsp,ics=1,cs_db%num_cs), &
                           (hcsb_m(j)%cs(ics)%irsw,ics=1,cs_db%num_cs), &
                           (hcsb_m(j)%cs(ics)%irgw,ics=1,cs_db%num_cs), &
                           (hcsb_m(j)%cs(ics)%irwo,ics=1,cs_db%num_cs), &
                           (hcsb_m(j)%cs(ics)%rain,ics=1,cs_db%num_cs), &
                           (hcsb_m(j)%cs(ics)%dryd,ics=1,cs_db%num_cs), &
                           (hcsb_m(j)%cs(ics)%fert,ics=1,cs_db%num_cs), &
                           (hcsb_m(j)%cs(ics)%uptk,ics=1,cs_db%num_cs), &
                           (hcsb_m(j)%cs(ics)%rctn,ics=1,cs_db%num_cs), &
                           (hcsb_m(j)%cs(ics)%sorb,ics=1,cs_db%num_cs), &
                           (hcsb_m(j)%cs(ics)%conc,ics=1,cs_db%num_cs), &
                           (hcsb_m(j)%cs(ics)%srbd,ics=1,cs_db%num_cs)
          if (pco%csvout == "y") then
            write (6024,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, & 
                                         (hcsb_m(j)%cs(ics)%soil,ics=1,cs_db%num_cs), & 
                                         (hcsb_m(j)%cs(ics)%surq,ics=1,cs_db%num_cs), &
                                         (hcsb_m(j)%cs(ics)%sedm,ics=1,cs_db%num_cs), &
                                         (hcsb_m(j)%cs(ics)%latq,ics=1,cs_db%num_cs), &
                                         (hcsb_m(j)%cs(ics)%urbq,ics=1,cs_db%num_cs), &
                                         (hcsb_m(j)%cs(ics)%wetq,ics=1,cs_db%num_cs), &
                                         (hcsb_m(j)%cs(ics)%tile,ics=1,cs_db%num_cs), &
                                         (hcsb_m(j)%cs(ics)%perc,ics=1,cs_db%num_cs), &
                                         (hcsb_m(j)%cs(ics)%wtsp,ics=1,cs_db%num_cs), &
                                         (hcsb_m(j)%cs(ics)%irsw,ics=1,cs_db%num_cs), &
                                         (hcsb_m(j)%cs(ics)%irgw,ics=1,cs_db%num_cs), &
                                         (hcsb_m(j)%cs(ics)%irwo,ics=1,cs_db%num_cs), &
                                         (hcsb_m(j)%cs(ics)%rain,ics=1,cs_db%num_cs), &
                                         (hcsb_m(j)%cs(ics)%dryd,ics=1,cs_db%num_cs), &
                                         (hcsb_m(j)%cs(ics)%fert,ics=1,cs_db%num_cs), &
                                         (hcsb_m(j)%cs(ics)%uptk,ics=1,cs_db%num_cs), &
                                         (hcsb_m(j)%cs(ics)%rctn,ics=1,cs_db%num_cs), &
                                         (hcsb_m(j)%cs(ics)%sorb,ics=1,cs_db%num_cs), &
                                         (hcsb_m(j)%cs(ics)%conc,ics=1,cs_db%num_cs), &
                                         (hcsb_m(j)%cs(ics)%srbd,ics=1,cs_db%num_cs)
          endif
        endif
        !zero out
        do ics = 1, cs_db%num_cs
          hcsb_m(j)%cs(ics)%soil = 0.
          hcsb_m(j)%cs(ics)%surq = 0.
          hcsb_m(j)%cs(ics)%sedm = 0.
          hcsb_m(j)%cs(ics)%latq = 0.
          hcsb_m(j)%cs(ics)%urbq = 0.
          hcsb_m(j)%cs(ics)%wetq = 0.
          hcsb_m(j)%cs(ics)%tile = 0.
          hcsb_m(j)%cs(ics)%perc = 0.
          hcsb_m(j)%cs(ics)%wtsp = 0.
          hcsb_m(j)%cs(ics)%irsw = 0.
          hcsb_m(j)%cs(ics)%irgw = 0.
          hcsb_m(j)%cs(ics)%irwo = 0.
          hcsb_m(j)%cs(ics)%rain = 0.
          hcsb_m(j)%cs(ics)%dryd = 0.
          hcsb_m(j)%cs(ics)%fert = 0.
          hcsb_m(j)%cs(ics)%uptk = 0.
          hcsb_m(j)%cs(ics)%rctn = 0.
          hcsb_m(j)%cs(ics)%sorb = 0.
          hcsb_m(j)%cs(ics)%conc = 0.
          hcsb_m(j)%cs(ics)%srbd = 0.
        enddo
      endif
      
      !yearly print
      if (time%end_yr == 1) then
        !add yearly values to total values
        do ics = 1, cs_db%num_cs
          hcsb_a(j)%cs(ics)%soil = hcsb_a(j)%cs(ics)%soil + hcsb_y(j)%cs(ics)%soil
          hcsb_a(j)%cs(ics)%surq = hcsb_a(j)%cs(ics)%surq + hcsb_y(j)%cs(ics)%surq
          hcsb_a(j)%cs(ics)%sedm = hcsb_a(j)%cs(ics)%sedm + hcsb_y(j)%cs(ics)%sedm
          hcsb_a(j)%cs(ics)%latq = hcsb_a(j)%cs(ics)%latq + hcsb_y(j)%cs(ics)%latq
          hcsb_a(j)%cs(ics)%urbq = hcsb_a(j)%cs(ics)%urbq + hcsb_y(j)%cs(ics)%urbq
          hcsb_a(j)%cs(ics)%wetq = hcsb_a(j)%cs(ics)%wetq + hcsb_y(j)%cs(ics)%wetq
          hcsb_a(j)%cs(ics)%tile = hcsb_a(j)%cs(ics)%tile + hcsb_y(j)%cs(ics)%tile
          hcsb_a(j)%cs(ics)%perc = hcsb_a(j)%cs(ics)%perc + hcsb_y(j)%cs(ics)%perc
          hcsb_a(j)%cs(ics)%wtsp = hcsb_a(j)%cs(ics)%wtsp + hcsb_y(j)%cs(ics)%wtsp
          hcsb_a(j)%cs(ics)%irsw = hcsb_a(j)%cs(ics)%irsw + hcsb_y(j)%cs(ics)%irsw
          hcsb_a(j)%cs(ics)%irgw = hcsb_a(j)%cs(ics)%irgw + hcsb_y(j)%cs(ics)%irgw
          hcsb_a(j)%cs(ics)%irwo = hcsb_a(j)%cs(ics)%irwo + hcsb_y(j)%cs(ics)%irwo
          hcsb_a(j)%cs(ics)%rain = hcsb_a(j)%cs(ics)%rain + hcsb_y(j)%cs(ics)%rain
          hcsb_a(j)%cs(ics)%dryd = hcsb_a(j)%cs(ics)%dryd + hcsb_y(j)%cs(ics)%dryd
          hcsb_a(j)%cs(ics)%fert = hcsb_a(j)%cs(ics)%fert + hcsb_y(j)%cs(ics)%fert
          hcsb_a(j)%cs(ics)%uptk = hcsb_a(j)%cs(ics)%uptk + hcsb_y(j)%cs(ics)%uptk
          hcsb_a(j)%cs(ics)%rctn = hcsb_a(j)%cs(ics)%rctn + hcsb_y(j)%cs(ics)%rctn
          hcsb_a(j)%cs(ics)%sorb = hcsb_a(j)%cs(ics)%sorb + hcsb_y(j)%cs(ics)%sorb
          hcsb_a(j)%cs(ics)%conc = hcsb_a(j)%cs(ics)%conc + hcsb_y(j)%cs(ics)%conc
          hcsb_a(j)%cs(ics)%srbd = hcsb_a(j)%cs(ics)%srbd + hcsb_y(j)%cs(ics)%srbd
        enddo
        const = time%day_end_yr
        do ics=1,cs_db%num_cs !average concentration
          hcsb_y(j)%cs(ics)%soil = hcsb_y(j)%cs(ics)%soil / const
          hcsb_y(j)%cs(ics)%conc = hcsb_y(j)%cs(ics)%conc / const
          hcsb_y(j)%cs(ics)%srbd = hcsb_y(j)%cs(ics)%srbd / const
        enddo
        if (pco%cs_hru%y == "y") then
          write (6025,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, & 
                           (hcsb_y(j)%cs(ics)%soil,ics=1,cs_db%num_cs), & 
                           (hcsb_y(j)%cs(ics)%surq,ics=1,cs_db%num_cs), &
                           (hcsb_y(j)%cs(ics)%sedm,ics=1,cs_db%num_cs), &
                           (hcsb_y(j)%cs(ics)%latq,ics=1,cs_db%num_cs), &
                           (hcsb_y(j)%cs(ics)%urbq,ics=1,cs_db%num_cs), &
                           (hcsb_y(j)%cs(ics)%wetq,ics=1,cs_db%num_cs), &
                           (hcsb_y(j)%cs(ics)%tile,ics=1,cs_db%num_cs), &
                           (hcsb_y(j)%cs(ics)%perc,ics=1,cs_db%num_cs), &
                           (hcsb_y(j)%cs(ics)%wtsp,ics=1,cs_db%num_cs), &
                           (hcsb_y(j)%cs(ics)%irsw,ics=1,cs_db%num_cs), &
                           (hcsb_y(j)%cs(ics)%irgw,ics=1,cs_db%num_cs), &
                           (hcsb_y(j)%cs(ics)%irwo,ics=1,cs_db%num_cs), &
                           (hcsb_y(j)%cs(ics)%rain,ics=1,cs_db%num_cs), &
                           (hcsb_y(j)%cs(ics)%dryd,ics=1,cs_db%num_cs), &
                           (hcsb_y(j)%cs(ics)%fert,ics=1,cs_db%num_cs), &
                           (hcsb_y(j)%cs(ics)%uptk,ics=1,cs_db%num_cs), &
                           (hcsb_y(j)%cs(ics)%rctn,ics=1,cs_db%num_cs), &
                           (hcsb_y(j)%cs(ics)%sorb,ics=1,cs_db%num_cs), &
                           (hcsb_y(j)%cs(ics)%conc,ics=1,cs_db%num_cs), &
                           (hcsb_y(j)%cs(ics)%srbd,ics=1,cs_db%num_cs)
          if (pco%csvout == "y") then
            write (6026,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, & 
                                         (hcsb_y(j)%cs(ics)%soil,ics=1,cs_db%num_cs), & 
                                         (hcsb_y(j)%cs(ics)%surq,ics=1,cs_db%num_cs), &
                                         (hcsb_y(j)%cs(ics)%sedm,ics=1,cs_db%num_cs), &
                                         (hcsb_y(j)%cs(ics)%latq,ics=1,cs_db%num_cs), &
                                         (hcsb_y(j)%cs(ics)%urbq,ics=1,cs_db%num_cs), &
                                         (hcsb_y(j)%cs(ics)%wetq,ics=1,cs_db%num_cs), &
                                         (hcsb_y(j)%cs(ics)%tile,ics=1,cs_db%num_cs), &
                                         (hcsb_y(j)%cs(ics)%perc,ics=1,cs_db%num_cs), &
                                         (hcsb_y(j)%cs(ics)%wtsp,ics=1,cs_db%num_cs), &
                                         (hcsb_y(j)%cs(ics)%irsw,ics=1,cs_db%num_cs), &
                                         (hcsb_y(j)%cs(ics)%irgw,ics=1,cs_db%num_cs), &
                                         (hcsb_y(j)%cs(ics)%irwo,ics=1,cs_db%num_cs), &
                                         (hcsb_y(j)%cs(ics)%rain,ics=1,cs_db%num_cs), &
                                         (hcsb_y(j)%cs(ics)%dryd,ics=1,cs_db%num_cs), &
                                         (hcsb_y(j)%cs(ics)%fert,ics=1,cs_db%num_cs), &
                                         (hcsb_y(j)%cs(ics)%uptk,ics=1,cs_db%num_cs), &
                                         (hcsb_y(j)%cs(ics)%rctn,ics=1,cs_db%num_cs), &
                                         (hcsb_y(j)%cs(ics)%sorb,ics=1,cs_db%num_cs), &
                                         (hcsb_y(j)%cs(ics)%conc,ics=1,cs_db%num_cs), &
                                         (hcsb_y(j)%cs(ics)%srbd,ics=1,cs_db%num_cs)
          endif
        endif
        !zero out
        do ics = 1, cs_db%num_cs
          hcsb_y(j)%cs(ics)%soil = 0.
          hcsb_y(j)%cs(ics)%surq = 0.
          hcsb_y(j)%cs(ics)%sedm = 0.
          hcsb_y(j)%cs(ics)%latq = 0.
          hcsb_y(j)%cs(ics)%urbq = 0.
          hcsb_y(j)%cs(ics)%wetq = 0.
          hcsb_y(j)%cs(ics)%tile = 0.
          hcsb_y(j)%cs(ics)%perc = 0.
          hcsb_y(j)%cs(ics)%wtsp = 0.
          hcsb_y(j)%cs(ics)%irsw = 0.
          hcsb_y(j)%cs(ics)%irgw = 0.
          hcsb_y(j)%cs(ics)%irwo = 0.
          hcsb_y(j)%cs(ics)%rain = 0.
          hcsb_y(j)%cs(ics)%dryd = 0.
          hcsb_y(j)%cs(ics)%fert = 0.
          hcsb_y(j)%cs(ics)%uptk = 0.
          hcsb_y(j)%cs(ics)%rctn = 0.
          hcsb_y(j)%cs(ics)%sorb = 0.
          hcsb_y(j)%cs(ics)%conc = 0.
          hcsb_y(j)%cs(ics)%srbd = 0.
        enddo
      endif
      
      !average annual print
      if (time%end_sim == 1 .and. pco%cs_hru%a == "y") then
        !calculate average annual values
        do ics= 1, cs_db%num_cs
          hcsb_a(j)%cs(ics)%soil = hcsb_a(j)%cs(ics)%soil / time%nbyr
          hcsb_a(j)%cs(ics)%surq = hcsb_a(j)%cs(ics)%surq / time%nbyr
          hcsb_a(j)%cs(ics)%sedm = hcsb_a(j)%cs(ics)%sedm / time%nbyr
          hcsb_a(j)%cs(ics)%latq = hcsb_a(j)%cs(ics)%latq / time%nbyr
          hcsb_a(j)%cs(ics)%urbq = hcsb_a(j)%cs(ics)%urbq / time%nbyr
          hcsb_a(j)%cs(ics)%wetq = hcsb_a(j)%cs(ics)%wetq / time%nbyr
          hcsb_a(j)%cs(ics)%tile = hcsb_a(j)%cs(ics)%tile / time%nbyr
          hcsb_a(j)%cs(ics)%perc = hcsb_a(j)%cs(ics)%perc / time%nbyr
          hcsb_a(j)%cs(ics)%wtsp = hcsb_a(j)%cs(ics)%wtsp / time%nbyr
          hcsb_a(j)%cs(ics)%irsw = hcsb_a(j)%cs(ics)%irsw / time%nbyr
          hcsb_a(j)%cs(ics)%irgw = hcsb_a(j)%cs(ics)%irgw / time%nbyr
          hcsb_a(j)%cs(ics)%irwo = hcsb_a(j)%cs(ics)%irwo / time%nbyr
          hcsb_a(j)%cs(ics)%rain = hcsb_a(j)%cs(ics)%rain / time%nbyr
          hcsb_a(j)%cs(ics)%dryd = hcsb_a(j)%cs(ics)%dryd / time%nbyr
          hcsb_a(j)%cs(ics)%fert = hcsb_a(j)%cs(ics)%fert / time%nbyr
          hcsb_a(j)%cs(ics)%uptk = hcsb_a(j)%cs(ics)%uptk / time%nbyr
          hcsb_a(j)%cs(ics)%rctn = hcsb_a(j)%cs(ics)%rctn / time%nbyr
          hcsb_a(j)%cs(ics)%sorb = hcsb_a(j)%cs(ics)%sorb / time%nbyr
          hcsb_a(j)%cs(ics)%conc = hcsb_a(j)%cs(ics)%conc / time%nbyr
          hcsb_a(j)%cs(ics)%srbd = hcsb_a(j)%cs(ics)%srbd / time%nbyr
        enddo
        write (6027,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, & 
                        (hcsb_a(j)%cs(ics)%soil,ics=1,cs_db%num_cs), & 
                        (hcsb_a(j)%cs(ics)%surq,ics=1,cs_db%num_cs), &
                        (hcsb_a(j)%cs(ics)%sedm,ics=1,cs_db%num_cs), &
                        (hcsb_a(j)%cs(ics)%latq,ics=1,cs_db%num_cs), &
                        (hcsb_a(j)%cs(ics)%urbq,ics=1,cs_db%num_cs), &
                        (hcsb_a(j)%cs(ics)%wetq,ics=1,cs_db%num_cs), &
                        (hcsb_a(j)%cs(ics)%tile,ics=1,cs_db%num_cs), &
                        (hcsb_a(j)%cs(ics)%perc,ics=1,cs_db%num_cs), &
                        (hcsb_a(j)%cs(ics)%wtsp,ics=1,cs_db%num_cs), &
                        (hcsb_a(j)%cs(ics)%irsw,ics=1,cs_db%num_cs), &
                        (hcsb_a(j)%cs(ics)%irgw,ics=1,cs_db%num_cs), &
                        (hcsb_a(j)%cs(ics)%irwo,ics=1,cs_db%num_cs), &
                        (hcsb_a(j)%cs(ics)%rain,ics=1,cs_db%num_cs), &
                        (hcsb_a(j)%cs(ics)%dryd,ics=1,cs_db%num_cs), &
                        (hcsb_a(j)%cs(ics)%fert,ics=1,cs_db%num_cs), &
                        (hcsb_a(j)%cs(ics)%uptk,ics=1,cs_db%num_cs), &
                        (hcsb_a(j)%cs(ics)%rctn,ics=1,cs_db%num_cs), &
                        (hcsb_a(j)%cs(ics)%sorb,ics=1,cs_db%num_cs), &
                        (hcsb_a(j)%cs(ics)%conc,ics=1,cs_db%num_cs), &
                        (hcsb_a(j)%cs(ics)%srbd,ics=1,cs_db%num_cs)
        if (pco%csvout == "y") then
          write (6028,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, & 
                                        (hcsb_a(j)%cs(ics)%soil,ics=1,cs_db%num_cs), & 
                                        (hcsb_a(j)%cs(ics)%surq,ics=1,cs_db%num_cs), &
                                        (hcsb_a(j)%cs(ics)%sedm,ics=1,cs_db%num_cs), &
                                        (hcsb_a(j)%cs(ics)%latq,ics=1,cs_db%num_cs), &
                                        (hcsb_a(j)%cs(ics)%urbq,ics=1,cs_db%num_cs), &
                                        (hcsb_a(j)%cs(ics)%wetq,ics=1,cs_db%num_cs), &
                                        (hcsb_a(j)%cs(ics)%tile,ics=1,cs_db%num_cs), &
                                        (hcsb_a(j)%cs(ics)%perc,ics=1,cs_db%num_cs), &
                                        (hcsb_a(j)%cs(ics)%wtsp,ics=1,cs_db%num_cs), &
                                        (hcsb_a(j)%cs(ics)%irsw,ics=1,cs_db%num_cs), &
                                        (hcsb_a(j)%cs(ics)%irgw,ics=1,cs_db%num_cs), &
                                        (hcsb_a(j)%cs(ics)%irwo,ics=1,cs_db%num_cs), &
                                        (hcsb_a(j)%cs(ics)%rain,ics=1,cs_db%num_cs), &
                                        (hcsb_a(j)%cs(ics)%dryd,ics=1,cs_db%num_cs), &
                                        (hcsb_a(j)%cs(ics)%fert,ics=1,cs_db%num_cs), &
                                        (hcsb_a(j)%cs(ics)%uptk,ics=1,cs_db%num_cs), &
                                        (hcsb_a(j)%cs(ics)%rctn,ics=1,cs_db%num_cs), &
                                        (hcsb_a(j)%cs(ics)%sorb,ics=1,cs_db%num_cs), &
                                        (hcsb_a(j)%cs(ics)%conc,ics=1,cs_db%num_cs), &
                                        (hcsb_a(j)%cs(ics)%srbd,ics=1,cs_db%num_cs)
        endif
      endif

      return
      
100   format (4i6,2i8,500e15.4)      

      end subroutine hru_cs_output