      subroutine hru_salt_output(ihru)
    
      use time_module
      use basin_module
      use hydrograph_module, only : ob, sp_ob1
      use salt_module
      use constituent_mass_module
      
      implicit none
      
      integer, intent (in) :: ihru       !             |HRU counter
      integer :: j                       !             |HRU number
      real :: const                      !             |constant used for rate, days, etc
      integer :: iob                     !             |
      integer :: isalt                   !						 |salt ion counter
                         
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine outputs salt mass loadings and concentrations from HRUs

      j = ihru
      iob = sp_ob1%hru + j - 1
      
      !add daily values to monthly values
      do isalt=1,cs_db%num_salts
        hsaltb_m(j)%salt(isalt)%soil = hsaltb_m(j)%salt(isalt)%soil + hsaltb_d(j)%salt(isalt)%soil
        hsaltb_m(j)%salt(isalt)%surq = hsaltb_m(j)%salt(isalt)%surq + hsaltb_d(j)%salt(isalt)%surq
        hsaltb_m(j)%salt(isalt)%latq = hsaltb_m(j)%salt(isalt)%latq + hsaltb_d(j)%salt(isalt)%latq
        hsaltb_m(j)%salt(isalt)%urbq = hsaltb_m(j)%salt(isalt)%urbq + hsaltb_d(j)%salt(isalt)%urbq
        hsaltb_m(j)%salt(isalt)%wetq = hsaltb_m(j)%salt(isalt)%wetq + hsaltb_d(j)%salt(isalt)%wetq
        hsaltb_m(j)%salt(isalt)%tile = hsaltb_m(j)%salt(isalt)%tile + hsaltb_d(j)%salt(isalt)%tile
        hsaltb_m(j)%salt(isalt)%perc = hsaltb_m(j)%salt(isalt)%perc + hsaltb_d(j)%salt(isalt)%perc
        hsaltb_m(j)%salt(isalt)%wtsp = hsaltb_m(j)%salt(isalt)%wtsp + hsaltb_d(j)%salt(isalt)%wtsp
        hsaltb_m(j)%salt(isalt)%irsw = hsaltb_m(j)%salt(isalt)%irsw + hsaltb_d(j)%salt(isalt)%irsw
        hsaltb_m(j)%salt(isalt)%irgw = hsaltb_m(j)%salt(isalt)%irgw + hsaltb_d(j)%salt(isalt)%irgw
        hsaltb_m(j)%salt(isalt)%irwo = hsaltb_m(j)%salt(isalt)%irwo + hsaltb_d(j)%salt(isalt)%irwo
        hsaltb_m(j)%salt(isalt)%rain = hsaltb_m(j)%salt(isalt)%rain + hsaltb_d(j)%salt(isalt)%rain
        hsaltb_m(j)%salt(isalt)%dryd = hsaltb_m(j)%salt(isalt)%dryd + hsaltb_d(j)%salt(isalt)%dryd
        hsaltb_m(j)%salt(isalt)%road = hsaltb_m(j)%salt(isalt)%road + hsaltb_d(j)%salt(isalt)%road
        hsaltb_m(j)%salt(isalt)%fert = hsaltb_m(j)%salt(isalt)%fert + hsaltb_d(j)%salt(isalt)%fert
        hsaltb_m(j)%salt(isalt)%amnd = hsaltb_m(j)%salt(isalt)%amnd + hsaltb_d(j)%salt(isalt)%amnd
        hsaltb_m(j)%salt(isalt)%uptk = hsaltb_m(j)%salt(isalt)%uptk + hsaltb_d(j)%salt(isalt)%uptk
        hsaltb_m(j)%salt(isalt)%conc = hsaltb_m(j)%salt(isalt)%conc + hsaltb_d(j)%salt(isalt)%conc
      enddo
      hsaltb_m(j)%salt(1)%diss = hsaltb_m(j)%salt(1)%diss + hsaltb_d(j)%salt(1)%diss
      
      !daily print
      if (pco%salt_hru%d == "y") then
        write (5021,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, & 
                         (hsaltb_d(j)%salt(isalt)%soil,isalt=1,cs_db%num_salts), &
                         (hsaltb_d(j)%salt(isalt)%surq,isalt=1,cs_db%num_salts), &
                         (hsaltb_d(j)%salt(isalt)%latq,isalt=1,cs_db%num_salts), &
                         (hsaltb_d(j)%salt(isalt)%urbq,isalt=1,cs_db%num_salts), &
                         (hsaltb_d(j)%salt(isalt)%wetq,isalt=1,cs_db%num_salts), &
                         (hsaltb_d(j)%salt(isalt)%tile,isalt=1,cs_db%num_salts), &
                         (hsaltb_d(j)%salt(isalt)%perc,isalt=1,cs_db%num_salts), &
                         (hsaltb_d(j)%salt(isalt)%wtsp,isalt=1,cs_db%num_salts), &
                         (hsaltb_d(j)%salt(isalt)%irsw,isalt=1,cs_db%num_salts), &
                         (hsaltb_d(j)%salt(isalt)%irgw,isalt=1,cs_db%num_salts), &
                         (hsaltb_d(j)%salt(isalt)%irwo,isalt=1,cs_db%num_salts), &
                         (hsaltb_d(j)%salt(isalt)%rain,isalt=1,cs_db%num_salts), &
                         (hsaltb_d(j)%salt(isalt)%dryd,isalt=1,cs_db%num_salts), &
                         (hsaltb_d(j)%salt(isalt)%road,isalt=1,cs_db%num_salts), &
                         (hsaltb_d(j)%salt(isalt)%fert,isalt=1,cs_db%num_salts), &
                         (hsaltb_d(j)%salt(isalt)%amnd,isalt=1,cs_db%num_salts), &
                         (hsaltb_d(j)%salt(isalt)%uptk,isalt=1,cs_db%num_salts), &
                         (hsaltb_d(j)%salt(isalt)%conc,isalt=1,cs_db%num_salts), &
                          hsaltb_d(j)%salt(1)%diss
        if (pco%csvout == "y") then
          write (5022,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, & 
																		    (hsaltb_d(j)%salt(isalt)%soil,isalt=1,cs_db%num_salts), &
                                        (hsaltb_d(j)%salt(isalt)%surq,isalt=1,cs_db%num_salts), &
                                        (hsaltb_d(j)%salt(isalt)%latq,isalt=1,cs_db%num_salts), &
                                        (hsaltb_d(j)%salt(isalt)%urbq,isalt=1,cs_db%num_salts), &
                                        (hsaltb_d(j)%salt(isalt)%wetq,isalt=1,cs_db%num_salts), &
                                        (hsaltb_d(j)%salt(isalt)%tile,isalt=1,cs_db%num_salts), &
                                        (hsaltb_d(j)%salt(isalt)%perc,isalt=1,cs_db%num_salts), &
                                        (hsaltb_d(j)%salt(isalt)%wtsp,isalt=1,cs_db%num_salts), &
                                        (hsaltb_d(j)%salt(isalt)%irsw,isalt=1,cs_db%num_salts), &
                                        (hsaltb_d(j)%salt(isalt)%irgw,isalt=1,cs_db%num_salts), &
                                        (hsaltb_d(j)%salt(isalt)%irwo,isalt=1,cs_db%num_salts), &
                                        (hsaltb_d(j)%salt(isalt)%rain,isalt=1,cs_db%num_salts), &
                                        (hsaltb_d(j)%salt(isalt)%dryd,isalt=1,cs_db%num_salts), &
                                        (hsaltb_d(j)%salt(isalt)%road,isalt=1,cs_db%num_salts), &
                                        (hsaltb_d(j)%salt(isalt)%fert,isalt=1,cs_db%num_salts), &
                                        (hsaltb_d(j)%salt(isalt)%amnd,isalt=1,cs_db%num_salts), &
                                        (hsaltb_d(j)%salt(isalt)%uptk,isalt=1,cs_db%num_salts), &
                                        (hsaltb_d(j)%salt(isalt)%conc,isalt=1,cs_db%num_salts), &
                                         hsaltb_d(j)%salt(1)%diss
        endif
      endif

      !monthly print
      if (time%end_mo == 1) then

        !add monthly values to yearly values
        do isalt = 1, cs_db%num_salts
          hsaltb_y(j)%salt(isalt)%soil = hsaltb_y(j)%salt(isalt)%soil + hsaltb_m(j)%salt(isalt)%soil
          hsaltb_y(j)%salt(isalt)%surq = hsaltb_y(j)%salt(isalt)%surq + hsaltb_m(j)%salt(isalt)%surq
          hsaltb_y(j)%salt(isalt)%latq = hsaltb_y(j)%salt(isalt)%latq + hsaltb_m(j)%salt(isalt)%latq
          hsaltb_y(j)%salt(isalt)%urbq = hsaltb_y(j)%salt(isalt)%urbq + hsaltb_m(j)%salt(isalt)%urbq
          hsaltb_y(j)%salt(isalt)%wetq = hsaltb_y(j)%salt(isalt)%wetq + hsaltb_m(j)%salt(isalt)%wetq
          hsaltb_y(j)%salt(isalt)%tile = hsaltb_y(j)%salt(isalt)%tile + hsaltb_m(j)%salt(isalt)%tile
          hsaltb_y(j)%salt(isalt)%perc = hsaltb_y(j)%salt(isalt)%perc + hsaltb_m(j)%salt(isalt)%perc
          hsaltb_y(j)%salt(isalt)%wtsp = hsaltb_y(j)%salt(isalt)%wtsp + hsaltb_m(j)%salt(isalt)%wtsp
          hsaltb_y(j)%salt(isalt)%irsw = hsaltb_y(j)%salt(isalt)%irsw + hsaltb_m(j)%salt(isalt)%irsw
          hsaltb_y(j)%salt(isalt)%irgw = hsaltb_y(j)%salt(isalt)%irgw + hsaltb_m(j)%salt(isalt)%irgw
          hsaltb_y(j)%salt(isalt)%irwo = hsaltb_y(j)%salt(isalt)%irwo + hsaltb_m(j)%salt(isalt)%irwo
          hsaltb_y(j)%salt(isalt)%rain = hsaltb_y(j)%salt(isalt)%rain + hsaltb_m(j)%salt(isalt)%rain
          hsaltb_y(j)%salt(isalt)%dryd = hsaltb_y(j)%salt(isalt)%dryd + hsaltb_m(j)%salt(isalt)%dryd
          hsaltb_y(j)%salt(isalt)%road = hsaltb_y(j)%salt(isalt)%road + hsaltb_m(j)%salt(isalt)%road
          hsaltb_y(j)%salt(isalt)%fert = hsaltb_y(j)%salt(isalt)%fert + hsaltb_m(j)%salt(isalt)%fert
          hsaltb_y(j)%salt(isalt)%amnd = hsaltb_y(j)%salt(isalt)%amnd + hsaltb_m(j)%salt(isalt)%amnd
          hsaltb_y(j)%salt(isalt)%uptk = hsaltb_y(j)%salt(isalt)%uptk + hsaltb_m(j)%salt(isalt)%uptk
          hsaltb_y(j)%salt(isalt)%conc = hsaltb_y(j)%salt(isalt)%conc + hsaltb_m(j)%salt(isalt)%conc
        enddo
        hsaltb_y(j)%salt(1)%diss = hsaltb_y(j)%salt(1)%diss + hsaltb_m(j)%salt(1)%diss
        const = float (ndays(time%mo + 1) - ndays(time%mo))
        do isalt=1,cs_db%num_salts !average mass and concentration
          hsaltb_m(j)%salt(isalt)%soil = hsaltb_m(j)%salt(isalt)%soil / const
          hsaltb_m(j)%salt(isalt)%conc = hsaltb_m(j)%salt(isalt)%conc / const
        enddo
        if (pco%salt_hru%m == "y") then
          write (5023,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, & 
                           (hsaltb_m(j)%salt(isalt)%soil,isalt=1,cs_db%num_salts), &
                           (hsaltb_m(j)%salt(isalt)%surq,isalt=1,cs_db%num_salts), &
                           (hsaltb_m(j)%salt(isalt)%latq,isalt=1,cs_db%num_salts), &
                           (hsaltb_m(j)%salt(isalt)%urbq,isalt=1,cs_db%num_salts), &
                           (hsaltb_m(j)%salt(isalt)%wetq,isalt=1,cs_db%num_salts), &
                           (hsaltb_m(j)%salt(isalt)%tile,isalt=1,cs_db%num_salts), &
                           (hsaltb_m(j)%salt(isalt)%perc,isalt=1,cs_db%num_salts), &
                           (hsaltb_m(j)%salt(isalt)%wtsp,isalt=1,cs_db%num_salts), &
                           (hsaltb_m(j)%salt(isalt)%irsw,isalt=1,cs_db%num_salts), &
                           (hsaltb_m(j)%salt(isalt)%irgw,isalt=1,cs_db%num_salts), &
                           (hsaltb_m(j)%salt(isalt)%irwo,isalt=1,cs_db%num_salts), &
                           (hsaltb_m(j)%salt(isalt)%rain,isalt=1,cs_db%num_salts), &
                           (hsaltb_m(j)%salt(isalt)%dryd,isalt=1,cs_db%num_salts), &
                           (hsaltb_m(j)%salt(isalt)%road,isalt=1,cs_db%num_salts), &
                           (hsaltb_m(j)%salt(isalt)%fert,isalt=1,cs_db%num_salts), &
                           (hsaltb_m(j)%salt(isalt)%amnd,isalt=1,cs_db%num_salts), &
                           (hsaltb_m(j)%salt(isalt)%uptk,isalt=1,cs_db%num_salts), &
                           (hsaltb_m(j)%salt(isalt)%conc,isalt=1,cs_db%num_salts), &
                            hsaltb_m(j)%salt(1)%diss
          if (pco%csvout == "y") then
            write (5024,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, & 
                                          (hsaltb_m(j)%salt(isalt)%soil,isalt=1,cs_db%num_salts), &
                                          (hsaltb_m(j)%salt(isalt)%surq,isalt=1,cs_db%num_salts), &
                                          (hsaltb_m(j)%salt(isalt)%latq,isalt=1,cs_db%num_salts), &
                                          (hsaltb_m(j)%salt(isalt)%urbq,isalt=1,cs_db%num_salts), &
                                          (hsaltb_m(j)%salt(isalt)%wetq,isalt=1,cs_db%num_salts), &
                                          (hsaltb_m(j)%salt(isalt)%tile,isalt=1,cs_db%num_salts), &
                                          (hsaltb_m(j)%salt(isalt)%perc,isalt=1,cs_db%num_salts), &
                                          (hsaltb_m(j)%salt(isalt)%wtsp,isalt=1,cs_db%num_salts), &
                                          (hsaltb_m(j)%salt(isalt)%irsw,isalt=1,cs_db%num_salts), &
                                          (hsaltb_m(j)%salt(isalt)%irgw,isalt=1,cs_db%num_salts), &
                                          (hsaltb_m(j)%salt(isalt)%irwo,isalt=1,cs_db%num_salts), &
                                          (hsaltb_m(j)%salt(isalt)%rain,isalt=1,cs_db%num_salts), &
                                          (hsaltb_m(j)%salt(isalt)%dryd,isalt=1,cs_db%num_salts), &
                                          (hsaltb_m(j)%salt(isalt)%road,isalt=1,cs_db%num_salts), &
                                          (hsaltb_m(j)%salt(isalt)%fert,isalt=1,cs_db%num_salts), &
                                          (hsaltb_m(j)%salt(isalt)%amnd,isalt=1,cs_db%num_salts), &
                                          (hsaltb_m(j)%salt(isalt)%uptk,isalt=1,cs_db%num_salts), &
                                          (hsaltb_m(j)%salt(isalt)%conc,isalt=1,cs_db%num_salts), &
                                           hsaltb_m(j)%salt(1)%diss
          endif
        endif
        !zero out
        do isalt = 1, cs_db%num_salts
          hsaltb_m(j)%salt(isalt)%soil = 0.
          hsaltb_m(j)%salt(isalt)%surq = 0.
          hsaltb_m(j)%salt(isalt)%latq = 0.
          hsaltb_m(j)%salt(isalt)%urbq = 0.
          hsaltb_m(j)%salt(isalt)%wetq = 0.
          hsaltb_m(j)%salt(isalt)%tile = 0.
          hsaltb_m(j)%salt(isalt)%perc = 0.
          hsaltb_m(j)%salt(isalt)%wtsp = 0.
          hsaltb_m(j)%salt(isalt)%irsw = 0.
          hsaltb_m(j)%salt(isalt)%irgw = 0.
          hsaltb_m(j)%salt(isalt)%irwo = 0.
          hsaltb_m(j)%salt(isalt)%rain = 0.
          hsaltb_m(j)%salt(isalt)%dryd = 0.
          hsaltb_m(j)%salt(isalt)%road = 0.
          hsaltb_m(j)%salt(isalt)%fert = 0.
          hsaltb_m(j)%salt(isalt)%amnd = 0.
          hsaltb_m(j)%salt(isalt)%uptk = 0.
          hsaltb_m(j)%salt(isalt)%conc = 0.
          hsaltb_m(j)%salt(1)%diss = 0.
        enddo
      endif
      
      !yearly print
      if (time%end_yr == 1) then
        !add yearly values to total values
        do isalt = 1, cs_db%num_salts
          hsaltb_a(j)%salt(isalt)%soil = hsaltb_a(j)%salt(isalt)%soil + hsaltb_y(j)%salt(isalt)%soil
          hsaltb_a(j)%salt(isalt)%surq = hsaltb_a(j)%salt(isalt)%surq + hsaltb_y(j)%salt(isalt)%surq
          hsaltb_a(j)%salt(isalt)%latq = hsaltb_a(j)%salt(isalt)%latq + hsaltb_y(j)%salt(isalt)%latq
          hsaltb_a(j)%salt(isalt)%urbq = hsaltb_a(j)%salt(isalt)%urbq + hsaltb_y(j)%salt(isalt)%urbq
          hsaltb_a(j)%salt(isalt)%wetq = hsaltb_a(j)%salt(isalt)%wetq + hsaltb_y(j)%salt(isalt)%wetq
          hsaltb_a(j)%salt(isalt)%tile = hsaltb_a(j)%salt(isalt)%tile + hsaltb_y(j)%salt(isalt)%tile
          hsaltb_a(j)%salt(isalt)%perc = hsaltb_a(j)%salt(isalt)%perc + hsaltb_y(j)%salt(isalt)%perc
          hsaltb_a(j)%salt(isalt)%wtsp = hsaltb_a(j)%salt(isalt)%wtsp + hsaltb_y(j)%salt(isalt)%wtsp
          hsaltb_a(j)%salt(isalt)%irsw = hsaltb_a(j)%salt(isalt)%irsw + hsaltb_y(j)%salt(isalt)%irsw
          hsaltb_a(j)%salt(isalt)%irgw = hsaltb_a(j)%salt(isalt)%irgw + hsaltb_y(j)%salt(isalt)%irgw
          hsaltb_a(j)%salt(isalt)%irwo = hsaltb_a(j)%salt(isalt)%irwo + hsaltb_y(j)%salt(isalt)%irwo
          hsaltb_a(j)%salt(isalt)%rain = hsaltb_a(j)%salt(isalt)%rain + hsaltb_y(j)%salt(isalt)%rain
          hsaltb_a(j)%salt(isalt)%dryd = hsaltb_a(j)%salt(isalt)%dryd + hsaltb_y(j)%salt(isalt)%dryd
          hsaltb_a(j)%salt(isalt)%road = hsaltb_a(j)%salt(isalt)%road + hsaltb_y(j)%salt(isalt)%road
          hsaltb_a(j)%salt(isalt)%fert = hsaltb_a(j)%salt(isalt)%fert + hsaltb_y(j)%salt(isalt)%fert
          hsaltb_a(j)%salt(isalt)%amnd = hsaltb_a(j)%salt(isalt)%amnd + hsaltb_y(j)%salt(isalt)%amnd
          hsaltb_a(j)%salt(isalt)%uptk = hsaltb_a(j)%salt(isalt)%uptk + hsaltb_y(j)%salt(isalt)%uptk
          hsaltb_a(j)%salt(isalt)%conc = hsaltb_a(j)%salt(isalt)%conc + hsaltb_y(j)%salt(isalt)%conc
        enddo
        hsaltb_a(j)%salt(1)%diss = hsaltb_a(j)%salt(1)%diss + hsaltb_y(j)%salt(1)%diss
        const = time%day_end_yr
        do isalt=1,cs_db%num_salts !average mass and concentration
          hsaltb_y(j)%salt(isalt)%soil = hsaltb_y(j)%salt(isalt)%soil / const
          hsaltb_y(j)%salt(isalt)%conc = hsaltb_y(j)%salt(isalt)%conc / const
        enddo
        if (pco%salt_hru%y == "y") then
          write (5025,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, & 
                           (hsaltb_y(j)%salt(isalt)%soil,isalt=1,cs_db%num_salts), &
                           (hsaltb_y(j)%salt(isalt)%surq,isalt=1,cs_db%num_salts), &
                           (hsaltb_y(j)%salt(isalt)%latq,isalt=1,cs_db%num_salts), &
                           (hsaltb_y(j)%salt(isalt)%urbq,isalt=1,cs_db%num_salts), &
                           (hsaltb_y(j)%salt(isalt)%wetq,isalt=1,cs_db%num_salts), &
                           (hsaltb_y(j)%salt(isalt)%tile,isalt=1,cs_db%num_salts), &
                           (hsaltb_y(j)%salt(isalt)%perc,isalt=1,cs_db%num_salts), &
                           (hsaltb_y(j)%salt(isalt)%wtsp,isalt=1,cs_db%num_salts), &
                           (hsaltb_y(j)%salt(isalt)%irsw,isalt=1,cs_db%num_salts), &
                           (hsaltb_y(j)%salt(isalt)%irgw,isalt=1,cs_db%num_salts), &
                           (hsaltb_y(j)%salt(isalt)%irwo,isalt=1,cs_db%num_salts), &
                           (hsaltb_y(j)%salt(isalt)%rain,isalt=1,cs_db%num_salts), &
                           (hsaltb_y(j)%salt(isalt)%dryd,isalt=1,cs_db%num_salts), &
                           (hsaltb_y(j)%salt(isalt)%road,isalt=1,cs_db%num_salts), &
                           (hsaltb_y(j)%salt(isalt)%fert,isalt=1,cs_db%num_salts), &
                           (hsaltb_y(j)%salt(isalt)%amnd,isalt=1,cs_db%num_salts), &
                           (hsaltb_y(j)%salt(isalt)%uptk,isalt=1,cs_db%num_salts), &
                           (hsaltb_y(j)%salt(isalt)%conc,isalt=1,cs_db%num_salts), &
                            hsaltb_y(j)%salt(1)%diss
          if (pco%csvout == "y") then
            write (5026,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, & 
                                          (hsaltb_y(j)%salt(isalt)%soil,isalt=1,cs_db%num_salts), &
                                          (hsaltb_y(j)%salt(isalt)%surq,isalt=1,cs_db%num_salts), &
                                          (hsaltb_y(j)%salt(isalt)%latq,isalt=1,cs_db%num_salts), &
                                          (hsaltb_y(j)%salt(isalt)%urbq,isalt=1,cs_db%num_salts), &
                                          (hsaltb_y(j)%salt(isalt)%wetq,isalt=1,cs_db%num_salts), &
                                          (hsaltb_y(j)%salt(isalt)%tile,isalt=1,cs_db%num_salts), &
                                          (hsaltb_y(j)%salt(isalt)%perc,isalt=1,cs_db%num_salts), &
                                          (hsaltb_y(j)%salt(isalt)%wtsp,isalt=1,cs_db%num_salts), &
                                          (hsaltb_y(j)%salt(isalt)%irsw,isalt=1,cs_db%num_salts), &
                                          (hsaltb_y(j)%salt(isalt)%irgw,isalt=1,cs_db%num_salts), &
                                          (hsaltb_y(j)%salt(isalt)%irwo,isalt=1,cs_db%num_salts), &
                                          (hsaltb_y(j)%salt(isalt)%rain,isalt=1,cs_db%num_salts), &
                                          (hsaltb_y(j)%salt(isalt)%dryd,isalt=1,cs_db%num_salts), &
                                          (hsaltb_y(j)%salt(isalt)%road,isalt=1,cs_db%num_salts), &
                                          (hsaltb_y(j)%salt(isalt)%fert,isalt=1,cs_db%num_salts), &
                                          (hsaltb_y(j)%salt(isalt)%amnd,isalt=1,cs_db%num_salts), &
                                          (hsaltb_y(j)%salt(isalt)%uptk,isalt=1,cs_db%num_salts), &
                                          (hsaltb_y(j)%salt(isalt)%conc,isalt=1,cs_db%num_salts), &
                                           hsaltb_y(j)%salt(1)%diss
          endif
        endif
        !zero out
        do isalt = 1, cs_db%num_salts
          hsaltb_y(j)%salt(isalt)%soil = 0.
          hsaltb_y(j)%salt(isalt)%surq = 0.
          hsaltb_y(j)%salt(isalt)%latq = 0.
          hsaltb_y(j)%salt(isalt)%urbq = 0.
          hsaltb_y(j)%salt(isalt)%wetq = 0.
          hsaltb_y(j)%salt(isalt)%tile = 0.
          hsaltb_y(j)%salt(isalt)%perc = 0.
          hsaltb_y(j)%salt(isalt)%wtsp = 0.
          hsaltb_y(j)%salt(isalt)%irsw = 0.
          hsaltb_y(j)%salt(isalt)%irgw = 0.
          hsaltb_y(j)%salt(isalt)%irwo = 0.
          hsaltb_y(j)%salt(isalt)%rain = 0.
          hsaltb_y(j)%salt(isalt)%dryd = 0.
          hsaltb_y(j)%salt(isalt)%road = 0.
          hsaltb_y(j)%salt(isalt)%fert = 0.
          hsaltb_y(j)%salt(isalt)%amnd = 0.
          hsaltb_y(j)%salt(isalt)%uptk = 0.
          hsaltb_y(j)%salt(isalt)%conc = 0.
          hsaltb_y(j)%salt(1)%diss = 0.
        enddo
      endif
      
      !average annual print
      if (time%end_sim == 1 .and. pco%salt_hru%a == "y") then
        !calculate average annual values
        do isalt = 1, cs_db%num_salts
          hsaltb_a(j)%salt(isalt)%soil = hsaltb_a(j)%salt(isalt)%soil / time%nbyr
          hsaltb_a(j)%salt(isalt)%surq = hsaltb_a(j)%salt(isalt)%surq / time%nbyr
          hsaltb_a(j)%salt(isalt)%latq = hsaltb_a(j)%salt(isalt)%latq / time%nbyr
          hsaltb_a(j)%salt(isalt)%urbq = hsaltb_a(j)%salt(isalt)%urbq / time%nbyr
          hsaltb_a(j)%salt(isalt)%wetq = hsaltb_a(j)%salt(isalt)%wetq / time%nbyr
          hsaltb_a(j)%salt(isalt)%tile = hsaltb_a(j)%salt(isalt)%tile / time%nbyr
          hsaltb_a(j)%salt(isalt)%perc = hsaltb_a(j)%salt(isalt)%perc / time%nbyr
          hsaltb_a(j)%salt(isalt)%wtsp = hsaltb_a(j)%salt(isalt)%wtsp / time%nbyr
          hsaltb_a(j)%salt(isalt)%irsw = hsaltb_a(j)%salt(isalt)%irsw / time%nbyr
          hsaltb_a(j)%salt(isalt)%irgw = hsaltb_a(j)%salt(isalt)%irgw / time%nbyr
          hsaltb_a(j)%salt(isalt)%irwo = hsaltb_a(j)%salt(isalt)%irwo / time%nbyr
          hsaltb_a(j)%salt(isalt)%rain = hsaltb_a(j)%salt(isalt)%rain / time%nbyr
          hsaltb_a(j)%salt(isalt)%dryd = hsaltb_a(j)%salt(isalt)%dryd / time%nbyr
          hsaltb_a(j)%salt(isalt)%road = hsaltb_a(j)%salt(isalt)%road / time%nbyr
          hsaltb_a(j)%salt(isalt)%fert = hsaltb_a(j)%salt(isalt)%fert / time%nbyr
          hsaltb_a(j)%salt(isalt)%amnd = hsaltb_a(j)%salt(isalt)%amnd / time%nbyr
          hsaltb_a(j)%salt(isalt)%uptk = hsaltb_a(j)%salt(isalt)%uptk / time%nbyr
          hsaltb_a(j)%salt(isalt)%conc = hsaltb_a(j)%salt(isalt)%conc / time%nbyr
        enddo
        write (5027,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, & 
                         (hsaltb_a(j)%salt(isalt)%soil,isalt=1,cs_db%num_salts), &
                         (hsaltb_a(j)%salt(isalt)%surq,isalt=1,cs_db%num_salts), &
                         (hsaltb_a(j)%salt(isalt)%latq,isalt=1,cs_db%num_salts), &
                         (hsaltb_a(j)%salt(isalt)%urbq,isalt=1,cs_db%num_salts), &
                         (hsaltb_a(j)%salt(isalt)%wetq,isalt=1,cs_db%num_salts), &
                         (hsaltb_a(j)%salt(isalt)%tile,isalt=1,cs_db%num_salts), &
                         (hsaltb_a(j)%salt(isalt)%perc,isalt=1,cs_db%num_salts), &
                         (hsaltb_a(j)%salt(isalt)%wtsp,isalt=1,cs_db%num_salts), &
                         (hsaltb_a(j)%salt(isalt)%irsw,isalt=1,cs_db%num_salts), &
                         (hsaltb_a(j)%salt(isalt)%irgw,isalt=1,cs_db%num_salts), &
                         (hsaltb_a(j)%salt(isalt)%irwo,isalt=1,cs_db%num_salts), &
                         (hsaltb_a(j)%salt(isalt)%rain,isalt=1,cs_db%num_salts), &
                         (hsaltb_a(j)%salt(isalt)%dryd,isalt=1,cs_db%num_salts), &
                         (hsaltb_a(j)%salt(isalt)%road,isalt=1,cs_db%num_salts), &
                         (hsaltb_a(j)%salt(isalt)%fert,isalt=1,cs_db%num_salts), &
                         (hsaltb_a(j)%salt(isalt)%amnd,isalt=1,cs_db%num_salts), &
                         (hsaltb_a(j)%salt(isalt)%uptk,isalt=1,cs_db%num_salts), &
                         (hsaltb_a(j)%salt(isalt)%conc,isalt=1,cs_db%num_salts), &
                         hsaltb_a(j)%salt(1)%diss
        if (pco%csvout == "y") then
          write (5028,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, & 
                                        (hsaltb_a(j)%salt(isalt)%soil,isalt=1,cs_db%num_salts), &
                                        (hsaltb_a(j)%salt(isalt)%surq,isalt=1,cs_db%num_salts), &
                                        (hsaltb_a(j)%salt(isalt)%latq,isalt=1,cs_db%num_salts), &
                                        (hsaltb_a(j)%salt(isalt)%urbq,isalt=1,cs_db%num_salts), &
                                        (hsaltb_a(j)%salt(isalt)%wetq,isalt=1,cs_db%num_salts), &
                                        (hsaltb_a(j)%salt(isalt)%tile,isalt=1,cs_db%num_salts), &
                                        (hsaltb_a(j)%salt(isalt)%perc,isalt=1,cs_db%num_salts), &
                                        (hsaltb_a(j)%salt(isalt)%wtsp,isalt=1,cs_db%num_salts), &
                                        (hsaltb_a(j)%salt(isalt)%irsw,isalt=1,cs_db%num_salts), &
                                        (hsaltb_a(j)%salt(isalt)%irgw,isalt=1,cs_db%num_salts), &
                                        (hsaltb_a(j)%salt(isalt)%irwo,isalt=1,cs_db%num_salts), &
                                        (hsaltb_a(j)%salt(isalt)%rain,isalt=1,cs_db%num_salts), &
                                        (hsaltb_a(j)%salt(isalt)%dryd,isalt=1,cs_db%num_salts), &
                                        (hsaltb_a(j)%salt(isalt)%road,isalt=1,cs_db%num_salts), &
                                        (hsaltb_a(j)%salt(isalt)%fert,isalt=1,cs_db%num_salts), &
                                        (hsaltb_a(j)%salt(isalt)%amnd,isalt=1,cs_db%num_salts), &
                                        (hsaltb_a(j)%salt(isalt)%uptk,isalt=1,cs_db%num_salts), &
                                        (hsaltb_a(j)%salt(isalt)%conc,isalt=1,cs_db%num_salts), &
                                        hsaltb_a(j)%salt(1)%diss
        endif
      endif
     
      return
      
100   format (4i6,2i8,500e15.4)      

      end subroutine hru_salt_output