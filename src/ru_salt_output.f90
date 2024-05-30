      subroutine ru_salt_output(iru) !rtb salt
    
      use time_module
      use basin_module
      use hydrograph_module, only : ob, sp_ob1
      use salt_module
      use constituent_mass_module
      
      implicit none
      
      integer, intent (in) :: iru        !             |
      integer :: iob                     !             |
      integer :: isalt                   !						 |salt ion counter
      integer :: ihyd                    !             |hydrograph counter
                         
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine outputs salt mass loadings and concentrations from routing units
      
      iob = sp_ob1%ru + iru - 1 
      
      !note:
      !hd(1) = total salt out
      !hd(2) = percolation
      !hd(3) = surface runoff
      !hd(4) = lateral flow
      !hd(5) = tile flow
      
      !add daily values to monthly values
      do isalt=1,cs_db%num_salts
        do ihyd=1,5  
          rusaltb_m(iru)%hd(ihyd)%salt(isalt) = rusaltb_m(iru)%hd(ihyd)%salt(isalt) + rusaltb_d(iru)%hd(ihyd)%salt(isalt)
        enddo
        ru_hru_saltb_m(iru)%salt(isalt)%wtsp = ru_hru_saltb_m(iru)%salt(isalt)%wtsp + ru_hru_saltb_d(iru)%salt(isalt)%wtsp
        ru_hru_saltb_m(iru)%salt(isalt)%irsw = ru_hru_saltb_m(iru)%salt(isalt)%irsw + ru_hru_saltb_d(iru)%salt(isalt)%irsw
        ru_hru_saltb_m(iru)%salt(isalt)%irgw = ru_hru_saltb_m(iru)%salt(isalt)%irgw + ru_hru_saltb_d(iru)%salt(isalt)%irgw
        ru_hru_saltb_m(iru)%salt(isalt)%irwo = ru_hru_saltb_m(iru)%salt(isalt)%irwo + ru_hru_saltb_d(iru)%salt(isalt)%irwo
        ru_hru_saltb_m(iru)%salt(isalt)%rain = ru_hru_saltb_m(iru)%salt(isalt)%rain + ru_hru_saltb_d(iru)%salt(isalt)%rain
        ru_hru_saltb_m(iru)%salt(isalt)%dryd = ru_hru_saltb_m(iru)%salt(isalt)%dryd + ru_hru_saltb_d(iru)%salt(isalt)%dryd
        ru_hru_saltb_m(iru)%salt(isalt)%road = ru_hru_saltb_m(iru)%salt(isalt)%road + ru_hru_saltb_d(iru)%salt(isalt)%road
        ru_hru_saltb_m(iru)%salt(isalt)%fert = ru_hru_saltb_m(iru)%salt(isalt)%fert + ru_hru_saltb_d(iru)%salt(isalt)%fert
        ru_hru_saltb_m(iru)%salt(isalt)%amnd = ru_hru_saltb_m(iru)%salt(isalt)%amnd + ru_hru_saltb_d(iru)%salt(isalt)%amnd
        ru_hru_saltb_m(iru)%salt(isalt)%uptk = ru_hru_saltb_m(iru)%salt(isalt)%uptk + ru_hru_saltb_d(iru)%salt(isalt)%uptk
      enddo
      ru_hru_saltb_m(iru)%salt(1)%diss = ru_hru_saltb_m(iru)%salt(1)%diss + ru_hru_saltb_d(iru)%salt(1)%diss
      
      !daily print
      if (pco%salt_ru%d == "y") then
        write (5070,100) time%day, time%mo, time%day_mo, time%yrc, iru, ob(iob)%gis_id, & 
                         (rusaltb_d(iru)%hd(1)%salt(isalt),isalt=1,cs_db%num_salts), &
                         (rusaltb_d(iru)%hd(2)%salt(isalt),isalt=1,cs_db%num_salts), &
                         (rusaltb_d(iru)%hd(3)%salt(isalt),isalt=1,cs_db%num_salts), &
                         (rusaltb_d(iru)%hd(4)%salt(isalt),isalt=1,cs_db%num_salts), &
                         (rusaltb_d(iru)%hd(5)%salt(isalt),isalt=1,cs_db%num_salts), &
                         (ru_hru_saltb_d(iru)%salt(isalt)%wtsp,isalt=1,cs_db%num_salts), &
                         (ru_hru_saltb_d(iru)%salt(isalt)%irsw,isalt=1,cs_db%num_salts), &
                         (ru_hru_saltb_d(iru)%salt(isalt)%irgw,isalt=1,cs_db%num_salts), &
                         (ru_hru_saltb_d(iru)%salt(isalt)%irwo,isalt=1,cs_db%num_salts), &
                         (ru_hru_saltb_d(iru)%salt(isalt)%rain,isalt=1,cs_db%num_salts), &
                         (ru_hru_saltb_d(iru)%salt(isalt)%dryd,isalt=1,cs_db%num_salts), &
                         (ru_hru_saltb_d(iru)%salt(isalt)%road,isalt=1,cs_db%num_salts), &
                         (ru_hru_saltb_d(iru)%salt(isalt)%fert,isalt=1,cs_db%num_salts), &
                         (ru_hru_saltb_d(iru)%salt(isalt)%amnd,isalt=1,cs_db%num_salts), &
                         (ru_hru_saltb_d(iru)%salt(isalt)%uptk,isalt=1,cs_db%num_salts), &
                          ru_hru_saltb_d(iru)%salt(1)%diss       
        if (pco%csvout == "y") then
          write (5071,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, iru, ob(iob)%gis_id, & 
																		    (rusaltb_d(iru)%hd(1)%salt(isalt),isalt=1,cs_db%num_salts), &
                                        (rusaltb_d(iru)%hd(2)%salt(isalt),isalt=1,cs_db%num_salts), &
                                        (rusaltb_d(iru)%hd(3)%salt(isalt),isalt=1,cs_db%num_salts), &
                                        (rusaltb_d(iru)%hd(4)%salt(isalt),isalt=1,cs_db%num_salts), &
                                        (rusaltb_d(iru)%hd(5)%salt(isalt),isalt=1,cs_db%num_salts), &
                                        (ru_hru_saltb_d(iru)%salt(isalt)%wtsp,isalt=1,cs_db%num_salts), &
                                        (ru_hru_saltb_d(iru)%salt(isalt)%irsw,isalt=1,cs_db%num_salts), &
                                        (ru_hru_saltb_d(iru)%salt(isalt)%irgw,isalt=1,cs_db%num_salts), &
                                        (ru_hru_saltb_d(iru)%salt(isalt)%irwo,isalt=1,cs_db%num_salts), &
                                        (ru_hru_saltb_d(iru)%salt(isalt)%rain,isalt=1,cs_db%num_salts), &
                                        (ru_hru_saltb_d(iru)%salt(isalt)%dryd,isalt=1,cs_db%num_salts), &
                                        (ru_hru_saltb_d(iru)%salt(isalt)%road,isalt=1,cs_db%num_salts), &
                                        (ru_hru_saltb_d(iru)%salt(isalt)%fert,isalt=1,cs_db%num_salts), &
                                        (ru_hru_saltb_d(iru)%salt(isalt)%amnd,isalt=1,cs_db%num_salts), &
                                        (ru_hru_saltb_d(iru)%salt(isalt)%uptk,isalt=1,cs_db%num_salts), &
                                         ru_hru_saltb_d(iru)%salt(1)%diss
        endif
      endif
      !zero out
      do isalt = 1, cs_db%num_salts
        do ihyd=1,5  
          rusaltb_d(iru)%hd(ihyd)%salt(isalt) = 0.
        enddo
        ru_hru_saltb_d(iru)%salt(isalt)%wtsp = 0.
        ru_hru_saltb_d(iru)%salt(isalt)%irsw = 0.
        ru_hru_saltb_d(iru)%salt(isalt)%irgw = 0.
        ru_hru_saltb_d(iru)%salt(isalt)%irwo = 0.
        ru_hru_saltb_d(iru)%salt(isalt)%rain = 0.
        ru_hru_saltb_d(iru)%salt(isalt)%dryd = 0.
        ru_hru_saltb_d(iru)%salt(isalt)%road = 0.
        ru_hru_saltb_d(iru)%salt(isalt)%fert = 0.
        ru_hru_saltb_d(iru)%salt(isalt)%amnd = 0.
        ru_hru_saltb_d(iru)%salt(isalt)%uptk = 0.
      enddo
      

      !monthly print
      if (time%end_mo == 1) then
        !add monthly values to yearly values
        do isalt = 1, cs_db%num_salts
          do ihyd=1,5  
            rusaltb_y(iru)%hd(ihyd)%salt(isalt) = rusaltb_y(iru)%hd(ihyd)%salt(isalt) + rusaltb_m(iru)%hd(ihyd)%salt(isalt)
          enddo
          ru_hru_saltb_y(iru)%salt(isalt)%wtsp = ru_hru_saltb_y(iru)%salt(isalt)%wtsp + ru_hru_saltb_m(iru)%salt(isalt)%wtsp
          ru_hru_saltb_y(iru)%salt(isalt)%irsw = ru_hru_saltb_y(iru)%salt(isalt)%irsw + ru_hru_saltb_m(iru)%salt(isalt)%irsw
          ru_hru_saltb_y(iru)%salt(isalt)%irgw = ru_hru_saltb_y(iru)%salt(isalt)%irgw + ru_hru_saltb_m(iru)%salt(isalt)%irgw
          ru_hru_saltb_y(iru)%salt(isalt)%irwo = ru_hru_saltb_y(iru)%salt(isalt)%irwo + ru_hru_saltb_m(iru)%salt(isalt)%irwo
          ru_hru_saltb_y(iru)%salt(isalt)%rain = ru_hru_saltb_y(iru)%salt(isalt)%rain + ru_hru_saltb_m(iru)%salt(isalt)%rain
          ru_hru_saltb_y(iru)%salt(isalt)%dryd = ru_hru_saltb_y(iru)%salt(isalt)%dryd + ru_hru_saltb_m(iru)%salt(isalt)%dryd
          ru_hru_saltb_y(iru)%salt(isalt)%road = ru_hru_saltb_y(iru)%salt(isalt)%road + ru_hru_saltb_m(iru)%salt(isalt)%road
          ru_hru_saltb_y(iru)%salt(isalt)%fert = ru_hru_saltb_y(iru)%salt(isalt)%fert + ru_hru_saltb_m(iru)%salt(isalt)%fert
          ru_hru_saltb_y(iru)%salt(isalt)%amnd = ru_hru_saltb_y(iru)%salt(isalt)%amnd + ru_hru_saltb_m(iru)%salt(isalt)%amnd
          ru_hru_saltb_y(iru)%salt(isalt)%uptk = ru_hru_saltb_y(iru)%salt(isalt)%uptk + ru_hru_saltb_m(iru)%salt(isalt)%uptk
        enddo
        ru_hru_saltb_y(iru)%salt(1)%diss = ru_hru_saltb_y(iru)%salt(1)%diss + ru_hru_saltb_m(iru)%salt(1)%diss
        if (pco%salt_ru%m == "y") then
          write (5072,100) time%day, time%mo, time%day_mo, time%yrc, iru, ob(iob)%gis_id, & 
                           (rusaltb_m(iru)%hd(1)%salt(isalt),isalt=1,cs_db%num_salts), &
                           (rusaltb_m(iru)%hd(2)%salt(isalt),isalt=1,cs_db%num_salts), &
                           (rusaltb_m(iru)%hd(3)%salt(isalt),isalt=1,cs_db%num_salts), &
                           (rusaltb_m(iru)%hd(4)%salt(isalt),isalt=1,cs_db%num_salts), &
                           (rusaltb_m(iru)%hd(5)%salt(isalt),isalt=1,cs_db%num_salts), &
                           (ru_hru_saltb_m(iru)%salt(isalt)%wtsp,isalt=1,cs_db%num_salts), &
                           (ru_hru_saltb_m(iru)%salt(isalt)%irsw,isalt=1,cs_db%num_salts), &
                           (ru_hru_saltb_m(iru)%salt(isalt)%irgw,isalt=1,cs_db%num_salts), &
                           (ru_hru_saltb_m(iru)%salt(isalt)%irwo,isalt=1,cs_db%num_salts), &
                           (ru_hru_saltb_m(iru)%salt(isalt)%rain,isalt=1,cs_db%num_salts), &
                           (ru_hru_saltb_m(iru)%salt(isalt)%dryd,isalt=1,cs_db%num_salts), &
                           (ru_hru_saltb_m(iru)%salt(isalt)%road,isalt=1,cs_db%num_salts), &
                           (ru_hru_saltb_m(iru)%salt(isalt)%fert,isalt=1,cs_db%num_salts), &
                           (ru_hru_saltb_m(iru)%salt(isalt)%amnd,isalt=1,cs_db%num_salts), &
                           (ru_hru_saltb_m(iru)%salt(isalt)%uptk,isalt=1,cs_db%num_salts), &
                            ru_hru_saltb_m(iru)%salt(1)%diss
          if (pco%csvout == "y") then
            write (5073,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, iru, ob(iob)%gis_id, & 
                                          (rusaltb_m(iru)%hd(1)%salt(isalt),isalt=1,cs_db%num_salts), &
                                          (rusaltb_m(iru)%hd(2)%salt(isalt),isalt=1,cs_db%num_salts), &
                                          (rusaltb_m(iru)%hd(3)%salt(isalt),isalt=1,cs_db%num_salts), &
                                          (rusaltb_m(iru)%hd(4)%salt(isalt),isalt=1,cs_db%num_salts), &
                                          (rusaltb_m(iru)%hd(5)%salt(isalt),isalt=1,cs_db%num_salts), &
                                          (ru_hru_saltb_m(iru)%salt(isalt)%wtsp,isalt=1,cs_db%num_salts), &
                                          (ru_hru_saltb_m(iru)%salt(isalt)%irsw,isalt=1,cs_db%num_salts), &
                                          (ru_hru_saltb_m(iru)%salt(isalt)%irgw,isalt=1,cs_db%num_salts), &
                                          (ru_hru_saltb_m(iru)%salt(isalt)%irwo,isalt=1,cs_db%num_salts), &
                                          (ru_hru_saltb_m(iru)%salt(isalt)%rain,isalt=1,cs_db%num_salts), &
                                          (ru_hru_saltb_m(iru)%salt(isalt)%dryd,isalt=1,cs_db%num_salts), &
                                          (ru_hru_saltb_m(iru)%salt(isalt)%road,isalt=1,cs_db%num_salts), &
                                          (ru_hru_saltb_m(iru)%salt(isalt)%fert,isalt=1,cs_db%num_salts), &
                                          (ru_hru_saltb_m(iru)%salt(isalt)%amnd,isalt=1,cs_db%num_salts), &
                                          (ru_hru_saltb_m(iru)%salt(isalt)%uptk,isalt=1,cs_db%num_salts), &
                                           ru_hru_saltb_m(iru)%salt(1)%diss
          endif
        endif
        !zero out
        do isalt = 1, cs_db%num_salts
          do ihyd=1,5  
            rusaltb_m(iru)%hd(ihyd)%salt(isalt) = 0.
          enddo
          ru_hru_saltb_m(iru)%salt(isalt)%wtsp = 0.
          ru_hru_saltb_m(iru)%salt(isalt)%irsw = 0.
          ru_hru_saltb_m(iru)%salt(isalt)%irgw = 0.
          ru_hru_saltb_m(iru)%salt(isalt)%irwo = 0.
          ru_hru_saltb_m(iru)%salt(isalt)%rain = 0.
          ru_hru_saltb_m(iru)%salt(isalt)%dryd = 0.
          ru_hru_saltb_m(iru)%salt(isalt)%road = 0.
          ru_hru_saltb_m(iru)%salt(isalt)%fert = 0.
          ru_hru_saltb_m(iru)%salt(isalt)%amnd = 0.
          ru_hru_saltb_m(iru)%salt(isalt)%uptk = 0.
        enddo
      endif
      
      !yearly print
      if (time%end_yr == 1) then
        !add yearly values to total values
        do isalt = 1, cs_db%num_salts
          do ihyd=1,5  
            rusaltb_a(iru)%hd(ihyd)%salt(isalt) = rusaltb_a(iru)%hd(ihyd)%salt(isalt) + rusaltb_y(iru)%hd(ihyd)%salt(isalt)
          enddo
          ru_hru_saltb_a(iru)%salt(isalt)%wtsp = ru_hru_saltb_a(iru)%salt(isalt)%wtsp + ru_hru_saltb_y(iru)%salt(isalt)%wtsp
          ru_hru_saltb_a(iru)%salt(isalt)%irsw = ru_hru_saltb_a(iru)%salt(isalt)%irsw + ru_hru_saltb_y(iru)%salt(isalt)%irsw
          ru_hru_saltb_a(iru)%salt(isalt)%irgw = ru_hru_saltb_a(iru)%salt(isalt)%irgw + ru_hru_saltb_y(iru)%salt(isalt)%irgw
          ru_hru_saltb_a(iru)%salt(isalt)%irwo = ru_hru_saltb_a(iru)%salt(isalt)%irwo + ru_hru_saltb_y(iru)%salt(isalt)%irwo
          ru_hru_saltb_a(iru)%salt(isalt)%rain = ru_hru_saltb_a(iru)%salt(isalt)%rain + ru_hru_saltb_y(iru)%salt(isalt)%rain
          ru_hru_saltb_a(iru)%salt(isalt)%dryd = ru_hru_saltb_a(iru)%salt(isalt)%dryd + ru_hru_saltb_y(iru)%salt(isalt)%dryd
          ru_hru_saltb_a(iru)%salt(isalt)%road = ru_hru_saltb_a(iru)%salt(isalt)%road + ru_hru_saltb_y(iru)%salt(isalt)%road
          ru_hru_saltb_a(iru)%salt(isalt)%fert = ru_hru_saltb_a(iru)%salt(isalt)%fert + ru_hru_saltb_y(iru)%salt(isalt)%fert
          ru_hru_saltb_a(iru)%salt(isalt)%amnd = ru_hru_saltb_a(iru)%salt(isalt)%amnd + ru_hru_saltb_y(iru)%salt(isalt)%amnd
          ru_hru_saltb_a(iru)%salt(isalt)%uptk = ru_hru_saltb_a(iru)%salt(isalt)%uptk + ru_hru_saltb_y(iru)%salt(isalt)%uptk
        enddo
        ru_hru_saltb_a(iru)%salt(1)%diss = ru_hru_saltb_a(iru)%salt(1)%diss + ru_hru_saltb_y(iru)%salt(1)%diss
        if (pco%salt_ru%y == "y") then
          write (5074,100) time%day, time%mo, time%day_mo, time%yrc, iru, ob(iob)%gis_id, & 
                           (rusaltb_y(iru)%hd(1)%salt(isalt),isalt=1,cs_db%num_salts), &
                           (rusaltb_y(iru)%hd(2)%salt(isalt),isalt=1,cs_db%num_salts), &
                           (rusaltb_y(iru)%hd(3)%salt(isalt),isalt=1,cs_db%num_salts), &
                           (rusaltb_y(iru)%hd(4)%salt(isalt),isalt=1,cs_db%num_salts), &
                           (rusaltb_y(iru)%hd(5)%salt(isalt),isalt=1,cs_db%num_salts), &
                           (ru_hru_saltb_y(iru)%salt(isalt)%wtsp,isalt=1,cs_db%num_salts), &
                           (ru_hru_saltb_y(iru)%salt(isalt)%irsw,isalt=1,cs_db%num_salts), &
                           (ru_hru_saltb_y(iru)%salt(isalt)%irgw,isalt=1,cs_db%num_salts), &
                           (ru_hru_saltb_y(iru)%salt(isalt)%irwo,isalt=1,cs_db%num_salts), &
                           (ru_hru_saltb_y(iru)%salt(isalt)%rain,isalt=1,cs_db%num_salts), &
                           (ru_hru_saltb_y(iru)%salt(isalt)%dryd,isalt=1,cs_db%num_salts), &
                           (ru_hru_saltb_y(iru)%salt(isalt)%road,isalt=1,cs_db%num_salts), &
                           (ru_hru_saltb_y(iru)%salt(isalt)%fert,isalt=1,cs_db%num_salts), &
                           (ru_hru_saltb_y(iru)%salt(isalt)%amnd,isalt=1,cs_db%num_salts), &
                           (ru_hru_saltb_y(iru)%salt(isalt)%uptk,isalt=1,cs_db%num_salts), &
                            ru_hru_saltb_y(iru)%salt(1)%diss
          if (pco%csvout == "y") then
            write (5075,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, iru, ob(iob)%gis_id, & 
                                          (rusaltb_y(iru)%hd(1)%salt(isalt),isalt=1,cs_db%num_salts), &
                                          (rusaltb_y(iru)%hd(2)%salt(isalt),isalt=1,cs_db%num_salts), &
                                          (rusaltb_y(iru)%hd(3)%salt(isalt),isalt=1,cs_db%num_salts), &
                                          (rusaltb_y(iru)%hd(4)%salt(isalt),isalt=1,cs_db%num_salts), &
                                          (rusaltb_y(iru)%hd(5)%salt(isalt),isalt=1,cs_db%num_salts), &
                                          (ru_hru_saltb_y(iru)%salt(isalt)%wtsp,isalt=1,cs_db%num_salts), &
                                          (ru_hru_saltb_y(iru)%salt(isalt)%irsw,isalt=1,cs_db%num_salts), &
                                          (ru_hru_saltb_y(iru)%salt(isalt)%irgw,isalt=1,cs_db%num_salts), &
                                          (ru_hru_saltb_y(iru)%salt(isalt)%irwo,isalt=1,cs_db%num_salts), &
                                          (ru_hru_saltb_y(iru)%salt(isalt)%rain,isalt=1,cs_db%num_salts), &
                                          (ru_hru_saltb_y(iru)%salt(isalt)%dryd,isalt=1,cs_db%num_salts), &
                                          (ru_hru_saltb_y(iru)%salt(isalt)%road,isalt=1,cs_db%num_salts), &
                                          (ru_hru_saltb_y(iru)%salt(isalt)%fert,isalt=1,cs_db%num_salts), &
                                          (ru_hru_saltb_y(iru)%salt(isalt)%amnd,isalt=1,cs_db%num_salts), &
                                          (ru_hru_saltb_y(iru)%salt(isalt)%uptk,isalt=1,cs_db%num_salts), &
                                           ru_hru_saltb_y(iru)%salt(1)%diss
          endif
        endif
        !zero out
        do isalt = 1, cs_db%num_salts
          do ihyd=1,5  
            rusaltb_y(iru)%hd(ihyd)%salt(isalt) = 0.
          enddo
          ru_hru_saltb_y(iru)%salt(isalt)%wtsp = 0.
          ru_hru_saltb_y(iru)%salt(isalt)%irsw = 0.
          ru_hru_saltb_y(iru)%salt(isalt)%irgw = 0.
          ru_hru_saltb_y(iru)%salt(isalt)%irwo = 0.
          ru_hru_saltb_y(iru)%salt(isalt)%rain = 0.
          ru_hru_saltb_y(iru)%salt(isalt)%dryd = 0.
          ru_hru_saltb_y(iru)%salt(isalt)%road = 0.
          ru_hru_saltb_y(iru)%salt(isalt)%fert = 0.
          ru_hru_saltb_y(iru)%salt(isalt)%amnd = 0.
          ru_hru_saltb_y(iru)%salt(isalt)%uptk = 0.
        enddo
      endif
      
      !average annual print
      if (time%end_sim == 1 .and. pco%salt_ru%a == "y") then
        !calculate average annual values
        do isalt = 1, cs_db%num_salts
          do ihyd=1,5  
            rusaltb_a(iru)%hd(ihyd)%salt(isalt) = rusaltb_a(iru)%hd(ihyd)%salt(isalt) / time%nbyr
          enddo
          ru_hru_saltb_a(iru)%salt(isalt)%wtsp = ru_hru_saltb_a(iru)%salt(isalt)%wtsp / time%nbyr
          ru_hru_saltb_a(iru)%salt(isalt)%irsw = ru_hru_saltb_a(iru)%salt(isalt)%irsw / time%nbyr
          ru_hru_saltb_a(iru)%salt(isalt)%irgw = ru_hru_saltb_a(iru)%salt(isalt)%irgw / time%nbyr
          ru_hru_saltb_a(iru)%salt(isalt)%irwo = ru_hru_saltb_a(iru)%salt(isalt)%irwo / time%nbyr
          ru_hru_saltb_a(iru)%salt(isalt)%rain = ru_hru_saltb_a(iru)%salt(isalt)%rain / time%nbyr
          ru_hru_saltb_a(iru)%salt(isalt)%dryd = ru_hru_saltb_a(iru)%salt(isalt)%dryd / time%nbyr
          ru_hru_saltb_a(iru)%salt(isalt)%road = ru_hru_saltb_a(iru)%salt(isalt)%road / time%nbyr
          ru_hru_saltb_a(iru)%salt(isalt)%fert = ru_hru_saltb_a(iru)%salt(isalt)%fert / time%nbyr
          ru_hru_saltb_a(iru)%salt(isalt)%amnd = ru_hru_saltb_a(iru)%salt(isalt)%amnd / time%nbyr
          ru_hru_saltb_a(iru)%salt(isalt)%uptk = ru_hru_saltb_a(iru)%salt(isalt)%uptk / time%nbyr
        enddo
        write (5076,100) time%day, time%mo, time%day_mo, time%yrc, iru, ob(iob)%gis_id, & 
                         (rusaltb_a(iru)%hd(1)%salt(isalt),isalt=1,cs_db%num_salts), &
                         (rusaltb_a(iru)%hd(2)%salt(isalt),isalt=1,cs_db%num_salts), &
                         (rusaltb_a(iru)%hd(3)%salt(isalt),isalt=1,cs_db%num_salts), &
                         (rusaltb_a(iru)%hd(4)%salt(isalt),isalt=1,cs_db%num_salts), &
                         (rusaltb_a(iru)%hd(5)%salt(isalt),isalt=1,cs_db%num_salts), &
                         (ru_hru_saltb_a(iru)%salt(isalt)%wtsp,isalt=1,cs_db%num_salts), &
                         (ru_hru_saltb_a(iru)%salt(isalt)%irsw,isalt=1,cs_db%num_salts), &
                         (ru_hru_saltb_a(iru)%salt(isalt)%irgw,isalt=1,cs_db%num_salts), &
                         (ru_hru_saltb_a(iru)%salt(isalt)%irwo,isalt=1,cs_db%num_salts), &
                         (ru_hru_saltb_a(iru)%salt(isalt)%rain,isalt=1,cs_db%num_salts), &
                         (ru_hru_saltb_a(iru)%salt(isalt)%dryd,isalt=1,cs_db%num_salts), &
                         (ru_hru_saltb_a(iru)%salt(isalt)%road,isalt=1,cs_db%num_salts), &
                         (ru_hru_saltb_a(iru)%salt(isalt)%fert,isalt=1,cs_db%num_salts), &
                         (ru_hru_saltb_a(iru)%salt(isalt)%amnd,isalt=1,cs_db%num_salts), &
                         (ru_hru_saltb_a(iru)%salt(isalt)%uptk,isalt=1,cs_db%num_salts), &
                          ru_hru_saltb_a(iru)%salt(1)%diss
        if (pco%csvout == "y") then
          write (5077,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, iru, ob(iob)%gis_id, & 
                                        (rusaltb_a(iru)%hd(1)%salt(isalt),isalt=1,cs_db%num_salts), &
                                        (rusaltb_a(iru)%hd(2)%salt(isalt),isalt=1,cs_db%num_salts), &
                                        (rusaltb_a(iru)%hd(3)%salt(isalt),isalt=1,cs_db%num_salts), &
                                        (rusaltb_a(iru)%hd(4)%salt(isalt),isalt=1,cs_db%num_salts), &
                                        (rusaltb_a(iru)%hd(5)%salt(isalt),isalt=1,cs_db%num_salts), &
                                        (ru_hru_saltb_a(iru)%salt(isalt)%wtsp,isalt=1,cs_db%num_salts), &
                                        (ru_hru_saltb_a(iru)%salt(isalt)%irsw,isalt=1,cs_db%num_salts), &
                                        (ru_hru_saltb_a(iru)%salt(isalt)%irgw,isalt=1,cs_db%num_salts), &
                                        (ru_hru_saltb_a(iru)%salt(isalt)%irwo,isalt=1,cs_db%num_salts), &
                                        (ru_hru_saltb_a(iru)%salt(isalt)%rain,isalt=1,cs_db%num_salts), &
                                        (ru_hru_saltb_a(iru)%salt(isalt)%dryd,isalt=1,cs_db%num_salts), &
                                        (ru_hru_saltb_a(iru)%salt(isalt)%road,isalt=1,cs_db%num_salts), &
																				(ru_hru_saltb_a(iru)%salt(isalt)%fert,isalt=1,cs_db%num_salts), &
                                        (ru_hru_saltb_a(iru)%salt(isalt)%amnd,isalt=1,cs_db%num_salts), &
                                        (ru_hru_saltb_a(iru)%salt(isalt)%uptk,isalt=1,cs_db%num_salts), &
                                         ru_hru_saltb_a(iru)%salt(1)%diss
        endif
      endif
     
      return
      
100   format (4i6,2i8,500e15.4)      

      end subroutine ru_salt_output