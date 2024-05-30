      subroutine ru_cs_output(iru) !rtb cs
    
      use time_module
      use basin_module
      use hydrograph_module, only : ob, sp_ob1
      use cs_module
      use constituent_mass_module
      
      implicit none
      
      integer, intent (in) :: iru        !             |
      integer :: iob                     !             |
      integer :: ics                     !						 |constituent counter
      integer :: ihyd                    !             |hydrograph counter
                         
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine outputs constituent mass loadings and concentrations from routing units
      
      iob = sp_ob1%ru + iru - 1 
      
      !note:
      !hd(1) = total constituent out
      !hd(2) = percolation
      !hd(3) = surface runoff
      !hd(4) = lateral flow
      !hd(5) = tile flow
      
      !add daily values to monthly values
      do ics=1,cs_db%num_cs
        do ihyd=1,5  
          rucsb_m(iru)%hd(ihyd)%cs(ics) = rucsb_m(iru)%hd(ihyd)%cs(ics) + rucsb_d(iru)%hd(ihyd)%cs(ics)
        enddo
        ru_hru_csb_m(iru)%cs(ics)%sedm = ru_hru_csb_m(iru)%cs(ics)%sedm + ru_hru_csb_d(iru)%cs(ics)%sedm
        ru_hru_csb_m(iru)%cs(ics)%wtsp = ru_hru_csb_m(iru)%cs(ics)%wtsp + ru_hru_csb_d(iru)%cs(ics)%wtsp
        ru_hru_csb_m(iru)%cs(ics)%irsw = ru_hru_csb_m(iru)%cs(ics)%irsw + ru_hru_csb_d(iru)%cs(ics)%irsw
        ru_hru_csb_m(iru)%cs(ics)%irgw = ru_hru_csb_m(iru)%cs(ics)%irgw + ru_hru_csb_d(iru)%cs(ics)%irgw
        ru_hru_csb_m(iru)%cs(ics)%irwo = ru_hru_csb_m(iru)%cs(ics)%irwo + ru_hru_csb_d(iru)%cs(ics)%irwo
        ru_hru_csb_m(iru)%cs(ics)%rain = ru_hru_csb_m(iru)%cs(ics)%rain + ru_hru_csb_d(iru)%cs(ics)%rain
        ru_hru_csb_m(iru)%cs(ics)%dryd = ru_hru_csb_m(iru)%cs(ics)%dryd + ru_hru_csb_d(iru)%cs(ics)%dryd
        ru_hru_csb_m(iru)%cs(ics)%fert = ru_hru_csb_m(iru)%cs(ics)%fert + ru_hru_csb_d(iru)%cs(ics)%fert
        ru_hru_csb_m(iru)%cs(ics)%uptk = ru_hru_csb_m(iru)%cs(ics)%uptk + ru_hru_csb_d(iru)%cs(ics)%uptk
        ru_hru_csb_m(iru)%cs(ics)%rctn = ru_hru_csb_m(iru)%cs(ics)%rctn + ru_hru_csb_d(iru)%cs(ics)%rctn
        ru_hru_csb_m(iru)%cs(ics)%sorb = ru_hru_csb_m(iru)%cs(ics)%sorb + ru_hru_csb_d(iru)%cs(ics)%sorb
      enddo     
      
      !daily print
      if (pco%cs_ru%d == "y") then
        write (6070,100) time%day, time%mo, time%day_mo, time%yrc, iru, ob(iob)%gis_id, & 
                         (rucsb_d(iru)%hd(1)%cs(ics),ics=1,cs_db%num_cs), & !total out
                         (rucsb_d(iru)%hd(2)%cs(ics),ics=1,cs_db%num_cs), & !percolation
                         (rucsb_d(iru)%hd(3)%cs(ics),ics=1,cs_db%num_cs), & !surface runoff
                         (rucsb_d(iru)%hd(4)%cs(ics),ics=1,cs_db%num_cs), & !soil lateral flow
                         (rucsb_d(iru)%hd(5)%cs(ics),ics=1,cs_db%num_cs), & !tile flow
                         (ru_hru_csb_d(iru)%cs(ics)%sedm,ics=1,cs_db%num_cs), &
                         (ru_hru_csb_d(iru)%cs(ics)%wtsp,ics=1,cs_db%num_cs), &
                         (ru_hru_csb_d(iru)%cs(ics)%irsw,ics=1,cs_db%num_cs), &
                         (ru_hru_csb_d(iru)%cs(ics)%irgw,ics=1,cs_db%num_cs), &
                         (ru_hru_csb_d(iru)%cs(ics)%irwo,ics=1,cs_db%num_cs), &
                         (ru_hru_csb_d(iru)%cs(ics)%rain,ics=1,cs_db%num_cs), &
                         (ru_hru_csb_d(iru)%cs(ics)%dryd,ics=1,cs_db%num_cs), &
                         (ru_hru_csb_d(iru)%cs(ics)%fert,ics=1,cs_db%num_cs), &
                         (ru_hru_csb_d(iru)%cs(ics)%uptk,ics=1,cs_db%num_cs), &
                         (ru_hru_csb_d(iru)%cs(ics)%rctn,ics=1,cs_db%num_cs), &
                         (ru_hru_csb_d(iru)%cs(ics)%sorb,ics=1,cs_db%num_cs)
        if (pco%csvout == "y") then
          write (6071,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, iru, ob(iob)%gis_id, & 
																		   (rucsb_d(iru)%hd(1)%cs(ics),ics=1,cs_db%num_cs), & !total out
                                       (rucsb_d(iru)%hd(2)%cs(ics),ics=1,cs_db%num_cs), & !percolation
                                       (rucsb_d(iru)%hd(3)%cs(ics),ics=1,cs_db%num_cs), & !surface runoff
                                       (rucsb_d(iru)%hd(4)%cs(ics),ics=1,cs_db%num_cs), & !soil lateral flow
                                       (rucsb_d(iru)%hd(5)%cs(ics),ics=1,cs_db%num_cs), & !tile flow
                                       (ru_hru_csb_d(iru)%cs(ics)%sedm,ics=1,cs_db%num_cs), &
                                       (ru_hru_csb_d(iru)%cs(ics)%wtsp,ics=1,cs_db%num_cs), &
                                       (ru_hru_csb_d(iru)%cs(ics)%irsw,ics=1,cs_db%num_cs), &
                                       (ru_hru_csb_d(iru)%cs(ics)%irgw,ics=1,cs_db%num_cs), &
                                       (ru_hru_csb_d(iru)%cs(ics)%irwo,ics=1,cs_db%num_cs), &
                                       (ru_hru_csb_d(iru)%cs(ics)%rain,ics=1,cs_db%num_cs), &
                                       (ru_hru_csb_d(iru)%cs(ics)%dryd,ics=1,cs_db%num_cs), &
                                       (ru_hru_csb_d(iru)%cs(ics)%fert,ics=1,cs_db%num_cs), &
                                       (ru_hru_csb_d(iru)%cs(ics)%uptk,ics=1,cs_db%num_cs), &
                                       (ru_hru_csb_d(iru)%cs(ics)%rctn,ics=1,cs_db%num_cs), &
                                       (ru_hru_csb_d(iru)%cs(ics)%sorb,ics=1,cs_db%num_cs)
        endif
      endif
      !zero out
      do ics = 1, cs_db%num_cs
        do ihyd=1,5  
          rucsb_d(iru)%hd(ihyd)%cs(ics) = 0.
        enddo
        ru_hru_csb_d(iru)%cs(ics)%sedm = 0.
        ru_hru_csb_d(iru)%cs(ics)%wtsp = 0.
        ru_hru_csb_d(iru)%cs(ics)%irsw = 0.
        ru_hru_csb_d(iru)%cs(ics)%irgw = 0.
        ru_hru_csb_d(iru)%cs(ics)%irwo = 0.
        ru_hru_csb_d(iru)%cs(ics)%rain = 0.
        ru_hru_csb_d(iru)%cs(ics)%dryd = 0.
        ru_hru_csb_d(iru)%cs(ics)%fert = 0.
        ru_hru_csb_d(iru)%cs(ics)%uptk = 0.
        ru_hru_csb_d(iru)%cs(ics)%rctn = 0.
        ru_hru_csb_d(iru)%cs(ics)%sorb = 0.
      enddo
      

      !monthly print
      if (time%end_mo == 1) then
        !add monthly values to yearly values
        do ics = 1, cs_db%num_cs
          do ihyd=1,5  
            rucsb_y(iru)%hd(ihyd)%cs(ics) = rucsb_y(iru)%hd(ihyd)%cs(ics) + rucsb_m(iru)%hd(ihyd)%cs(ics)
          enddo
          ru_hru_csb_y(iru)%cs(ics)%sedm = ru_hru_csb_y(iru)%cs(ics)%sedm + ru_hru_csb_m(iru)%cs(ics)%sedm
          ru_hru_csb_y(iru)%cs(ics)%wtsp = ru_hru_csb_y(iru)%cs(ics)%wtsp + ru_hru_csb_m(iru)%cs(ics)%wtsp
          ru_hru_csb_y(iru)%cs(ics)%irsw = ru_hru_csb_y(iru)%cs(ics)%irsw + ru_hru_csb_m(iru)%cs(ics)%irsw
          ru_hru_csb_y(iru)%cs(ics)%irgw = ru_hru_csb_y(iru)%cs(ics)%irgw + ru_hru_csb_m(iru)%cs(ics)%irgw
          ru_hru_csb_y(iru)%cs(ics)%irwo = ru_hru_csb_y(iru)%cs(ics)%irwo + ru_hru_csb_m(iru)%cs(ics)%irwo
          ru_hru_csb_y(iru)%cs(ics)%rain = ru_hru_csb_y(iru)%cs(ics)%rain + ru_hru_csb_m(iru)%cs(ics)%rain
          ru_hru_csb_y(iru)%cs(ics)%dryd = ru_hru_csb_y(iru)%cs(ics)%dryd + ru_hru_csb_m(iru)%cs(ics)%dryd
          ru_hru_csb_y(iru)%cs(ics)%fert = ru_hru_csb_y(iru)%cs(ics)%fert + ru_hru_csb_m(iru)%cs(ics)%fert
          ru_hru_csb_y(iru)%cs(ics)%uptk = ru_hru_csb_y(iru)%cs(ics)%uptk + ru_hru_csb_m(iru)%cs(ics)%uptk
          ru_hru_csb_y(iru)%cs(ics)%rctn = ru_hru_csb_y(iru)%cs(ics)%rctn + ru_hru_csb_m(iru)%cs(ics)%rctn
          ru_hru_csb_y(iru)%cs(ics)%sorb = ru_hru_csb_y(iru)%cs(ics)%sorb + ru_hru_csb_m(iru)%cs(ics)%sorb
        enddo
        if (pco%cs_ru%m == "y") then
          write (6072,100) time%day, time%mo, time%day_mo, time%yrc, iru, ob(iob)%gis_id, & 
                           (rucsb_m(iru)%hd(1)%cs(ics),ics=1,cs_db%num_cs), & !total out
                           (rucsb_m(iru)%hd(2)%cs(ics),ics=1,cs_db%num_cs), & !percolation
                           (rucsb_m(iru)%hd(3)%cs(ics),ics=1,cs_db%num_cs), & !surface runoff
                           (rucsb_m(iru)%hd(4)%cs(ics),ics=1,cs_db%num_cs), & !soil lateral flow
                           (rucsb_m(iru)%hd(5)%cs(ics),ics=1,cs_db%num_cs), & !tile flow
                           (ru_hru_csb_m(iru)%cs(ics)%sedm,ics=1,cs_db%num_cs), &
                           (ru_hru_csb_m(iru)%cs(ics)%wtsp,ics=1,cs_db%num_cs), &
                           (ru_hru_csb_m(iru)%cs(ics)%irsw,ics=1,cs_db%num_cs), &
                           (ru_hru_csb_m(iru)%cs(ics)%irgw,ics=1,cs_db%num_cs), &
                           (ru_hru_csb_m(iru)%cs(ics)%irwo,ics=1,cs_db%num_cs), &
                           (ru_hru_csb_m(iru)%cs(ics)%rain,ics=1,cs_db%num_cs), &
                           (ru_hru_csb_m(iru)%cs(ics)%dryd,ics=1,cs_db%num_cs), &
                           (ru_hru_csb_m(iru)%cs(ics)%fert,ics=1,cs_db%num_cs), &
                           (ru_hru_csb_m(iru)%cs(ics)%uptk,ics=1,cs_db%num_cs), &
                           (ru_hru_csb_m(iru)%cs(ics)%rctn,ics=1,cs_db%num_cs), &
                           (ru_hru_csb_m(iru)%cs(ics)%sorb,ics=1,cs_db%num_cs)
          if (pco%csvout == "y") then
            write (6073,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, iru, ob(iob)%gis_id, & 
                                         (rucsb_m(iru)%hd(1)%cs(ics),ics=1,cs_db%num_cs), & !total out
                                         (rucsb_m(iru)%hd(2)%cs(ics),ics=1,cs_db%num_cs), & !percolation
                                         (rucsb_m(iru)%hd(3)%cs(ics),ics=1,cs_db%num_cs), & !surface runoff
                                         (rucsb_m(iru)%hd(4)%cs(ics),ics=1,cs_db%num_cs), & !soil lateral flow
                                         (rucsb_m(iru)%hd(5)%cs(ics),ics=1,cs_db%num_cs), & !tile flow
                                         (ru_hru_csb_m(iru)%cs(ics)%sedm,ics=1,cs_db%num_cs), &
                                         (ru_hru_csb_m(iru)%cs(ics)%wtsp,ics=1,cs_db%num_cs), &
                                         (ru_hru_csb_m(iru)%cs(ics)%irsw,ics=1,cs_db%num_cs), &
                                         (ru_hru_csb_m(iru)%cs(ics)%irgw,ics=1,cs_db%num_cs), &
                                         (ru_hru_csb_m(iru)%cs(ics)%irwo,ics=1,cs_db%num_cs), &
                                         (ru_hru_csb_m(iru)%cs(ics)%rain,ics=1,cs_db%num_cs), &
                                         (ru_hru_csb_m(iru)%cs(ics)%dryd,ics=1,cs_db%num_cs), &
                                         (ru_hru_csb_m(iru)%cs(ics)%fert,ics=1,cs_db%num_cs), &
                                         (ru_hru_csb_m(iru)%cs(ics)%uptk,ics=1,cs_db%num_cs), &
                                         (ru_hru_csb_m(iru)%cs(ics)%rctn,ics=1,cs_db%num_cs), &
                                         (ru_hru_csb_m(iru)%cs(ics)%sorb,ics=1,cs_db%num_cs)
          endif
        endif
        !zero out
        do ics = 1, cs_db%num_cs
          do ihyd=1,5  
            rucsb_m(iru)%hd(ihyd)%cs(ics) = 0.
          enddo
          ru_hru_csb_m(iru)%cs(ics)%sedm = 0.
          ru_hru_csb_m(iru)%cs(ics)%wtsp = 0.
          ru_hru_csb_m(iru)%cs(ics)%irsw = 0.
          ru_hru_csb_m(iru)%cs(ics)%irgw = 0.
          ru_hru_csb_m(iru)%cs(ics)%irwo = 0.
          ru_hru_csb_m(iru)%cs(ics)%rain = 0.
          ru_hru_csb_m(iru)%cs(ics)%dryd = 0.
          ru_hru_csb_m(iru)%cs(ics)%fert = 0.
          ru_hru_csb_m(iru)%cs(ics)%uptk = 0.
          ru_hru_csb_m(iru)%cs(ics)%rctn = 0.
          ru_hru_csb_m(iru)%cs(ics)%sorb = 0.
        enddo
      endif
      
      !yearly print
      if (time%end_yr == 1) then
        !add yearly values to total values
        do ics = 1, cs_db%num_cs
          do ihyd=1,5  
            rucsb_a(iru)%hd(ihyd)%cs(ics) = rucsb_a(iru)%hd(ihyd)%cs(ics) + rucsb_y(iru)%hd(ihyd)%cs(ics)
          enddo
          ru_hru_csb_a(iru)%cs(ics)%sedm = ru_hru_csb_a(iru)%cs(ics)%sedm + ru_hru_csb_y(iru)%cs(ics)%sedm
          ru_hru_csb_a(iru)%cs(ics)%wtsp = ru_hru_csb_a(iru)%cs(ics)%wtsp + ru_hru_csb_y(iru)%cs(ics)%wtsp
          ru_hru_csb_a(iru)%cs(ics)%irsw = ru_hru_csb_a(iru)%cs(ics)%irsw + ru_hru_csb_y(iru)%cs(ics)%irsw
          ru_hru_csb_a(iru)%cs(ics)%irgw = ru_hru_csb_a(iru)%cs(ics)%irgw + ru_hru_csb_y(iru)%cs(ics)%irgw
          ru_hru_csb_a(iru)%cs(ics)%irwo = ru_hru_csb_a(iru)%cs(ics)%irwo + ru_hru_csb_y(iru)%cs(ics)%irwo
          ru_hru_csb_a(iru)%cs(ics)%rain = ru_hru_csb_a(iru)%cs(ics)%rain + ru_hru_csb_y(iru)%cs(ics)%rain
          ru_hru_csb_a(iru)%cs(ics)%dryd = ru_hru_csb_a(iru)%cs(ics)%dryd + ru_hru_csb_y(iru)%cs(ics)%dryd
          ru_hru_csb_a(iru)%cs(ics)%fert = ru_hru_csb_a(iru)%cs(ics)%fert + ru_hru_csb_y(iru)%cs(ics)%fert
          ru_hru_csb_a(iru)%cs(ics)%uptk = ru_hru_csb_a(iru)%cs(ics)%uptk + ru_hru_csb_y(iru)%cs(ics)%uptk
          ru_hru_csb_a(iru)%cs(ics)%rctn = ru_hru_csb_a(iru)%cs(ics)%rctn + ru_hru_csb_y(iru)%cs(ics)%rctn
          ru_hru_csb_a(iru)%cs(ics)%sorb = ru_hru_csb_a(iru)%cs(ics)%sorb + ru_hru_csb_y(iru)%cs(ics)%sorb
        enddo
        if (pco%cs_ru%y == "y") then
          write (6074,100) time%day, time%mo, time%day_mo, time%yrc, iru, ob(iob)%gis_id, & 
                           (rucsb_y(iru)%hd(1)%cs(ics),ics=1,cs_db%num_cs), & !total out
                           (rucsb_y(iru)%hd(2)%cs(ics),ics=1,cs_db%num_cs), & !percolation
                           (rucsb_y(iru)%hd(3)%cs(ics),ics=1,cs_db%num_cs), & !surface runoff
                           (rucsb_y(iru)%hd(4)%cs(ics),ics=1,cs_db%num_cs), & !soil lateral flow
                           (rucsb_y(iru)%hd(5)%cs(ics),ics=1,cs_db%num_cs), & !tile flow
                           (ru_hru_csb_y(iru)%cs(ics)%sedm,ics=1,cs_db%num_cs), &
                           (ru_hru_csb_y(iru)%cs(ics)%wtsp,ics=1,cs_db%num_cs), &
                           (ru_hru_csb_y(iru)%cs(ics)%irsw,ics=1,cs_db%num_cs), &
                           (ru_hru_csb_y(iru)%cs(ics)%irgw,ics=1,cs_db%num_cs), &
                           (ru_hru_csb_y(iru)%cs(ics)%irwo,ics=1,cs_db%num_cs), &
                           (ru_hru_csb_y(iru)%cs(ics)%rain,ics=1,cs_db%num_cs), &
                           (ru_hru_csb_y(iru)%cs(ics)%dryd,ics=1,cs_db%num_cs), &
                           (ru_hru_csb_y(iru)%cs(ics)%fert,ics=1,cs_db%num_cs), &
                           (ru_hru_csb_y(iru)%cs(ics)%uptk,ics=1,cs_db%num_cs), &
                           (ru_hru_csb_y(iru)%cs(ics)%rctn,ics=1,cs_db%num_cs), &
                           (ru_hru_csb_y(iru)%cs(ics)%sorb,ics=1,cs_db%num_cs)
          if (pco%csvout == "y") then
            write (6075,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, iru, ob(iob)%gis_id, & 
                                         (rucsb_y(iru)%hd(1)%cs(ics),ics=1,cs_db%num_cs), & !total out
                                         (rucsb_y(iru)%hd(2)%cs(ics),ics=1,cs_db%num_cs), & !percolation
                                         (rucsb_y(iru)%hd(3)%cs(ics),ics=1,cs_db%num_cs), & !surface runoff
                                         (rucsb_y(iru)%hd(4)%cs(ics),ics=1,cs_db%num_cs), & !soil lateral flow
                                         (rucsb_y(iru)%hd(5)%cs(ics),ics=1,cs_db%num_cs), & !tile flow
                                         (ru_hru_csb_y(iru)%cs(ics)%sedm,ics=1,cs_db%num_cs), &
                                         (ru_hru_csb_y(iru)%cs(ics)%wtsp,ics=1,cs_db%num_cs), &
                                         (ru_hru_csb_y(iru)%cs(ics)%irsw,ics=1,cs_db%num_cs), &
                                         (ru_hru_csb_y(iru)%cs(ics)%irgw,ics=1,cs_db%num_cs), &
                                         (ru_hru_csb_y(iru)%cs(ics)%irwo,ics=1,cs_db%num_cs), &
                                         (ru_hru_csb_y(iru)%cs(ics)%rain,ics=1,cs_db%num_cs), &
                                         (ru_hru_csb_y(iru)%cs(ics)%dryd,ics=1,cs_db%num_cs), &
                                         (ru_hru_csb_y(iru)%cs(ics)%fert,ics=1,cs_db%num_cs), &
                                         (ru_hru_csb_y(iru)%cs(ics)%uptk,ics=1,cs_db%num_cs), &
                                         (ru_hru_csb_y(iru)%cs(ics)%rctn,ics=1,cs_db%num_cs), &
                                         (ru_hru_csb_y(iru)%cs(ics)%sorb,ics=1,cs_db%num_cs)
          endif
        endif
        !zero out
        do ics = 1, cs_db%num_cs
          do ihyd=1,5  
            rucsb_y(iru)%hd(ihyd)%cs(ics) = 0.
          enddo
          ru_hru_csb_y(iru)%cs(ics)%sedm = 0.
          ru_hru_csb_y(iru)%cs(ics)%wtsp = 0.
          ru_hru_csb_y(iru)%cs(ics)%irsw = 0.
          ru_hru_csb_y(iru)%cs(ics)%irgw = 0.
          ru_hru_csb_y(iru)%cs(ics)%irwo = 0.
          ru_hru_csb_y(iru)%cs(ics)%rain = 0.
          ru_hru_csb_y(iru)%cs(ics)%dryd = 0.
          ru_hru_csb_y(iru)%cs(ics)%fert = 0.
          ru_hru_csb_y(iru)%cs(ics)%uptk = 0.
          ru_hru_csb_y(iru)%cs(ics)%rctn = 0.
          ru_hru_csb_y(iru)%cs(ics)%sorb = 0.
        enddo
      endif
      
      !average annual print
      if (time%end_sim == 1 .and. pco%cs_ru%a == "y") then
        !calculate average annual values
        do ics = 1, cs_db%num_cs
          do ihyd=1,5  
            rucsb_a(iru)%hd(ihyd)%cs(ics) = rucsb_a(iru)%hd(ihyd)%cs(ics) / time%nbyr
          enddo
          ru_hru_csb_a(iru)%cs(ics)%sedm = ru_hru_csb_a(iru)%cs(ics)%sedm / time%nbyr
          ru_hru_csb_a(iru)%cs(ics)%wtsp = ru_hru_csb_a(iru)%cs(ics)%wtsp / time%nbyr
          ru_hru_csb_a(iru)%cs(ics)%irsw = ru_hru_csb_a(iru)%cs(ics)%irsw / time%nbyr
          ru_hru_csb_a(iru)%cs(ics)%irgw = ru_hru_csb_a(iru)%cs(ics)%irgw / time%nbyr
          ru_hru_csb_a(iru)%cs(ics)%irwo = ru_hru_csb_a(iru)%cs(ics)%irwo / time%nbyr
          ru_hru_csb_a(iru)%cs(ics)%rain = ru_hru_csb_a(iru)%cs(ics)%rain / time%nbyr
          ru_hru_csb_a(iru)%cs(ics)%dryd = ru_hru_csb_a(iru)%cs(ics)%dryd / time%nbyr
          ru_hru_csb_a(iru)%cs(ics)%fert = ru_hru_csb_a(iru)%cs(ics)%fert / time%nbyr
          ru_hru_csb_a(iru)%cs(ics)%uptk = ru_hru_csb_a(iru)%cs(ics)%uptk / time%nbyr
          ru_hru_csb_a(iru)%cs(ics)%rctn = ru_hru_csb_a(iru)%cs(ics)%rctn / time%nbyr
          ru_hru_csb_a(iru)%cs(ics)%sorb = ru_hru_csb_a(iru)%cs(ics)%sorb / time%nbyr
        enddo
        write (6076,100) time%day, time%mo, time%day_mo, time%yrc, iru, ob(iob)%gis_id, & 
                        (rucsb_a(iru)%hd(1)%cs(ics),ics=1,cs_db%num_cs), & !total out
                        (rucsb_a(iru)%hd(2)%cs(ics),ics=1,cs_db%num_cs), & !percolation
                        (rucsb_a(iru)%hd(3)%cs(ics),ics=1,cs_db%num_cs), & !surface runoff
                        (rucsb_a(iru)%hd(4)%cs(ics),ics=1,cs_db%num_cs), & !soil lateral flow
                        (rucsb_a(iru)%hd(5)%cs(ics),ics=1,cs_db%num_cs), & !tile flow
                        (ru_hru_csb_a(iru)%cs(ics)%sedm,ics=1,cs_db%num_cs), &
                        (ru_hru_csb_a(iru)%cs(ics)%wtsp,ics=1,cs_db%num_cs), &
                        (ru_hru_csb_a(iru)%cs(ics)%irsw,ics=1,cs_db%num_cs), &
                        (ru_hru_csb_a(iru)%cs(ics)%irgw,ics=1,cs_db%num_cs), &
                        (ru_hru_csb_a(iru)%cs(ics)%irwo,ics=1,cs_db%num_cs), &
                        (ru_hru_csb_a(iru)%cs(ics)%rain,ics=1,cs_db%num_cs), &
                        (ru_hru_csb_a(iru)%cs(ics)%dryd,ics=1,cs_db%num_cs), &
                        (ru_hru_csb_a(iru)%cs(ics)%fert,ics=1,cs_db%num_cs), &
                        (ru_hru_csb_a(iru)%cs(ics)%uptk,ics=1,cs_db%num_cs), &
                        (ru_hru_csb_a(iru)%cs(ics)%rctn,ics=1,cs_db%num_cs), &
                        (ru_hru_csb_a(iru)%cs(ics)%sorb,ics=1,cs_db%num_cs)
        if (pco%csvout == "y") then
          write (6077,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, iru, ob(iob)%gis_id, & 
                                        (rucsb_a(iru)%hd(1)%cs(ics),ics=1,cs_db%num_cs), & !total out
                                        (rucsb_a(iru)%hd(2)%cs(ics),ics=1,cs_db%num_cs), & !percolation
                                        (rucsb_a(iru)%hd(3)%cs(ics),ics=1,cs_db%num_cs), & !surface runoff
                                        (rucsb_a(iru)%hd(4)%cs(ics),ics=1,cs_db%num_cs), & !soil lateral flow
                                        (rucsb_a(iru)%hd(5)%cs(ics),ics=1,cs_db%num_cs), & !tile flow
                                        (ru_hru_csb_a(iru)%cs(ics)%sedm,ics=1,cs_db%num_cs), &
                                        (ru_hru_csb_a(iru)%cs(ics)%wtsp,ics=1,cs_db%num_cs), &
                                        (ru_hru_csb_a(iru)%cs(ics)%irsw,ics=1,cs_db%num_cs), &
                                        (ru_hru_csb_a(iru)%cs(ics)%irgw,ics=1,cs_db%num_cs), &
                                        (ru_hru_csb_a(iru)%cs(ics)%irwo,ics=1,cs_db%num_cs), &
                                        (ru_hru_csb_a(iru)%cs(ics)%rain,ics=1,cs_db%num_cs), &
                                        (ru_hru_csb_a(iru)%cs(ics)%dryd,ics=1,cs_db%num_cs), &
                                        (ru_hru_csb_a(iru)%cs(ics)%fert,ics=1,cs_db%num_cs), &
                                        (ru_hru_csb_a(iru)%cs(ics)%uptk,ics=1,cs_db%num_cs), &
                                        (ru_hru_csb_a(iru)%cs(ics)%rctn,ics=1,cs_db%num_cs), &
                                        (ru_hru_csb_a(iru)%cs(ics)%sorb,ics=1,cs_db%num_cs)
        endif
      endif
    
      return
      
100   format (4i6,2i8,500e15.4)      

      end subroutine ru_cs_output