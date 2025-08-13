      subroutine lsu_output
      
      use time_module
      use basin_module
      use maximum_data_module
      use calibration_data_module
      use hydrograph_module
      use output_landscape_module
      
      implicit none
      
      integer :: ilsu = 0       !none       |counter
      integer :: ielem = 0      !none       |counter
      integer :: ihru = 0       !none       |counter 
      integer :: iob = 0        !none       |counter 
      real :: const = 0.        !           |  
      real :: sw_init = 0.      !           |
      real :: sno_init = 0.     !           |
          
      !! zero daily outputs before summing
      do ilsu = 1, db_mx%lsu_out
        sw_init = ruwb_d(ilsu)%sw_init
        sno_init = ruwb_d(ilsu)%sno_init
        ruwb_d(ilsu) = hwbz
        ruwb_d(ilsu)%sw_init = sw_init
        ruwb_d(ilsu)%sno_init = sno_init
        runb_d(ilsu) = hnbz
        ruls_d(ilsu) = hlsz
        rupw_d(ilsu) = hpwz
      end do
                     
      do ilsu = 1, db_mx%lsu_out
        ! summing HRU output for the landscape unit
        do ielem = 1, lsu_out(ilsu)%num_tot
          ihru = lsu_out(ilsu)%num(ielem)
          iob = sp_ob1%ru + ilsu - 1
          if (lsu_elem(ihru)%ru_frac > 1.e-9) then
            const = lsu_elem(ihru)%ru_frac
            if (lsu_elem(ihru)%obtyp == "hru") then
              ruwb_d(ilsu) = ruwb_d(ilsu) + hwb_d(ihru) * const
              !ruwb_d(ilsu)%sw_init = ruwb_d(ilsu)%sw_init + hwb_d(ihru)%sw_init * const
              ruwb_d(ilsu)%sw_final = ruwb_d(ilsu)%sw_final + hwb_d(ihru)%sw_final * const
              !ruwb_d(ilsu)%sno_init = ruwb_d(ilsu)%sno_init + hwb_d(ihru)%sno_init * const
              ruwb_d(ilsu)%sno_final = ruwb_d(ilsu)%sno_final + hwb_d(ihru)%sno_final * const
              runb_d(ilsu) = runb_d(ilsu) + hnb_d(ihru) * const
              ruls_d(ilsu) = ruls_d(ilsu) + hls_d(ihru) * const
              rupw_d(ilsu) = rupw_d(ilsu) + hpw_d(ihru) * const
            end if
            ! summing HRU_LTE output
            if (lsu_elem(ihru)%obtyp == "hlt") then
              ruwb_d(ilsu) = ruwb_d(ilsu) + hltwb_d(ihru) * const
              !ruwb_d(ilsu)%sw_init = ruwb_d(ilsu)%sw_init + hltwb_d(ihru)%sw_init * const
              ruwb_d(ilsu)%sw_final = ruwb_d(ilsu)%sw_final + hltwb_d(ihru)%sw_final * const
              runb_d(ilsu) = runb_d(ilsu) + hltnb_d(ihru) * const
              ruls_d(ilsu) = ruls_d(ilsu) + hltls_d(ihru) * const
              rupw_d(ilsu) = rupw_d(ilsu) + hltpw_d(ihru) * const
            end if
          end if
        end do    !ielem
      
        !! sum monthly variables
        ruwb_m(ilsu) = ruwb_m(ilsu) + ruwb_d(ilsu)
        runb_m(ilsu) = runb_m(ilsu) + runb_d(ilsu)
        ruls_m(ilsu) = ruls_m(ilsu) + ruls_d(ilsu)
        rupw_m(ilsu) = rupw_m(ilsu) + rupw_d(ilsu)
        
        !! daily print - LANDSCAPE UNIT
         if (pco%day_print == "y" .and. pco%int_day_cur == pco%int_day) then
          if (pco%wb_lsu%d == "y") then
            ruwb_d(ilsu)%sw = (ruwb_d(ilsu)%sw_init + ruwb_d(ilsu)%sw_final) / 2.
            ruwb_d(ilsu)%snopack = (ruwb_d(ilsu)%sno_init + ruwb_d(ilsu)%sno_final) / 2.
            write (2140,100) time%day, time%mo, time%day_mo, time%yrc, ilsu, ob(iob)%gis_id, lsu_out(ilsu)%name, ruwb_d(ilsu)  !! waterbal
            if (pco%csvout == "y") then 
              write (2144,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, &
                ilsu, ob(iob)%gis_id, lsu_out(ilsu)%name, ruwb_d(ilsu)  !! waterbal
            end if 
            ruwb_d(ilsu)%sw_init = ruwb_d(ilsu)%sw_final
            ruwb_d(ilsu)%sno_init = ruwb_d(ilsu)%sno_final
          end if 
          if (pco%nb_lsu%d == "y") then
            write (2150,103) time%day, time%mo, time%day_mo, time%yrc, ilsu, ob(iob)%gis_id, lsu_out(ilsu)%name, runb_d(ilsu)  !! nutrient bal
            if (pco%csvout == "y") then 
              write (2154,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, &
                ilsu, ob(iob)%gis_id, lsu_out(ilsu)%name, runb_d(ilsu)  !! nutrient bal
            end if 
          end if
          if (pco%ls_lsu%d == "y") then
            write (2160,100) time%day, time%mo, time%day_mo, time%yrc, ilsu, ob(iob)%gis_id, lsu_out(ilsu)%name, ruls_d(ilsu)  !! losses
            if (pco%csvout == "y") then 
              write (2164,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, &
                ilsu, ob(iob)%gis_id, lsu_out(ilsu)%name, ruls_d(ilsu)  !! losses
            end if 
          end if
          if (pco%pw_lsu%d == "y") then
            write (2170,100) time%day, time%mo, time%day_mo, time%yrc, ilsu, ob(iob)%gis_id, lsu_out(ilsu)%name, rupw_d(ilsu)  !! plant weather
            if (pco%csvout == "y") then 
              write (2175,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, &
                ilsu, ob(iob)%gis_id, lsu_out(ilsu)%name, rupw_d(ilsu)  !! plant weather 
            end if
          end if 
         end if

        !! check end of month
        if (time%end_mo == 1) then
          ruwb_y(ilsu) = ruwb_y(ilsu) + ruwb_m(ilsu)
          runb_y(ilsu) = runb_y(ilsu) + runb_m(ilsu)
          ruls_y(ilsu) = ruls_y(ilsu) + ruls_m(ilsu)
          rupw_y(ilsu) = rupw_y(ilsu) + rupw_m(ilsu)
          
          const = float (ndays(time%mo + 1) - ndays(time%mo)) 
          rupw_m(ilsu) = rupw_m(ilsu) // const
          ruwb_m(ilsu) = ruwb_m(ilsu) // const 
          
          if (pco%wb_lsu%m == "y") then
            ruwb_m(ilsu)%sw_final = ruwb_d(ilsu)%sw_final
            ruwb_m(ilsu)%sno_final = ruwb_d(ilsu)%sno_final
            write (2141,100) time%day, time%mo, time%day_mo, time%yrc, ilsu, ob(iob)%gis_id, lsu_out(ilsu)%name, ruwb_m(ilsu)
            if (pco%csvout == "y") then 
              write (2145,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, &
                ilsu, ob(iob)%gis_id, lsu_out(ilsu)%name, ruwb_m(ilsu)
            end if
            ruwb_m(ilsu)%sw_init = ruwb_m(ilsu)%sw_final
            ruwb_m(ilsu)%sno_init = ruwb_m(ilsu)%sno_final
          end if
          if (pco%nb_lsu%m == "y") then 
            write (2151,103) time%day, time%mo, time%day_mo, time%yrc, ilsu, ob(iob)%gis_id, lsu_out(ilsu)%name, runb_m(ilsu)
            if (pco%csvout == "y") then 
              write (2155,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, &
                ilsu, ob(iob)%gis_id, lsu_out(ilsu)%name, runb_m(ilsu)
            end if 
          end if
          if (pco%ls_lsu%m == "y") then
            write (2161,100) time%day, time%mo, time%day_mo, time%yrc, ilsu, ob(iob)%gis_id, lsu_out(ilsu)%name, ruls_m(ilsu)
            if (pco%csvout == "y") then 
              write (2165,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, &
                ilsu, ob(iob)%gis_id, lsu_out(ilsu)%name, ruls_m(ilsu)
            end if 
          end if
          if (pco%pw_lsu%m == "y") then
            rupw_m(ilsu)%nplnt = rupw_d(ilsu)%nplnt
            rupw_m(ilsu)%pplnt = rupw_d(ilsu)%pplnt
            write (2171,100) time%day, time%mo, time%day_mo, time%yrc, ilsu, ob(iob)%gis_id, lsu_out(ilsu)%name, rupw_m(ilsu)
            if (pco%csvout == "y") then 
              write (2175,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, &
                ilsu, ob(iob)%gis_id, lsu_out(ilsu)%name, rupw_m(ilsu)
            end if 
          end if
  
          sw_init = ruwb_m(ilsu)%sw_final
          sno_init = ruwb_m(ilsu)%sno_final
          ruwb_m(ilsu) = hwbz
          ruwb_m(ilsu)%sw_init = sw_init
          ruwb_m(ilsu)%sno_init = sno_init
          runb_m(ilsu) = hnbz
          ruls_m(ilsu) = hlsz
          rupw_m(ilsu) = hpwz
        end if

        !!  check end of year
        if (time%end_yr == 1) then
           ruwb_a(ilsu) = ruwb_a(ilsu) + ruwb_y(ilsu)
           runb_a(ilsu) = runb_a(ilsu) + runb_y(ilsu)
           ruls_a(ilsu) = ruls_a(ilsu) + ruls_y(ilsu)
           rupw_a(ilsu) = rupw_a(ilsu) + rupw_y(ilsu)
           const = time%day_end_yr
           ruwb_y(ilsu) = ruwb_y(ilsu) // const
           rupw_y(ilsu) = rupw_y(ilsu) // const

           if (pco%wb_lsu%y == "y") then
             ruwb_y(ilsu)%sw_final = ruwb_d(ilsu)%sw_final
             ruwb_y(ilsu)%sno_final = ruwb_d(ilsu)%sno_final
             write (2142,100) time%day, time%mo, time%day_mo, time%yrc, ilsu, ob(iob)%gis_id, lsu_out(ilsu)%name, ruwb_y(ilsu)
             if (pco%csvout == "y") then 
               write (2146,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, &
                ilsu, ob(iob)%gis_id, lsu_out(ilsu)%name, ruwb_y(ilsu)
             end if
             ruwb_y(ilsu)%sw_init = ruwb_y(ilsu)%sw_final
             ruwb_y(ilsu)%sno_init = ruwb_y(ilsu)%sno_final
           end if
           if (pco%nb_lsu%y == "y") then
             write (2152,103) time%day, time%mo, time%day_mo, time%yrc, ilsu, ob(iob)%gis_id, lsu_out(ilsu)%name, runb_y(ilsu)
             if (pco%csvout == "y") then 
               write (2156,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, &
                ilsu, ob(iob)%gis_id, lsu_out(ilsu)%name, runb_y(ilsu)
             end if 
           end if
           if (pco%ls_lsu%y == "y") then
             write (2162,102) time%day, time%mo, time%day_mo, time%yrc, ilsu, ob(iob)%gis_id, lsu_out(ilsu)%name, ruls_y(ilsu)
             if (pco%csvout == "y") then 
               write (2166,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, &
                ilsu, ob(iob)%gis_id, lsu_out(ilsu)%name, ruls_y(ilsu)
             end if 
           end if
           if (pco%pw_lsu%y == "y") then
             rupw_y(ilsu)%nplnt = rupw_d(ilsu)%nplnt
             rupw_y(ilsu)%pplnt = rupw_d(ilsu)%pplnt
             write (2172,102) time%day, time%mo, time%day_mo, time%yrc, ilsu, ob(iob)%gis_id, lsu_out(ilsu)%name, rupw_y(ilsu)
             if (pco%csvout == "y") then 
               write (2176,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, &
                ilsu, ob(iob)%gis_id, lsu_out(ilsu)%name, rupw_y(ilsu)
             end if 
           end if
 
          !! zero yearly variables
          sw_init = ruwb_y(ilsu)%sw_final
          sno_init = ruwb_y(ilsu)%sno_final
          ruwb_y(ilsu) = hwbz
          ruwb_y(ilsu)%sw_init = sw_init
          ruwb_y(ilsu)%sno_init = sno_init
          runb_y(ilsu) = hnbz
          ruls_y(ilsu) = hlsz
          rupw_y(ilsu) = hpwz
        end if
        
      !! average annual print - LANDSCAPE UNIT
      if (time%end_sim == 1 .and. pco%wb_lsu%a == "y") then
        sw_init = ruwb_a(ilsu)%sw_init
        sno_init = ruwb_a(ilsu)%sno_init
        ruwb_a(ilsu) = ruwb_a(ilsu) / time%yrs_prt
        ruwb_a(ilsu) = ruwb_a(ilsu) // time%days_prt
        ruwb_a(ilsu)%sw_init = sw_init
        ruwb_a(ilsu)%sw_final = ruwb_d(ilsu)%sw_final
        ruwb_a(ilsu)%sno_init = sno_init
        ruwb_a(ilsu)%sno_final = ruwb_d(ilsu)%sno_final
        
        write (2143,100) time%day, time%mo, time%day_mo, time%yrc, ilsu, ob(iob)%gis_id, lsu_out(ilsu)%name, ruwb_a(ilsu)
        if (pco%csvout == "y") then 
          write (2147,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, ilsu, ob(iob)%gis_id, lsu_out(ilsu)%name, ruwb_a(ilsu)
        end if 
      end if
      if (time%end_sim == 1 .and. pco%nb_lsu%a == "y") then
        runb_a(ilsu) = runb_a(ilsu) / time%yrs_prt
        write (2153,103) time%day, time%mo, time%day_mo, time%yrc, ilsu, ob(iob)%gis_id, lsu_out(ilsu)%name, runb_a(ilsu)
        if (pco%csvout == "y") then 
          write (2157,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, ilsu, ob(iob)%gis_id, lsu_out(ilsu)%name, runb_a(ilsu)
        end if
      end if
      if (time%end_sim == 1 .and. pco%ls_lsu%a == "y") then     
        ruls_a(ilsu) = ruls_a(ilsu) / time%yrs_prt
        write (2163,102) time%day, time%mo, time%day_mo, time%yrc, ilsu, ob(iob)%gis_id, lsu_out(ilsu)%name, ruls_a(ilsu)
        if (pco%csvout == "y") then 
          write (2167,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, ilsu, ob(iob)%gis_id, lsu_out(ilsu)%name, ruls_a(ilsu)
        end if 
      end if
      if (time%end_sim == 1 .and. pco%pw_lsu%a == "y") then    
        rupw_a(ilsu) = rupw_a(ilsu) / time%yrs_prt
        rupw_a(ilsu) = rupw_a(ilsu) // time%days_prt
        rupw_a(ilsu)%nplnt = rupw_d(ilsu)%nplnt
        rupw_a(ilsu)%pplnt = rupw_d(ilsu)%pplnt
        write (2173,102) time%day, time%mo, time%day_mo, time%yrc, ilsu, ob(iob)%gis_id, lsu_out(ilsu)%name, rupw_a(ilsu) 
        if (pco%csvout == "y") then 
          write (2177,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, ilsu, ob(iob)%gis_id, lsu_out(ilsu)%name, rupw_a(ilsu)
        end if
      end if
      end do    !ilsu
      
      return
      
100   format (1x,4i6,i7,i8,2x,a,42f12.3)
102   format (1x,4i6,i7,i8,2x,a,40f12.3)
!103   format (4i6,i8,a,2x,a,6f12.3,29f17.3)
103   format (4i6,i7,i8,2x,a,4f12.3,23f17.3)
!*** tu Wunused-label: 104   format (4i6,i8,a,2x,a,6f12.3,29f17.3)
       
      end subroutine lsu_output
