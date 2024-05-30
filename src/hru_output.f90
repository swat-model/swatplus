      subroutine hru_output (ihru)

      use plant_module
      use plant_data_module
      use time_module
      use basin_module
      use output_landscape_module
      use hydrograph_module, only : sp_ob1, ob
      use organic_mineral_mass_module
      use soil_module
      use carbon_module
      use hru_module, only : hru
      
      implicit none
      
      integer, intent (in) :: ihru             !            |
      integer :: idp                           !            |
      integer :: j
      integer :: iob
      integer :: ipl
      real :: const
      real :: sw_init
      real :: sno_init
                         
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine outputs HRU variables on daily, monthly and annual time steps

      j = ihru
      
      iob = sp_ob1%hru + j - 1   !!!!!! added for new output write
          
        hwb_m(j) = hwb_m(j) + hwb_d(j)
        hnb_m(j) = hnb_m(j) + hnb_d(j)
        hls_m(j) = hls_m(j) + hls_d(j) 
        hpw_m(j) = hpw_m(j) + hpw_d(j)
        
        hwb_d(j)%sw_final = soil(j)%sw
        hwb_d(j)%sw = (hwb_d(j)%sw_init + hwb_d(j)%sw_final) / 2.
        hwb_d(j)%sno_final = hru(j)%sno_mm
        hwb_d(j)%snopack = (hwb_d(j)%sno_init + hwb_d(j)%sno_final) / 2.
             
      !! daily print
         if (pco%day_print == "y" .and. pco%int_day_cur == pco%int_day) then
          if (pco%wb_hru%d == "y") then
             write (2000,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hwb_d(j)   !! waterbal
             if (pco%csvout == "y") then
               !! changed write unit below (2004 to write file data)
               write (2004,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hwb_d(j)  !! waterbal
             end if
          end if
          hwb_d(j)%sw_init = hwb_d(j)%sw_final
          hwb_d(j)%sno_init = hwb_d(j)%sno_final
          
          if (pco%nb_hru%d == "y") then
            write (2020,104) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hnb_d(j)  !! nutrient bal
            if (pco%csvout == "y") then
                write (2024,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hnb_d(j)  !! nutrient bal
                end if
          end if
          if (pco%ls_hru%d == "y") then
            write (2030,102) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hls_d(j)  !! losses
            if (pco%csvout == "y") then
                write (2034,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hls_d(j)  !! losses
            end if
          end if
          if (pco%pw_hru%d == "y") then
            write (2040,101) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hpw_d(j)  !! plant weather 
              if (pco%csvout == "y") then 
                write (2044,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hpw_d(j)  !! plant weather
              end if 
          end if
        end if
         
        !! check end of month
        if (time%end_mo == 1) then
          hwb_y(j) = hwb_y(j) + hwb_m(j)
          hnb_y(j) = hnb_y(j) + hnb_m(j)
          hls_y(j) = hls_y(j) + hls_m(j)
          hpw_y(j) = hpw_y(j) + hpw_m(j)
          
          const = float (ndays(time%mo + 1) - ndays(time%mo))
          hpw_m(j) = hpw_m(j) // const
          hwb_m(j) = hwb_m(j) // const
          
          !! monthly print
           hwb_m(j)%sw_final = hwb_d(j)%sw_final
           hwb_m(j)%sno_final = hwb_d(j)%sno_final
           
           if (pco%wb_hru%m == "y") then
             write (2001,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hwb_m(j)
               if (pco%csvout == "y") then
                 write (2005,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hwb_m(j)
               end if
           end if
           
           if (pco%nb_hru%m == "y") then
             write (2021,104) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hnb_m(j)
             if (pco%csvout == "y") then
                 write (2025,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hnb_m(j)
                 end if
           end if
           
           if (pco%ls_hru%m == "y") then
             write (2031,102) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hls_m(j)
             if (pco%csvout == "y") then 
                 write (2035,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hls_m(j)
             end if
           end if
           
           if (pco%pw_hru%m == "y") then
             hpw_m(j)%nplnt = pl_mass(j)%tot_com%n
             hpw_m(j)%pplnt = pl_mass(j)%tot_com%p
             write (2041,101) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hpw_m(j)
               if (pco%csvout == "y") then 
                 write (2045,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hpw_m(j)
               end if 
           end if
          
          sw_init = hwb_m(j)%sw_final
          sno_init = hwb_m(j)%sno_final
          hwb_m(j) = hwbz
          hwb_m(j)%sw_init = sw_init
          hwb_m(j)%sno_init = sno_init
          hnb_m(j) = hnbz
          hpw_m(j) = hpwz
          hls_m(j) = hlsz
        end if
        
        !! check end of year
        if (time%end_yr == 1) then
          hwb_a(j) = hwb_a(j) + hwb_y(j)
          hnb_a(j) = hnb_a(j) + hnb_y(j)
          hls_a(j) = hls_a(j) + hls_y(j)
          hpw_a(j) = hpw_a(j) + hpw_y(j)         
          
          const = time%day_end_yr
          hwb_y(j) = hwb_y(j) // const
          hpw_y(j) = hpw_y(j) // const
          
          !! yearly print
          hwb_y(j)%sw_final = hwb_d(j)%sw_final
          hwb_y(j)%sno_final = hwb_d(j)%sno_final
           !! if > 10mm irrigation, flag as irrigated for soft cal
           if (hwb_a(j)%irr > 10.) then
             hru(j)%irr = 1
           end if
          if (pco%wb_hru%y == "y") then
             write (2002,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hwb_y(j)
               if (pco%csvout == "y") then
                 write (2006,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hwb_y(j)
               end if
          end if
          
           if (pco%nb_hru%y == "y") then
             write (2022,104) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hnb_y(j)
             if (pco%csvout == "y") then
                 write (2026,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hnb_y(j)
                 end if
           end if
           
           if (pco%ls_hru%y == "y") then
             write (2032,102) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hls_y(j)
             if (pco%csvout == "y") then
                 write (2036,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hls_y(j)
             end if
           end if
           
           if (pco%pw_hru%y == "y") then
             hpw_y(j)%nplnt = pl_mass(j)%tot_com%n
             hpw_y(j)%pplnt = pl_mass(j)%tot_com%p
             write (2042,101) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hpw_y(j)
               if (pco%csvout == "y") then 
                 write (2046,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hpw_y(j)
               end if 
           end if
           
          !reset yearly parameters in time_control - for calibration runs
        end if
        
!!!!! average annual print
         if (time%end_sim == 1) then
           sw_init = hwb_a(j)%sw_init
           sno_init = hwb_a(j)%sno_init
           hwb_a(j) = hwb_a(j) / time%yrs_prt
           hwb_a(j) = hwb_a(j) // time%days_prt
           hwb_a(j)%sw_init = sw_init
           hwb_a(j)%sw_final = hwb_d(j)%sw_final
           hwb_a(j)%sno_init = sno_init
           hwb_a(j)%sno_final = hwb_d(j)%sno_final
           if (pco%wb_hru%a == "y") then
             write (2003,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hwb_a(j)
             if (pco%csvout == "y") then
               write (2007,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hwb_a(j)
             end if
           end if
           sw_init = hwb_d(j)%sw_final
           sno_init = hwb_d(j)%sno_final
           hru(j)%precip_aa = hwb_a(j)%precip
           hru(j)%flow(1) = hwb_a(j)%wateryld
           hru(j)%flow(2) = hwb_a(j)%perc
           hru(j)%flow(3) = hwb_a(j)%surq_gen
           hru(j)%flow(4) = hwb_a(j)%latq
           hru(j)%flow(5) = hwb_a(j)%qtile
           hwb_a(j) = hwbz
           hwb_a(j)%sw_init = sw_init
           hwb_a(j)%sno_init = sno_init
         end if
        
         if (time%end_sim == 1 .and. pco%nb_hru%a == "y") then 
           hnb_a(j) = hnb_a(j) / time%yrs_prt
           write (2023,104) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hnb_a(j)
           if (pco%csvout == "y") then 
               write (2027,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hnb_a(j)
               end if
             hnb_a(j) = hnbz
         end if
        
         if (time%end_sim == 1 .and. pco%ls_hru%a == "y") then
           hls_a(j) = hls_a(j) / time%yrs_prt 
           write (2033,101) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hls_a(j)
             if (pco%csvout == "y") then 
               write (2037,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hls_a(j)
             end if
             hls_a(j) = hlsz
         end if
        
         if (time%end_sim == 1 .and. pco%pw_hru%a == "y") then     
           hpw_a(j) = hpw_a(j) / time%yrs_prt
           hpw_a(j) = hpw_a(j) // time%days_prt
           hpw_a(j)%nplnt = pl_mass(j)%tot_com%n
           hpw_a(j)%pplnt = pl_mass(j)%tot_com%p
           write (2043,102) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hpw_a(j)
             if (pco%csvout == "y") then 
               write (2047,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hpw_a(j)
             end if
             hru(j)%strsa = hpw_a(j)%strsa
             hpw_a(j) = hpwz
         end if
         
         !! write yearly crop yields
         if (time%end_yr == 1) then
           if (pco%crop_yld == "y" .or. pco%crop_yld == "b") then
           do ipl = 1, pcom(j)%npl
             if (pcom(j)%plcur(ipl)%harv_num_yr > 0) then 
               pl_mass(j)%yield_yr(ipl) = pl_mass(j)%yield_yr(ipl) / float(pcom(j)%plcur(ipl)%harv_num_yr)
             endif
           if (pco%crop_yld == "y" .or. pco%crop_yld == "b") then
            write (4010,103) time%day, time%mo, time%day_mo, time%yrc, j, pcom(j)%pl(ipl), pl_mass(j)%yield_yr(ipl)
           end if
            if (pco%csvout == "y") then
              write (4011,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, pcom(j)%pl(ipl), pl_mass(j)%yield_yr(ipl) 
            end if
           end do
           end if
         end if

         !! write average annual crop yields
         if (time%end_sim == 1) then
           if (pco%crop_yld == "a" .or. pco%crop_yld == "b") then
           do ipl = 1, pcom(j)%npl
             idp = pcom(j)%plcur(ipl)%idplt
             if (pcom(j)%plcur(ipl)%harv_num > 0) then 
               pl_mass(j)%yield_tot(ipl) = pl_mass(j)%yield_tot(ipl) / float(pcom(j)%plcur(ipl)%harv_num)
             endif
            write (4008,103) time%day, time%mo, time%day_mo, time%yrc, j,pldb(idp)%plantnm, pl_mass(j)%yield_tot(ipl)
            if (pco%csvout == "y") then
              write (4009,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j,pldb(idp)%plantnm, pl_mass(j)%yield_tot(ipl) 
            end if
           end do
           end if
         end if
      return
      
100   format (4i6,2i8,2x,a,42f12.3)
101   format (4i6,2i8,2x,a,25f12.3)    !!!!!!!!!!   nbs chg
102   format (4i6,2i8,2x,a,25f12.3)    !!!!!!!!!!   nbs chg
103   format (4i6,i8,4x,a,5x,4f12.3)
104   format (4i6,2i8,2x,a8,4f12.3,23f17.3)
105   format (4i6,2i8,2x,a8,8f17.3)
106   format (4i6,2i8,2x,a8,29f17.3)
       
      end subroutine hru_output