      subroutine hru_output(ihru)

      use plant_module
      use plant_data_module
      use time_module
      use basin_module
      use output_landscape_module
      use hydrograph_module, only : sp_ob1, ob
      use organic_mineral_mass_module
      use soil_module
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

      !! daily print
         if (pco%day_print == "y" .and. pco%int_day_cur == pco%int_day) then
          if (pco%wb_hru%d == "y") then
             hwb_d(j)%sw_final = soil(j)%sw
             hwb_d(j)%sw = (hwb_d(j)%sw_init + hwb_d(j)%sw_final) / 2.
             hwb_d(j)%sno_final = hru(j)%sno_mm
             hwb_d(j)%snopack = (hwb_d(j)%sno_init + hwb_d(j)%sno_final) / 2.
             write (2000,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hwb_d(j)   !! waterbal
             if (pco%csvout == "y") then
               !! changed write unit below (2004 to write file data)
               write (2004,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hwb_d(j)  !! waterbal
             end if
             hwb_d(j)%sw_init = hwb_d(j)%sw_final
             hwb_d(j)%sno_init = hwb_d(j)%sno_final
          end if
          
          if (pco%nb_hru%d == "y") then
            write (2020,104) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hnb_d(j)  !! nutrient bal
            write (3333,105) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hcyl_d(j) !! new nutrient carb file
            if (pco%csvout == "y") then
                write (2024,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hnb_d(j)  !! nutrient bal
                write (3334,105) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hcyl_d(j) !! new nutrient carb file
            end if
          end if
          if (pco%ls_hru%d == "y") then
            write (2030,102) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hls_d(j)  !! losses
            write (3341,106) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hgl_d(j) !! new nutrient carb file
            if (pco%csvout == "y") then
                write (2034,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hls_d(j)  !! losses
                write (3342,106) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hgl_d(j) !! new nutrient carb file
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
           if (pco%wb_hru%m == "y") then
             hwb_m(j)%sw_final = hwb_d(j)%sw_final
             hwb_m(j)%sno_final = hwb_d(j)%sno_final
             write (2001,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hwb_m(j)
               if (pco%csvout == "y") then
                 write (2005,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hwb_m(j)
               end if
           end if
           if (pco%nb_hru%m == "y") then
             write (2021,104) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hnb_m(j)
             write (3335,106) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hcyl_m(j) !! new nutrient carb file
             if (pco%csvout == "y") then
                 write (2025,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hnb_m(j)
                 write (3336,106) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hcyl_m(j) !! new nutrient carb file               end if
             end if
           end if
           if (pco%ls_hru%m == "y") then
             write (2031,102) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hls_m(j)
             write (3343,106) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hgl_m(j) !! new nutrient carb file
             if (pco%csvout == "y") then 
                 write (2035,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hls_m(j)
                 write (3344,106) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hgl_d(j) !! new nutrient carb file
             end if
           end if
           if (pco%pw_hru%m == "y") then
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
           if (time%end_yr == 1 .and. pco%wb_hru%y == "y") then
             hwb_y(j)%sw_final = hwb_d(j)%sw_final
             hwb_y(j)%sno_final = hwb_d(j)%sno_final
             write (2002,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hwb_y(j)
               if (pco%csvout == "y") then
                 write (2006,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hwb_y(j)
               end if
           end if
           if (time%end_yr == 1 .and. pco%nb_hru%y == "y") then
             write (2022,104) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hnb_y(j)
             write (3337,106) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hcyl_y(j) !! new nutrient carb file               end if
             if (pco%csvout == "y") then
                 write (2026,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hnb_y(j)
                 write (3338,106) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hcyl_y(j) !! new nutrient carb file               end if
             end if
           end if
           if (time%end_yr == 1 .and. pco%ls_hru%y == "y") then
             write (2032,102) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hls_y(j)
             write (3345,106) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hgl_y(j) !! new nutrient carb file
             if (pco%csvout == "y") then
                 write (2036,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hls_y(j)
                 write (3346,106) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hgl_y(j) !! new nutrient carb file
             end if
           end if
           if (time%end_yr == 1 .and. pco%pw_hru%y == "y") then
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
           hru(j)%precip_aa = hwb_a(j)%precip
           hwb_a(j)%sw_init = sw_init
           hwb_a(j)%sw_final = hwb_d(j)%sw_final
           hwb_a(j)%sno_init = sno_init
           hwb_a(j)%sno_final = hwb_d(j)%sno_final
           if (pco%wb_hru%a == "y") then
             write (2003,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hwb_a(j)
             if (pco%csvout == "y") then
               write (2007,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hwb_a(j)
             end if
           end if
           sw_init = hwb_d(j)%sw_final
           sno_init = hwb_d(j)%sno_final
           hwb_a(j) = hwbz
           hwb_a(j)%sw_init = sw_init
           hwb_a(j)%sno_init = sno_init
         end if
        
         if (time%end_sim == 1 .and. pco%nb_hru%a == "y") then 
           hnb_a(j) = hnb_a(j) / time%yrs_prt
           write (2023,104) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hnb_a(j)
           write (3339,106) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hcyl_a(j) !! new nutrient carb file               end if
             if (pco%csvout == "y") then 
               write (2027,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hnb_a(j)
               write (3340,106) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hcyl_a(j) !! new nutrient carb file               end if
             end if
             hnb_a(j) = hnbz
         end if
        
         if (time%end_sim == 1 .and. pco%ls_hru%a == "y") then
           hls_a(j) = hls_a(j) / time%yrs_prt 
           write (2033,101) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hls_a(j)
           write (3347,106) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hgl_a(j) !! new nutrient carb file               end if
             if (pco%csvout == "y") then 
               write (2037,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hls_a(j)
               write (3348,106) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hgl_a(j) !! new nutrient carb file               end if
             end if
             hls_a(j) = hlsz
         end if
        
         if (time%end_sim == 1 .and. pco%pw_hru%a == "y") then     
           hpw_a(j) = hpw_a(j) / time%yrs_prt
           hpw_a(j) = hpw_a(j) // time%days_prt
           write (2043,102) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hpw_a(j)
             if (pco%csvout == "y") then 
               write (2047,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hpw_a(j)
             end if
             hru(j)%strsa = hpw_a(j)%strsa
             hpw_a(j) = hpwz
         end if

         if (time%end_sim == 1) then
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
      return
      
100   format (4i6,2i8,2x,a,39f12.3)
101   format (4i6,2i8,2x,a,24f12.3)
102   format (4i6,2i8,2x,a,24f12.3)
103   format (4i6,i8,4x,a,5x,4f12.3)
104   format (4i6,2i8,2x,a8,4f12.3,23f17.3)
105   format (4i6,2i8,2x,a8,8f17.3)
106   format (4i6,2i8,2x,a8,29f17.3)
       
      end subroutine hru_output