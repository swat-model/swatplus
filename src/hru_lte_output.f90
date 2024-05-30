      subroutine hru_lte_output (isd)

      use time_module
      use basin_module
      use output_landscape_module
      use hydrograph_module, only : sp_ob1, ob

      implicit none

      integer, intent (in) :: isd
      integer :: iob
      real :: const
      
      iob = sp_ob1%hru_lte + isd - 1
              
        hltwb_m(isd) = hltwb_m(isd) + hltwb_d(isd)
        hltnb_m(isd) = hltnb_m(isd) + hltnb_d(isd)
        hltls_m(isd) = hltls_m(isd) + hltls_d(isd) 
        hltpw_m(isd) = hltpw_m(isd) + hltpw_d(isd)

        !! daily print
         if (pco%day_print == "y" .and. pco%int_day_cur == pco%int_day) then
          if (pco%wb_sd%d == "y") then
            write (2300,100) time%day, time%mo, time%day_mo, time%yrc, isd, ob(iob)%gis_id, ob(iob)%name, hltwb_d(isd)  !! waterbal
              if (pco%csvout == "y") then 
                write (2304,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, isd, ob(iob)%gis_id, ob(iob)%name, &
                    hltwb_d(isd)  !! waterbal
              end if 
          end if
!          if (pco%nb_sd%d == "y") then
!            write (2420,100) time%day, time%yrc, isd, ob(iob)%gis_id, ob(iob)%name, hltnb_d(isd)  !! nutrient bal
!             if (pco%csvout == "y") then 
!               write (2424,'(*(G0.3,:","))') time%day, time%yrc, isd, ob(iob)%gis_id, ob(iob)%name, hltnb_d(isd)  !! nutrient bal
!             end if 
!          end if
          if (pco%ls_sd%d == "y") then
            write (2440,101) time%day, time%mo, time%day_mo, time%yrc, isd, ob(iob)%gis_id, ob(iob)%name, hltls_d(isd)  !! losses
              if (pco%csvout == "y") then 
                write (2444,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, isd, ob(iob)%gis_id, ob(iob)%name, &
                    hltls_d(isd)  !! losses
              endif 
          end if
          if (pco%pw_sd%d == "y") then
            write (2460,101) time%day, time%mo, time%day_mo, time%yrc, isd, ob(iob)%gis_id, ob(iob)%name, hltpw_d(isd)  !! plant weather 
              if (pco%csvout == "y") then 
                write (2464,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, isd, ob(iob)%gis_id, ob(iob)%name, &
                    hltpw_d(isd)  !! plant weather 
              end if 
          end if
        end if

        !! check end of month
        if (time%end_mo == 1) then
          hltwb_y(isd) = hltwb_y(isd) + hltwb_m(isd)
          hltnb_y(isd) = hltnb_y(isd) + hltnb_m(isd)
          hltls_y(isd) = hltls_y(isd) + hltls_m(isd)
          hltpw_y(isd) = hltpw_y(isd) + hltpw_m(isd)
          
          const = float (ndays(time%mo + 1) - ndays(time%mo))
          hltpw_m(isd) = hltpw_m(isd) // const
          hltwb_m(isd) = hltwb_m(isd) // const
          
          !! monthly print
           if (pco%wb_sd%m == "y") then
             write (2301,100) time%day, time%mo, time%day_mo, time%yrc, isd, ob(iob)%gis_id, ob(iob)%name, hltwb_m(isd)
               if (pco%csvout == "y") then 
                 write (2305,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, isd, ob(iob)%gis_id, ob(iob)%name, &
                    hltwb_m(isd)
               end if 
           end if
!           if (pco%nb_sd%m == "y") then
!             write (2421,100) time%mo, time%yrc, isd, ob(iob)%gis_id, ob(iob)%name, hltnb_m(isd)
!             if (pco%csvout == "y") then 
!               write (2425,'(*(G0.3,:","))') time%mo, time%yrc, isd, ob(iob)%gis_id, ob(iob)%name, hltnb_m(isd)
!             end if 
!           end if
           if (pco%ls_sd%m == "y") then
             write (2441,101) time%day, time%mo, time%day_mo, time%yrc, isd, ob(iob)%gis_id, ob(iob)%name, hltls_m(isd)
               if (pco%csvout == "y") then 
                 write (2445,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, isd, ob(iob)%gis_id, ob(iob)%name, &
                    hltls_m(isd)
               end if 
           end if
           if (pco%pw_sd%m == "y") then
             write (2461,101) time%day, time%mo, time%day_mo, time%yrc, isd, ob(iob)%gis_id, ob(iob)%name, hltpw_m(isd)
               if (pco%csvout == "y") then 
                 write (2465,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, isd, ob(iob)%gis_id, ob(iob)%name, &
                    hltpw_m(isd)
               end if 
           end if
          
          hltwb_m(isd) = hwbz
          hltnb_m(isd) = hnbz
          hltpw_m(isd) = hpwz
          hltls_m(isd) = hlsz
        end if
        
        !! check end of year
        if (time%end_yr == 1) then
          hltwb_a(isd) = hltwb_a(isd) + hltwb_y(isd)
          hltnb_a(isd) = hltnb_a(isd) + hltnb_y(isd)
          hltls_a(isd) = hltls_a(isd) + hltls_y(isd)
          hltpw_a(isd) = hltpw_a(isd) + hltpw_y(isd)
          
          const = time%day_end_yr
          hltwb_y(isd) = hltwb_y(isd) // const
          hltpw_y(isd) = hltpw_y(isd) // const

          !! yearly print
           if (time%end_yr == 1 .and. pco%wb_sd%y == "y") then
             write (2302,100) time%day, time%mo, time%day_mo, time%yrc, isd, ob(iob)%gis_id, ob(iob)%name, hltwb_y(isd)
                if (pco%csvout == "y") then 
                  write (2306,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, isd, ob(iob)%gis_id, ob(iob)%name, &
                    hltwb_y(isd)
                end if 
           end if
!           if (time%end_yr == 1 .and. pco%nb_sd%y == "y") then
!             write (2422,100) time%end_yr, time%yrc, isd, ob(iob)%gis_id, ob(iob)%name, hltnb_y(isd)
!             if (pco%csvout == "y") then 
!               write (2426,'(*(G0.3,:","))') time%end_yr, time%yrc, isd, ob(iob)%gis_id, ob(iob)%name, hltnb_y(isd)
!             end if 
!           end if
           if (time%end_yr == 1 .and. pco%ls_sd%y == "y") then
             write (2442,101) time%day, time%mo, time%day_mo, time%yrc, isd, ob(iob)%gis_id, ob(iob)%name, hltls_y(isd)
               if (pco%csvout == "y") then 
                 write (2446,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, isd, ob(iob)%gis_id, ob(iob)%name, &
                    hltls_y(isd)
               end if 
           end if
           if (time%end_yr == 1 .and. pco%pw_sd%y == "y") then
             write (2462,101) time%day, time%mo, time%day_mo, time%yrc, isd, ob(iob)%gis_id, ob(iob)%name, hltpw_y(isd)
              if (pco%csvout == "y") then 
                write (2466,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, isd, ob(iob)%gis_id, ob(iob)%name, &
                    hltpw_y(isd)
              end if 
           end if

        end if
        
!!!!! average annual print
         if (time%end_sim == 1 .and. pco%wb_sd%a == "y") then
           hltwb_a(isd) = hltwb_a(isd) / time%yrs_prt
           hltwb_a(isd) = hltwb_a(isd) // time%days_prt          
           write (2303,100) time%day, time%mo, time%day_mo, time%yrc, isd, ob(iob)%gis_id, ob(iob)%name, hltwb_a(isd)
           if (pco%csvout == "y") then 
             write (2307,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, isd, ob(iob)%gis_id, ob(iob)%name, &
                hltwb_a(isd)
           end if
           hltwb_a(isd) = hwbz
         end if
        
!         if (time%end_sim == 1 .and. pco%nb_sd%a == "y") then 
!           hltnb_a(isd) = hltnb_a(isd) / time%yrs_prt
!           write (2423,100) time%end_yr, time%yrs, isd, ob(iob)%gis_id, ob(iob)%name, hltnb_a(isd)
!         if (pco%csvout == "y") then 
!             write (2427,'(*(G0.3,:","))') time%end_yr, time%yrs, isd, ob(iob)%gis_id, ob(iob)%name, hltnb_a(isd)
!           end if
!         end if
!         hltnb_a(isd) = hnbz       
         
         if (time%end_sim == 1 .and. pco%ls_sd%a == "y") then
           hltls_a(isd) = hltls_a(isd) / time%yrs_prt  
           write (2443,101) time%day, time%mo, time%day_mo, time%yrc, isd, ob(iob)%gis_id, ob(iob)%name, hltls_a(isd)
           if (pco%csvout == "y") then 
             write (2447,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, isd, ob(iob)%gis_id, ob(iob)%name, &
                hltls_a(isd)
           end if
         end if
         hltls_a(isd) = hlsz
        
         if (time%end_sim == 1 .and. pco%pw_sd%a == "y") then   
           hltpw_a(isd) = hltpw_a(isd) / time%yrs_prt 
           hltpw_a(isd) = hltpw_a(isd) // time%days_prt
           write (2463,101) time%day, time%mo, time%day_mo, time%yrc, isd, ob(iob)%gis_id, ob(iob)%name, hltpw_a(isd)
           if (pco%csvout == "y") then 
             write (2467,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, isd, ob(iob)%gis_id, ob(iob)%name, &
                hltpw_a(isd)
           end if
           hltpw_a(isd) = hpwz
         end if

      return
     
100   format (4i6,2i8,2x,a,42f12.3)
101   format (1x,4i6,i7,i8,2x,a,31f12.3)
 
      end subroutine hru_lte_output