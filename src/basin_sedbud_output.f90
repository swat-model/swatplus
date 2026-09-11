      subroutine basin_sedbud_output

      use time_module
      use basin_module
      
      implicit none

        !! sum monthly variables
        bsn_sedbudm = bsn_sedbudm + bsn_sedbud
        
        !! daily print
        if (pco%sd_chan%d == "y") then
          write (3152,*) time%day, time%mo, time%day_mo, time%yrc, bsn_sedbud

          if (pco%csvout == "y") then
          write (3156,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, bsn_sedbud
          end if
        end if
       
        !! zero daily
        bsn_sedbud = bsn_sedbudz

        !! monthly print
        if (time%end_mo == 1) then
          !! add into the yearly total
          bsn_sedbudy = bsn_sedbudy + bsn_sedbudm

          if (pco%sd_chan%m == "y") then
          write (3153,*) time%mo, time%day_mo, time%yrc, bsn_sedbudm
 
          if (pco%csvout == "y") then
          write (3157,'(*(G0.6,:","))') time%mo, time%day_mo, time%yrc, bsn_sedbudm
          end if
          end if

          !! zero monthly
          bsn_sedbudm = bsn_sedbudz

        end if

      !! yearly print
      if (time%end_yr == 1) then
        !! add into the yearly total
        bsn_sedbudy =  bsn_sedbudy + bsn_sedbudm
          
        if (pco%sd_chan%y == "y") then
          write (3154,*) time%mo, time%day_mo, time%yrc, bsn_sedbudy
  
              if (pco%csvout == "y") then
          write (3158,'(*(G0.6,:","))') time%mo, time%day_mo, time%yrc, bsn_sedbudy
          end if
        end if

        !! accumulate the year's total into the average-annual accumulator
        !! (fix: bsn_sedbuda was divided by yrs_prt at end of sim but never summed -> AA printed 0)
        bsn_sedbuda = bsn_sedbuda + bsn_sedbudy

        !! zero yearly
        bsn_sedbudy = bsn_sedbudz

      end if

      !! average annual print
      if (time%end_sim == 1) then
        !! convert the accumulated total to an average annual value
        bsn_sedbuda = bsn_sedbuda / time%yrs_prt

        if (pco%sd_chan%a == "y") then
        write (3155,*) time%mo, time%day_mo, time%yrc, bsn_sedbuda

        if (pco%csvout == "y") then
        write (3159,'(*(G0.6,:","))') time%mo, time%day_mo, time%yrc, bsn_sedbuda
        end if
       end if
      end if

      return
      
      end subroutine basin_sedbud_output

