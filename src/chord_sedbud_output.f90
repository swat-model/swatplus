      subroutine chord_sedbud_output

      use time_module
      use sd_channel_module
      use basin_module
      
      implicit none
      
      integer :: iord

      !! loop through and print each use object
      do iord = 1, 12
        
        !! sum monthly variables
        ch_morph_ordm(iord) = ch_morph_ordm(iord) + ch_morph_ord(iord)
        
        !! daily print
        if (pco%sd_chan%d == "y") then
            write (3161,*) time%day, time%mo, time%day_mo, time%yrc, iord, ch_morph_ord(iord)

          if (pco%csvout == "y") then
          write (3165,'(*(G0.6,:","))') time%mo, time%day_mo, time%yrc, iord, ch_morph_ord(iord)
          end if
        end if
       
        !! zero daily
        ch_morph_ord(iord) = ch_morphz

        !! monthly print
        if (time%end_mo == 1) then
          !! sum amount of yearly used water
          ch_morph_ordy(iord) = ch_morph_ordy(iord) + ch_morph_ordm(iord)   

          if (pco%sd_chan%m == "y") then
          write (3162,*) time%mo, time%day_mo, time%yrc, iord, ch_morph_ordm(iord)
 
          if (pco%csvout == "y") then
          write (3166,'(*(G0.6,:","))') time%mo, time%day_mo, time%yrc, iord, ch_morph_ordm(iord)
          end if
          end if

          !! zero monthly
          ch_morph_ordm(iord) = ch_morphz

        end if

      !! yearly print
      if (time%end_yr == 1) then
        !! sum amount of yearly used water
        ch_morph_ordy(iord) =  ch_morph_ordy(iord) + ch_morph_ordm(iord)
          
        if (pco%sd_chan%y == "y") then
          write (3163,*) time%mo, time%day_mo, time%yrc, iord, ch_morph_ordy(iord)   
  
              if (pco%csvout == "y") then
          write (3167,'(*(G0.6,:","))') time%mo, time%day_mo, time%yrc, iord, ch_morph_ordy(iord)
          end if
        end if

        !! accumulate the year's total into the average-annual accumulator
        !! (fix: ch_morph_orda was divided by yrs_prt at end of sim but never summed -> AA printed 0)
        ch_morph_orda(iord) = ch_morph_orda(iord) + ch_morph_ordy(iord)

        !! zero yearly
        ch_morph_ordy(iord) = ch_morphz

      end if

      !! average annual print
      if (time%end_sim == 1) then
        !! sum amount of average annual used water
        ch_morph_orda(iord) = ch_morph_orda(iord) / time%yrs_prt

        if (pco%sd_chan%a == "y") then
        write (3164,*) time%mo, time%day_mo, time%yrc, iord, ch_morph_orda(iord) 

        if (pco%csvout == "y") then
        write (3168,'(*(G0.6,:","))') time%mo, time%day_mo, time%yrc, iord, ch_morph_orda(iord)
        end if
       end if
      end if

      end do    ! do iord = 1, 12
      
      return
      
100   format (4i6,i8,5x,a,5x,i8,5x,i8,5x,a,5x,i8,20(7x,a,5x,i8,3f15.1))
      end subroutine chord_sedbud_output

