      subroutine ch_sedbud_output

      use time_module
      use hydrograph_module
      use sd_channel_module
      
      implicit none
      
      integer :: ichan
      integer :: iob

      !! loop through and print each use object
      do ichan = 1, sp_ob%chandeg
        iob = sp_ob1%chandeg + ichan - 1
        
        !! sum monthly variables
        ch_morphm(ichan) = ch_morphm(ichan) + ch_morph(ichan)
        
        !! daily print
        if (pco%water_allo%d == "y") then
          write (3118,*) time%day, time%mo, time%day_mo, time%yrc, ichan, ob(iob)%gis_id, ob(iob)%name, ch_morph(ichan)

          if (pco%csvout == "y") then
          write (3122,'(*(G0.6,:","))') time%mo, time%day_mo, time%yrc, ichan, ob(iob)%gis_id, ob(iob)%name, ch_morph(ichan)
          end if
        end if
       
        !! zero daily
        ch_morph(ichan) = ch_morphz

        !! monthly print
        if (time%end_mo == 1) then
          !! sum amount of yearly used water
          ch_morphy(ichan) = ch_morphy(ichan) + ch_morphm(ichan)

          if (pco%water_allo%m == "y") then
          write (3119,*) time%mo, time%day_mo, time%yrc, ichan, ob(iob)%gis_id, ob(iob)%name, ch_morphm(ichan)
 
          if (pco%csvout == "y") then
          write (3123,'(*(G0.6,:","))') time%mo, time%day_mo, time%yrc, ichan, ob(iob)%gis_id, ob(iob)%name, ch_morphm(ichan)
          end if
          end if

          !! zero monthly
          ch_morphm(ichan) = ch_morphz

        end if

      !! yearly print
      if (time%end_yr == 1) then
        !! sum amount of yearly used water
        ch_morpha(ichan) =  ch_morpha(ichan) + ch_morphm(ichan)
          
        if (pco%water_allo%y == "y") then
          write (3120,*) time%mo, time%day_mo, time%yrc, ichan, ob(iob)%gis_id, ob(iob)%name, ch_morpha(ichan)
  
              if (pco%csvout == "y") then
          write (3124,'(*(G0.6,:","))') time%mo, time%day_mo, time%yrc, ichan, ob(iob)%gis_id, ob(iob)%name, ch_morpha(ichan)
          end if
        end if

        !! zero yearly
        ch_morpha(ichan) = ch_morphz

      end if

      !! average annual print
      if (time%end_sim == 1) then
        !! sum amount of average annual used water
        ch_morpha(ichan) = ch_morpha(ichan) / time%yrs_prt

        if (pco%water_allo%a == "y") then
        write (3121,*) time%mo, time%day_mo, time%yrc, ichan, ob(iob)%gis_id, ch_morpha(ichan)

        if (pco%csvout == "y") then
        write (3125,'(*(G0.6,:","))') time%mo, time%day_mo, time%yrc, ichan, ob(iob)%gis_id, ch_morpha(ichan)
        end if
       end if
      end if

      end do    ! do ichan = 1, sp_ob%chandeg
      
      return
      
100   format (4i6,i8,5x,a,5x,i8,5x,i8,5x,a,5x,i8,20(7x,a,5x,i8,3f15.1))
      end subroutine ch_sedbud_output