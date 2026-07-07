      subroutine sd_channel_output (ichan)
    
      use sd_channel_module
      use basin_module
      use time_module
      use hydrograph_module
      use water_body_module
      
      implicit none
      
      integer, intent (in) :: ichan         !             |
      integer :: iob                        !             |
      integer :: ii                         !             
      real :: const                         !             |

      iob = sp_ob1%chandeg + ichan - 1

      !ch_stor_m(ichan) = ch_stor_m(ichan) + ch_stor(ichan)
      ch_in_m(ichan) = ch_in_m(ichan) + ch_in_d(ichan)
      ch_out_m(ichan) = ch_out_m(ichan) + ch_out_d(ichan)
      ch_wat_m(ichan) = ch_wat_m(ichan) + ch_wat_d(ichan)
      
!!!!! subdaily print
       if (pco%day_print == "y" .and. time%step > 0 .and. pco%int_day_cur == pco%int_day) then
         do ii = 1, time%step 
           write (2508,"(7i6,10(1x,e15.4))") iob, ob(iob)%gis_id, time%yrc, time%mo, time%day_mo, time%day, &
                    ii, ob(iob)%hyd_flo(1,ii)         
         end do
      end if
      
!!!!! daily print
       if (pco%day_print == "y" .and. pco%int_day_cur == pco%int_day) then
        if (pco%sd_chan%d == "y") then
          write (2500,100) time%day, time%mo, time%day_mo, time%yrc, ichan, ob(iob)%gis_id, ob(iob)%name, ch_wat_d(ichan), &
            ch_stor(ichan), ch_in_d(ichan), ch_out_d(ichan), wtemp
!! new sd_channel output file
          !write (2509,100)  time%day, time%mo, time%day_mo, time%yrc, ichan, ob(iob)%gis_id, ob(iob)%name,    &
          !  ch_in_d(ichan)%flo, ch_out_d(ichan)%flo, ch_in_d(ichan)%sed, ch_out_d(ichan)%sed,                 &
          !  ch_in_d(ichan)%orgn, ch_out_d(ichan)%orgn, ch_in_d(ichan)%sedp, ch_out_d(ichan)%sedp,             &
          !  ch_in_d(ichan)%no3, ch_out_d(ichan)%no3, ch_in_d(ichan)%solp, ch_out_d(ichan)%solp,               &
          !  ch_in_d(ichan)%chla, ch_out_d(ichan)%chla, ch_in_d(ichan)%nh3, ch_out_d(ichan)%nh3,               &
          !  ch_in_d(ichan)%no2, ch_out_d(ichan)%no2, ch_in_d(ichan)%cbod, ch_out_d(ichan)%cbod,               &
          !  ch_in_d(ichan)%dox, ch_out_d(ichan)%dox
           if (pco%csvout == "y") then
             write (2504,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, ichan, ob(iob)%gis_id, ob(iob)%name,  &
            ch_wat_d(ichan), ch_stor(ichan), ch_in_d(ichan), ch_out_d(ichan), wtemp
            
            ! write (2510,'(*(G0.3,:","))')  time%day, time%mo, time%day_mo, time%yrc, ichan, ob(iob)%gis_id, ob(iob)%name,  &
            ! ch_in_d(ichan)%flo, ch_out_d(ichan)%flo, ch_in_d(ichan)%sed, ch_out_d(ichan)%sed,                 &
            ! ch_in_d(ichan)%orgn, ch_out_d(ichan)%orgn, ch_in_d(ichan)%sedp, ch_out_d(ichan)%sedp,             &
            ! ch_in_d(ichan)%no3, ch_out_d(ichan)%no3, ch_in_d(ichan)%solp, ch_out_d(ichan)%solp,               &
            ! ch_in_d(ichan)%chla, ch_out_d(ichan)%chla, ch_in_d(ichan)%nh3, ch_out_d(ichan)%nh3,               &
            ! ch_in_d(ichan)%no2, ch_out_d(ichan)%no2, ch_in_d(ichan)%cbod, ch_out_d(ichan)%cbod,               &
            ! ch_in_d(ichan)%dox, ch_out_d(ichan)%dox
            
           end if
        end if
      end if

!!!!! monthly print
        if (time%end_mo == 1) then
          !ch_stor_y(ichan) = ch_stor_y(ichan) + ch_stor_m(ichan)
          const = float (ndays(time%mo + 1) - ndays(time%mo))
          ch_in_y(ichan) = ch_in_y(ichan) + ch_in_m(ichan)
          ch_out_y(ichan) = ch_out_y(ichan) + ch_out_m(ichan)
          ch_wat_y(ichan) = ch_wat_y(ichan) + ch_wat_m(ichan)
          ch_in_m(ichan)%flo = ch_in_m(ichan)%flo / const
          ch_out_m(ichan)%flo = ch_out_m(ichan)%flo / const
          
          !ch_stor_m(ichan) = ch_stor_m(ichan) / const           !! all storage variables are averages
          ch_wat_m(ichan) = ch_wat_m(ichan) // const            !! // only divides area (daily average values)
          
          if (pco%sd_chan%m == "y") then
          write (2501,100) time%day, time%mo, time%day_mo, time%yrc, ichan, ob(iob)%gis_id, ob(iob)%name, ch_wat_m(ichan), &
            ch_stor(ichan), ch_in_m(ichan), ch_out_m(ichan), wtemp
        !! new sd_channel output file
          !write (3511,100)  time%day, time%mo, time%day_mo, time%yrc, ichan, ob(iob)%gis_id, ob(iob)%name,    &
          !  ch_in_m(ichan)%flo, ch_out_m(ichan)%flo, ch_in_m(ichan)%sed, ch_out_m(ichan)%sed,                 &
          !  ch_in_m(ichan)%orgn, ch_out_m(ichan)%orgn, ch_in_m(ichan)%sedp, ch_out_m(ichan)%sedp,             &
          !  ch_in_m(ichan)%no3, ch_out_m(ichan)%no3, ch_in_m(ichan)%solp, ch_out_m(ichan)%solp,               &
          !  ch_in_m(ichan)%chla, ch_out_m(ichan)%chla, ch_in_m(ichan)%nh3, ch_out_m(ichan)%nh3,               &
          !  ch_in_m(ichan)%no2, ch_out_m(ichan)%no2, ch_in_m(ichan)%cbod, ch_out_m(ichan)%cbod,               &
          !  ch_in_m(ichan)%dox, ch_out_m(ichan)%dox

          if (pco%csvout == "y") then
            write (2505,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, ichan, ob(iob)%gis_id, ob(iob)%name,   &
            ch_wat_m(ichan), ch_stor(ichan), ch_in_m(ichan), ch_out_m(ichan), wtemp

            !write (3512,'(*(G0.3,:","))')  time%day, time%mo, time%day_mo, time%yrc, ichan, ob(iob)%gis_id, ob(iob)%name,  &
            ! ch_in_m(ichan)%flo, ch_out_m(ichan)%flo, ch_in_m(ichan)%sed, ch_out_m(ichan)%sed,                 &
            ! ch_in_m(ichan)%orgn, ch_out_m(ichan)%orgn, ch_in_m(ichan)%sedp, ch_out_m(ichan)%sedp,             &
            ! ch_in_m(ichan)%no3, ch_out_m(ichan)%no3, ch_in_m(ichan)%solp, ch_out_m(ichan)%solp,               &
            ! ch_in_m(ichan)%chla, ch_out_m(ichan)%chla, ch_in_m(ichan)%nh3, ch_out_m(ichan)%nh3,               &
            ! ch_in_m(ichan)%no2, ch_out_m(ichan)%no2, ch_in_m(ichan)%cbod, ch_out_m(ichan)%cbod,               &
            ! ch_in_m(ichan)%dox, ch_out_m(ichan)%dox
          end if
        end if
        !ch_stor_m(ichan) = chaz
        ch_in_m(ichan) = chaz
        ch_out_m(ichan) = chaz
        ch_wat_m(ichan) = wbodz
        end if

!!!!! yearly print
      if (time%end_yr == 1) then
        !ch_stor_a(ichan) = ch_stor_a(ichan) + ch_stor_y(ichan)
        const = time%day_end_yr
        ch_in_y(ichan)%flo = ch_in_y(ichan)%flo / const
        ch_out_y(ichan)%flo = ch_out_y(ichan)%flo / const
        ch_in_a(ichan) = ch_in_a(ichan) + ch_in_y(ichan)
        ch_out_a(ichan) = ch_out_a(ichan) + ch_out_y(ichan)
        ch_wat_a(ichan) = ch_wat_a(ichan) + ch_wat_y(ichan)
        
        !ch_stor_y(ichan) = ch_stor_y(ichan) / const     !! all storage variables are averages
        ch_wat_y(ichan) = ch_wat_y(ichan) // const      !! // only divides area (daily average values)
          
        if (pco%sd_chan%y == "y") then 
          write (2502,100) time%day, time%mo, time%day_mo, time%yrc, ichan, ob(iob)%gis_id, ob(iob)%name, ch_wat_y(ichan), &
            ch_stor(ichan), ch_in_y(ichan), ch_out_y(ichan), wtemp
        !! new sd_channel output file
          !write (3513,100)  time%day, time%mo, time%day_mo, time%yrc, ichan, ob(iob)%gis_id, ob(iob)%name,    &
          !  ch_in_y(ichan)%flo, ch_out_y(ichan)%flo, ch_in_y(ichan)%sed, ch_out_y(ichan)%sed,                 &
          !  ch_in_y(ichan)%orgn, ch_out_y(ichan)%orgn, ch_in_y(ichan)%sedp, ch_out_y(ichan)%sedp,             &
          !  ch_in_y(ichan)%no3, ch_out_y(ichan)%no3, ch_in_y(ichan)%solp, ch_out_y(ichan)%solp,               &
          !  ch_in_y(ichan)%chla, ch_out_y(ichan)%chla, ch_in_y(ichan)%nh3, ch_out_y(ichan)%nh3,               &
          !  ch_in_y(ichan)%no2, ch_out_y(ichan)%no2, ch_in_y(ichan)%cbod, ch_out_y(ichan)%cbod,               &
          !  ch_in_y(ichan)%dox, ch_out_y(ichan)%dox
          if (pco%csvout == "y") then
           write (2506,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, ichan, ob(iob)%gis_id, ob(iob)%name,    &
            ch_wat_y(ichan), ch_stor(ichan), ch_in_y(ichan), ch_out_y(ichan), wtemp
          !write (3514,'(*(G0.3,:","))')  time%day, time%mo, time%day_mo, time%yrc, ichan, ob(iob)%gis_id, ob(iob)%name,  &
          !   ch_in_y(ichan)%flo, ch_out_y(ichan)%flo, ch_in_y(ichan)%sed, ch_out_y(ichan)%sed,                 &
          !   ch_in_y(ichan)%orgn, ch_out_y(ichan)%orgn, ch_in_y(ichan)%sedp, ch_out_y(ichan)%sedp,             &
          !   ch_in_y(ichan)%no3, ch_out_y(ichan)%no3, ch_in_y(ichan)%solp, ch_out_y(ichan)%solp,               &
          !   ch_in_y(ichan)%chla, ch_out_y(ichan)%chla, ch_in_y(ichan)%nh3, ch_out_y(ichan)%nh3,               &
          !   ch_in_y(ichan)%no2, ch_out_y(ichan)%no2, ch_in_y(ichan)%cbod, ch_out_y(ichan)%cbod,               &
          !   ch_in_y(ichan)%dox, ch_out_y(ichan)%dox
          end if
        end if
      end if

!!!!! average annual print
      if (time%end_sim == 1) then
        !ch_stor_a(ichan) = ch_stor_a(ichan) / time%yrs_prt      !! all storage variables (averaged) must be divided by years
        ch_in_a(ichan) = ch_in_a(ichan) / time%yrs_prt          !! all inflow and outflow varaibles (summed) are divided by years
        ch_out_a(ichan) = ch_out_a(ichan) / time%yrs_prt
        ch_wat_a(ichan) = ch_wat_a(ichan) / time%yrs_prt        !! all summed variables divided by years
        ch_wat_a(ichan) = ch_wat_a(ichan) // time%yrs_prt       !! all averaged variables divided by years
        
        if (pco%sd_chan%a == "y") then
        write (2503,100) time%day, time%mo, time%day_mo, time%yrc, ichan, ob(iob)%gis_id, ob(iob)%name, ch_wat_a(ichan), &
          ch_stor(ichan), ch_in_a(ichan), ch_out_a(ichan), wtemp
       !! new sd_channel output file
          !write (3515,100)  time%day, time%mo, time%day_mo, time%yrc, ichan, ob(iob)%gis_id, ob(iob)%name,    &
            !ch_in_a(ichan)%flo, ch_out_a(ichan)%flo, ch_in_a(ichan)%sed, ch_out_a(ichan)%sed,                 &
            !ch_in_a(ichan)%orgn, ch_out_a(ichan)%orgn, ch_in_a(ichan)%sedp, ch_out_a(ichan)%sedp,             &            ch_in_a(ichan)%no3, ch_out_a(ichan)%no3, ch_in_a(ichan)%solp, ch_out_a(ichan)%solp,               &
            !ch_in_a(ichan)%chla, ch_out_a(ichan)%chla, ch_in_a(ichan)%nh3, ch_out_a(ichan)%nh3,               &
            !ch_in_a(ichan)%no2, ch_out_a(ichan)%no2, ch_in_a(ichan)%cbod, ch_out_a(ichan)%cbod,               &
            !ch_in_a(ichan)%dox, ch_out_a(ichan)%dox
        if (pco%csvout == "y") then
          write (2507,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, ichan, ob(iob)%gis_id, ob(iob)%name,   &
          ch_wat_a(ichan), ch_stor(ichan), ch_in_a(ichan), ch_out_a(ichan), wtemp
          !write (3516,'(*(G0.3,:","))')  time%day, time%mo, time%day_mo, time%yrc, ichan, ob(iob)%gis_id, ob(iob)%name,  &
          !   ch_in_a(ichan)%flo, ch_out_a(ichan)%flo, ch_in_a(ichan)%sed, ch_out_a(ichan)%sed,                 &
          !   ch_in_a(ichan)%orgn, ch_out_a(ichan)%orgn, ch_in_a(ichan)%sedp, ch_out_a(ichan)%sedp,             &
          !   ch_in_a(ichan)%no3, ch_out_a(ichan)%no3, ch_in_a(ichan)%solp, ch_out_a(ichan)%solp,               &
          !   ch_in_a(ichan)%chla, ch_out_a(ichan)%chla, ch_in_a(ichan)%nh3, ch_out_a(ichan)%nh3,               &
          !   ch_in_a(ichan)%no2, ch_out_a(ichan)%no2, ch_in_a(ichan)%cbod, ch_out_a(ichan)%cbod,               &
          !   ch_in_a(ichan)%dox, ch_out_a(ichan)%dox
        end if
       end if
     end if 
      
      return

100   format (4i6,2i8,2x,a,61e15.4)      
       
      end subroutine sd_channel_output