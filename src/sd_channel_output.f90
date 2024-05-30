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
      
         if (pco%day_print == "y" .and. time%step > 1 .and. pco%int_day_cur == pco%int_day) then
          do ii = 1, time%step 
            write (2508,101)  time%day, time%mo, time%day_mo, time%yrc, ichan, ob(iob)%gis_id, ii, ob(iob)%name,  &
                     ob(iob)%hyd_flo(1,ii)
          end do
         end if
           
!!!!! daily print
       if (pco%day_print == "y" .and. pco%int_day_cur == pco%int_day) then
        if (pco%sd_chan%d == "y") then
          write (2500,100) time%day, time%mo, time%day_mo, time%yrc, ichan, ob(iob)%gis_id, ob(iob)%name,       &
            ch_wat_d(ichan)%area_ha, ch_wat_d(ichan)%precip, ch_wat_d(ichan)%evap, ch_wat_d(ichan)%seep,        &                
            ch_stor(ichan), ch_in_d(ichan), ch_out_d(ichan), wtemp
           if (pco%csvout == "y") then
             write (2504,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, ichan, ob(iob)%gis_id, ob(iob)%name,  &
               ch_wat_d(ichan)%area_ha, ch_wat_d(ichan)%precip, ch_wat_d(ichan)%evap, ch_wat_d(ichan)%seep,     &
               ch_stor(ichan), ch_in_d(ichan), ch_out_d(ichan), wtemp
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
          write (2501,100) time%day, time%mo, time%day_mo, time%yrc, ichan, ob(iob)%gis_id, ob(iob)%name,  &
            ch_wat_m(ichan)%area_ha, ch_wat_m(ichan)%precip, ch_wat_m(ichan)%evap, ch_wat_m(ichan)%seep,   &
            ch_stor(ichan), ch_in_m(ichan), ch_out_m(ichan), wtemp

          if (pco%csvout == "y") then
            write (2505,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, ichan, ob(iob)%gis_id, ob(iob)%name,   &
            ch_wat_m(ichan)%area_ha, ch_wat_m(ichan)%precip, ch_wat_m(ichan)%evap, ch_wat_m(ichan)%seep,   &
            ch_stor(ichan), ch_in_m(ichan), ch_out_m(ichan), wtemp
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
          write (2502,100) time%day, time%mo, time%day_mo, time%yrc, ichan, ob(iob)%gis_id, ob(iob)%name,   &
            ch_wat_y(ichan)%area_ha, ch_wat_y(ichan)%precip, ch_wat_y(ichan)%evap, ch_wat_y(ichan)%seep,    &
            ch_stor(ichan), ch_in_y(ichan), ch_out_y(ichan), wtemp
          if (pco%csvout == "y") then
           write (2506,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, ichan, ob(iob)%gis_id, ob(iob)%name,    &
            ch_wat_y(ichan)%area_ha, ch_wat_y(ichan)%precip, ch_wat_y(ichan)%evap, ch_wat_y(ichan)%seep,    &
            ch_stor(ichan), ch_in_y(ichan), ch_out_y(ichan), wtemp
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
        write (2503,100) time%day, time%mo, time%day_mo, time%yrc, ichan, ob(iob)%gis_id, ob(iob)%name,  &
          ch_wat_a(ichan)%area_ha, ch_wat_a(ichan)%precip, ch_wat_a(ichan)%evap, ch_wat_a(ichan)%seep,   &
          ch_stor(ichan), ch_in_a(ichan), ch_out_a(ichan), wtemp
        if (pco%csvout == "y") then
          write (2507,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, ichan, ob(iob)%gis_id, ob(iob)%name,   &
          ch_wat_a(ichan)%area_ha, ch_wat_a(ichan)%precip, ch_wat_a(ichan)%evap, ch_wat_a(ichan)%seep,   &
          ch_stor(ichan), ch_in_a(ichan), ch_out_a(ichan), wtemp
        end if
       end if
     end if 
      
      return

100   format (4i6,2i8,2x,a,73e15.4)
101   format (4i6,3i8,2x,a,e15.4)  
       
      end subroutine sd_channel_output