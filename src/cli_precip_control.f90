      subroutine cli_precip_control (istart)
 
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine controls weather inputs to SWAT. Precipitation and
!!    temperature data is read in and the weather generator is called to 
!!    fill in radiation, wind speed and relative humidity as well as 
!!    missing precipitation and temperatures. Adjustments for climate
!!    changes studies are also made in this subroutine.

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition  
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    elevp(:)    |m             |elevation of precipitation gage station
!!    elevt(:)    |m             |elevation of temperature gage station
!!    welev(:)    |m             |elevation of weather station used to compile
!!                               |weather generator data
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max, Min
!!    SWAT: pmeas, tmeas, smeas, hmeas, wmeas
!!    SWAT: pgen, tgen, weatgn, clgen, slrgen, rhgen, wndgen

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use climate_module
      use basin_module
      use time_module
      use hydrograph_module
      use maximum_data_module
      
      implicit none
           
      integer, intent (in) :: istart           !none          |0 for initial (first day), 1 for following days
      integer :: iwgn             !              |
      integer :: ipg              !              | 
      integer :: ist              !none          |counter
      integer :: yrs_to_start     !              |
      integer :: cur_day

      character(len=1) :: out_bounds = 'n'
        
      !! Precipitation:
      do iwst = 1, db_mx%wst
                  
        !! set current day to next day (that was previously calculated)
        wst(iwst)%weat%precip = wst(iwst)%weat%precip_next
        wst(iwst)%weat%precip_next = 0.
        wst(iwst)%weat%ts = wst(iwst)%weat%ts_next
        
        iwgn = wst(iwst)%wco%wgn
        ipg = wst(iwst)%wco%pgage
        if (wst(iwst)%wco_c%pgage == "sim") then
          !! simulated precip
          call cli_pgen(iwgn)
          if (time%step > 1) then
            call cli_pgenhr
            wst(iwst)%weat%precip_next = sum (wst(iwst)%weat%ts(:))
          end if
        else
          !! measured precip
          !! check to see if out of bounds (simulation starts before or ends after precip data)
          out_bounds = "n"
          cur_day = time%day + istart
          yrs_to_start = time%yrs - pcp(ipg)%yrs_start
          if (cur_day > time%day_end_yr) then
            cur_day = 1
            yrs_to_start = yrs_to_start + 1
          end if
          call cli_bounds_check (pcp(ipg)%start_day, pcp(ipg)%start_yr,       &
                                pcp(ipg)%end_day, pcp(ipg)%end_yr, out_bounds)
          if (yrs_to_start > pcp(ipg)%end_yr - pcp(ipg)%start_yr + 1) out_bounds = "y"
            
          if (pcp(ipg)%tstep > 0) then
            !! subdaily precip
            if (out_bounds == "y") then 
              wst(iwst)%weat%ts_next = -98.
            end if
            wst(iwst)%weat%precip_next = 0.
            do ist = 1, time%step
              wst(iwst)%weat%ts_next(ist) = pcp(ipg)%tss(ist,cur_day,time%yrs)
              if (wst(iwst)%weat%ts_next(ist) <= -97.) then
				!! simulate missing data
				call cli_pgen(iwgn)
				call cli_pgenhr
				exit
			  end if
			  wst(iwst)%weat%precip_next = wst(iwst)%weat%precip_next + wst(iwst)%weat%ts_next(ist)
            end do
            wst(iwst)%weat%precip_next = sum (pcp(ipg)%tss(:,cur_day,time%yrs))
          else
		  !! daily precip
            if (out_bounds == "y") then 
              wst(iwst)%weat%precip_next = -98.
            else
              wst(iwst)%weat%precip_next = pcp(ipg)%ts(cur_day, yrs_to_start)
            end if

            !! simulate missing data
            if (wst(iwst)%weat%precip_next <= -97.) then
              call cli_pgen(iwgn)
              pcp(ipg)%days_gen = pcp(ipg)%days_gen + 1
			end if
          end if
        end if

        !! sum to get ave annual precip for SWIFT input 
        if (istart > 0) then
          wst(iwst)%precip_aa = wst(iwst)%precip_aa + wst(iwst)%weat%precip
          wst(iwst)%pet_aa = wst(iwst)%pet_aa + wst(iwst)%weat%pet
        end if
        
      end do
      
      return

      end subroutine cli_precip_control