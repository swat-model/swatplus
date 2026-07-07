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
      integer :: k                !none          |counter
      integer :: inum3sprev       !none          |subbasin number of previous HRU
      integer :: ii               !none          |counter       
      integer :: iyp              !none          |year currently being simulated
      integer :: idap             !julain date   |day currently being simulated
      integer :: ib               !none          |counter
      real :: tdif                !deg C         |difference in temperature for station and
                                  !              |temperature for elevation band
      real :: pdif                !mm H2O        |difference in precipitation for station and
                                  !              |precipitation for elevation band
      real :: ratio               !none          |fraction change in precipitation due to 
                                  !              |elevation changes
      real :: petmeas             !mm H2O        |potential ET value read in for day 
      real :: half_hr_mn          !mm H2O        |lowest value half hour precip fraction can have
      real :: half_hr_mx          !mm H2O        |highest value half hour precip fraction can have
      integer :: iwgn             !              |
      integer :: ipg              !              | 
      integer :: ist              !none          |counter
      integer :: ig               !              |
      integer :: yrs_to_start     !              |
      integer :: cur_day
      real :: ramm                !MJ/m2         |extraterrestrial radiation
      real :: xl                  !MJ/kg         |latent heat of vaporization
      real :: expo                !              | 
      real :: atri                !none          |daily value generated for distribution
      real :: ifirstpet           !none          |potential ET data search code
                                  !              |0 first day of potential ET data located in
                                  !              |file
                                  !              |1 first day of potential ET data not located
                                  !              |in file
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
          if (time%step > 0) then
            call cli_pgenhr(iwgn)
            wst(iwst)%weat%precip_next = sum (wst(iwst)%weat%ts(:))
          end if
        else
          !! measured precip
          if (pcp(ipg)%tstep > 0) then
          !! subdaily precip
            wst(iwst)%weat%precip_next = 0.
            do ist = 1, time%step
              wst(iwst)%weat%ts_next(ist) = pcp(ipg)%tss(ist,time%day,time%yrs)
              if (wst(iwst)%weat%ts_next(ist) <= -97.) then
				!! simulate missing data
				call cli_pgen(iwgn)
				call cli_pgenhr(iwgn)
				exit
			  end if
			  wst(iwst)%weat%precip_next = wst(iwst)%weat%precip_next + wst(iwst)%weat%ts_next(ist)
            end do
            wst(iwst)%weat%precip_next = sum (pcp(ipg)%tss(:,time%day,time%yrs))
          else
		  !! daily precip
            out_bounds = "n"
            cur_day = time%day + istart
            yrs_to_start = time%yrs - pcp(ipg)%yrs_start
            if (cur_day > time%day_end_yr) then
              cur_day = 1
              yrs_to_start = yrs_to_start + 1
            end if
            call cli_bounds_check (cur_day, pcp(ipg)%start_day, pcp(ipg)%start_yr,       &
                                pcp(ipg)%end_day, pcp(ipg)%end_yr, out_bounds)
            if (yrs_to_start > pcp(ipg)%end_yr - pcp(ipg)%start_yr + 1) out_bounds = "y"
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

      end do
      
      return

      end subroutine cli_precip_control