      subroutine climate_control
 
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
           
      integer :: ii               !none          |counter       
      integer :: iyp              !none          |year currently being simulated
      integer :: idap             !julain date   |day currently being simulated
      real :: petmeas             !mm H2O        |potential ET value read in for day 
      real :: half_hr_mn          !mm H2O        |lowest value half hour precip fraction can have
      real :: half_hr_mx          !mm H2O        |highest value half hour precip fraction can have
      integer :: iwgn             !              |
      integer :: ipet             !              |
      integer :: ig               !              |
      integer :: yrs_to_start     !              |
      integer :: cur_day
      real :: ramm                !MJ/m2         |extraterrestrial radiation
      real :: xl                  !MJ/kg         |latent heat of vaporization
      real :: atri                !none          |daily value generated for distribution
      integer :: ifirstpet = -1   !none          |potential ET data search code
                                  !              |0 first day of potential ET data located in
                                  !              |file
                                  !              |1 first day of potential ET data not located
                                  !              |in file
      real :: xx
      character(len=1) :: out_bounds = 'n'
        
      !! Precipitation:
      call cli_precip_control (1)
      
!! Temperature: 
      do iwst = 1, db_mx%wst
        iwgn = wst(iwst)%wco%wgn
        call cli_weatgn(iwgn)
        if (wst(iwst)%wco_c%tgage == "sim") then
          call cli_tgen(iwgn)
        else
          ig = wst(iwst)%wco%tgage
          out_bounds = "n"
          cur_day = time%day
          call cli_bounds_check (tmp(ig)%start_day, tmp(ig)%start_yr,       &
                                tmp(ig)%end_day, tmp(ig)%end_yr, out_bounds)
          if (out_bounds == "y") then
            wst(iwst)%weat%tmax = -98.
            wst(iwst)%weat%tmin = -98.
          else
              yrs_to_start = time%yrs - tmp(ig)%yrs_start
              wst(iwst)%weat%tmax = tmp(ig)%ts(time%day,yrs_to_start)
              wst(iwst)%weat%tmin = tmp(ig)%ts2(time%day,yrs_to_start)
          end if
          if (wst(iwst)%weat%tmax <= -97. .or. wst(iwst)%weat%tmin <= -97.) then
            call cli_weatgn(iwgn)
            call cli_tgen(iwgn)
            tmp(ig)%days_gen = tmp(ig)%days_gen + 1
          end if
        end if
        wst(iwst)%weat%tave = (wst(iwst)%weat%tmax + wst(iwst)%weat%tmin) / 2.
      end do

!! Solar Radiation: 
      do iwst = 1, db_mx%wst
        iwgn = wst(iwst)%wco%wgn
        call cli_clgen(iwgn)
        if (wst(iwst)%wco_c%sgage== "sim") then
          call cli_slrgen(iwgn)
        else
          ig = wst(iwst)%wco%sgage
          out_bounds = "n"
          cur_day = time%day
          call cli_bounds_check (slr(ig)%start_day, slr(ig)%start_yr,       &
                                slr(ig)%end_day, slr(ig)%end_yr, out_bounds)
          if (out_bounds == "y") then 
            wst(iwst)%weat%solrad = -98.
          else
            yrs_to_start = time%yrs - slr(ig)%yrs_start
            wst(iwst)%weat%solrad = slr(ig)%ts(time%day,yrs_to_start)
          end if
          if (wst(iwst)%weat%solrad <= -97.) then
            call cli_slrgen(iwgn)
            slr(ig)%days_gen = slr(ig)%days_gen + 1
          end if
        end if
      end do
        
!! Relative Humidity: 
      do iwst = 1, db_mx%wst
        iwgn = wst(iwst)%wco%wgn
        if (wst(iwst)%wco_c%hgage == "sim") then
          call cli_rhgen(iwgn)
        else
          ig = wst(iwst)%wco%hgage
          out_bounds = "n"
          cur_day = time%day
          call cli_bounds_check (hmd(ig)%start_day, hmd(ig)%start_yr,       &
                                hmd(ig)%end_day, hmd(ig)%end_yr, out_bounds)
          if (out_bounds == "y") then 
            wst(iwst)%weat%rhum = -98.
          else
            yrs_to_start = time%yrs - hmd(ig)%yrs_start
            wst(iwst)%weat%rhum = hmd(ig)%ts(time%day,yrs_to_start)
          end if
          if (wst(iwst)%weat%rhum <= -97.) then
            call cli_rhgen(iwgn)
            hmd(ig)%days_gen = hmd(ig)%days_gen + 1
          end if
        end if
        !! simple dewpoint eqn from Lawrence 2005. Bull. Amer. Meteor. Soc.
        wst(iwst)%weat%dewpt = wst(iwst)%weat%tave - (1. - wst(iwst)%weat%rhum) / 5.
      end do 

!! Wind Speed: 
      do iwst = 1, db_mx%wst
        iwgn = wst(iwst)%wco%wgn
        if (wst(iwst)%wco_c%wgage == "sim") then
          call cli_wndgen(iwgn)
        else
          ig = wst(iwst)%wco%wgage
          out_bounds = "n"
          cur_day = time%day
          call cli_bounds_check (wnd(ig)%start_day, wnd(ig)%start_yr,       &
                                wnd(ig)%end_day, wnd(ig)%end_yr, out_bounds)
          if (out_bounds == "y") then 
            wst(iwst)%weat%windsp = -98.
          else
            yrs_to_start = time%yrs - wnd(ig)%yrs_start
            wst(iwst)%weat%windsp = wnd(ig)%ts(time%day,yrs_to_start)
          end if
          if (wst(iwst)%weat%windsp <= -97.) then
            call cli_wndgen(iwgn)
            wnd(ig)%days_gen = wnd(ig)%days_gen + 1
          end if
        end if
      end do 

!! Potential ET: Read in data !!
      do iwst = 1, db_mx%wst
        iwgn = wst(iwst)%wco%wgn
        ig = wst(iwst)%wco%petgage
        !! if using a measured data
        if (ig > 0) then
          out_bounds = "n"
          cur_day = time%day
          call cli_bounds_check (petm(ig)%start_day, petm(ig)%start_yr,       &
                petm(ig)%end_day, petm(ig)%end_yr, out_bounds)
          if (out_bounds == "y") then 
            wst(iwst)%weat%pet = -98.
          else
            yrs_to_start = time%yrs - petm(ig)%yrs_start
            wst(iwst)%weat%pet = petm(ig)%ts(time%day,yrs_to_start)
          end if
          if (wst(iwst)%weat%pet <= -97.) then
            !! Use HARGREAVES POTENTIAL EVAPOTRANSPIRATION METHOD
            ramm = wst(iwst)%weat%solradmx * 37.59 / 30. 
            if (wst(iwst)%weat%tmax > wst(iwst)%weat%tmin) then
              xl = 2.501 - 2.361e-3 * wst(iwst)%weat%tave
              wst(iwst)%weat%pet = .0023 * (ramm / xl) * (wst(iwst)%weat%tave      &
              + 17.8) * (wst(iwst)%weat%tmax - wst(iwst)%weat%tmin) ** 0.5
              wst(iwst)%weat%pet = Max(0., wst(iwst)%weat%pet)
            else
              wst(iwst)%weat%pet = 0.
            end if
          end if
        end if
      end do

!! Update CMI and Precip minus PET 30 day moving sum
      ppet_mce = ppet_mce + 1
      if (ppet_mce > ppet_ndays) ppet_mce = 1
      do iwst = 1, db_mx%wst
        !! calculate climatic moisture index - cumulative p/pet
        !! Use Hargreaves Potential ET Method 
        ramm = wst(iwst)%weat%solradmx * 37.59 / 30. 
        if (wst(iwst)%weat%tmax > wst(iwst)%weat%tmin) then
          xl = 2.501 - 2.361e-3 * wst(iwst)%weat%tave
          wst(iwst)%weat%pet = .0023 * (ramm / xl) * (wst(iwst)%weat%tave      &
                + 17.8) * (wst(iwst)%weat%tmax - wst(iwst)%weat%tmin) ** 0.5
          wst(iwst)%weat%pet = Max(0., wst(iwst)%weat%pet)
        else
          wst(iwst)%weat%pet = 0.
        endif
        if (wst(iwst)%weat%pet > 0.1) then
          wst(iwst)%weat%ppet = wst(iwst)%weat%ppet + wst(iwst)%weat%precip / wst(iwst)%weat%pet
        end if
        !! subtract the 30 day previous and add the current day precip/pet
        iwgn = wst(iwst)%wco%wgn
        wgn_pms(iwgn)%precip_sum = wgn_pms(iwgn)%precip_sum + wst(iwst)%weat%precip - wgn_pms(iwgn)%precip_mce(ppet_mce)
        wgn_pms(iwgn)%pet_sum = wgn_pms(iwgn)%pet_sum + wst(iwst)%weat%pet - wgn_pms(iwgn)%pet_mce(ppet_mce)
        wgn_pms(iwgn)%p_pet_rto = wgn_pms(iwgn)%precip_sum / wgn_pms(iwgn)%pet_sum
        wgn_pms(iwgn)%precip_mce(ppet_mce) = wst(iwst)%weat%precip
        wgn_pms(iwgn)%pet_mce(ppet_mce) = wst(iwst)%weat%pet
        
        !! Calculate air temperature lag day for stream temperature
        !! replace current day temperature
        wst(iwst)%tlag(wst(iwst)%tlag_mne) = wst(iwst)%weat%tave
        !! lag day is the next variable in array
        wst(iwst)%tlag_mne = wst(iwst)%tlag_mne + 1
        if (wst(iwst)%tlag_mne > w_temp%airlag_d) wst(iwst)%tlag_mne = 1
        wst(iwst)%airlag_temp = wst(iwst)%tlag(wst(iwst)%tlag_mne)
      end do
            
!! Calculate maximum half-hour rainfall fraction
      do iwst = 1, db_mx%wst
        iwgn = wst(iwst)%wco%wgn
        half_hr_mn = 0.02083
        xx = (-125. / (wst(iwst)%weat%precip + 5.))
        half_hr_mx = 1. - Exp(xx)
        wst(iwst)%weat%precip_half_hr = Atri(half_hr_mn, wgn_pms(iwgn)%amp_r(time%mo), half_hr_mx, rndseed(10,iwgn))
      end do

!! Base Zero Heat Units
      do iwst = 1, db_mx%wst
        iwgn = wst(iwst)%wco%wgn
        if (wgn_pms(iwgn)%phutot > 0.) then
          if (wst(iwst)%weat%tave > 0.) wst(iwst)%weat%phubase0 = wst(iwst)%weat%phubase0   &
                                            + wst(iwst)%weat%tave / wgn_pms(iwgn)%phutot
        else
          wst(iwst)%weat%phubase0 = 0.
        end if
        if (time%end_yr == 1) wst(iwst)%weat%phubase0 = 0.
      end do

!! Climate Change Adjustments !!
      do iwst = 1, db_mx%wst
        wst(iwst)%weat%precip = wst(iwst)%weat%precip * (1. + wst(iwst)%rfinc(time%mo) / 100.)
        if (wst(iwst)%weat%precip < 0.) wst(iwst)%weat%precip = 0.
        if (time%step > 1) then
          do ii = 1, time%step
            wst(iwst)%weat%ts(ii) = wst(iwst)%weat%ts(ii) * (1. + wst(iwst)%rfinc(time%mo) / 100.)
            if (wst(iwst)%weat%ts(ii) < 0.) wst(iwst)%weat%ts(ii) = 0.
          end do
        end if
        wst(iwst)%weat%tmax = wst(iwst)%weat%tmax + wst(iwst)%tmpinc(time%mo)
        wst(iwst)%weat%tmin = wst(iwst)%weat%tmin + wst(iwst)%tmpinc(time%mo)
        wst(iwst)%weat%solrad = wst(iwst)%weat%solrad + wst(iwst)%radinc(time%mo)
        wst(iwst)%weat%solrad = Max(0.,wst(iwst)%weat%solrad)
        wst(iwst)%weat%rhum = wst(iwst)%weat%rhum + wst(iwst)%huminc(time%mo)
        wst(iwst)%weat%rhum = Max(0.01,wst(iwst)%weat%rhum)
        wst(iwst)%weat%rhum = Min(0.99,wst(iwst)%weat%rhum)
      end do

      return

      end subroutine climate_control