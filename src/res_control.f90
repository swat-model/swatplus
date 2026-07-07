      subroutine res_control (jres)
      
      use basin_module
      use reservoir_data_module 
      use time_module
      use reservoir_module
      use climate_module
      use hydrograph_module
      use conditional_module
      use water_body_module
      
      implicit none

      integer :: ii                   !none          |counter 
      integer :: jres                 !none          |reservoir number
      integer :: idat                 !              |
      integer :: ihyd                 !none          |counter
      integer :: ised                 !none          |counter
      integer :: irel                 !              |
      integer :: inut                 !none          |counter
      integer :: iob                  !none          |counter
      real :: pvol_m3
      real :: evol_m3

      iob = res_ob(jres)%ob
      iwst = ob(iob)%wst
      
      !! adjust precip and temperature for elevation using lapse rates
      w = wst(iwst)%weat
      if (bsn_cc%lapse == 1) call cli_lapse
      wst(iwst)%weat = w
      
      !! set water body pointer to res
      wbody => res(jres)
      wbody_wb => res_wat_d(jres)
      
      ht1 = ob(icmd)%hin    !! set incoming flow
      ht2 = resz            !! zero outgoing flow

      !! add incoming flow to reservoir
      res(jres) = res(jres) + ht1

      if (time%yrc > res_hyd(jres)%iyres .or. (time%mo >= res_hyd(jres)%mores   &
                                   .and. time%yrc == res_hyd(jres)%iyres)) then
        !! perform reservoir water/sediment balance
        idat = res_ob(jres)%props
        ihyd = res_dat(idat)%hyd
        ised = res_dat(idat)%sed
        if(time%step == 0) then
          !! determine reservoir outflow
          irel = res_dat(idat)%release
          d_tbl => dtbl_res(irel)
          pvol_m3 = res_ob(jres)%pvol
          evol_m3 = res_ob(jres)%evol
          call conditions (jres, irel)
          call res_hydro (jres, irel, ihyd, pvol_m3, evol_m3)
          call res_sediment (jres, ihyd, ised)
	    else
	      !call res_hourly
        endif

        
      !! calculate water balance for day
      res_wat_d(jres)%evap = 10. * res_hyd(ihyd)%evrsv * wst(iwst)%weat%pet * res_wat_d(jres)%area_ha
      res_wat_d(jres)%seep = 240. * res_hyd(ihyd)%k * res_wat_d(jres)%area_ha
      res_wat_d(jres)%precip = 10. * wst(iwst)%weat%precip * res_wat_d(jres)%area_ha

      !! add precip to reservoir storage
      res(jres)%flo = res(jres)%flo + res_wat_d(jres)%precip

      !! subtract outflow from reservoir storage
      res(jres)%flo = res(jres)%flo - ht2%flo
      if (res(jres)%flo < 0.) then
        ht2%flo = ht2%flo + res(jres)%flo
        res(jres)%flo = 0.
      end if

      !! subtract evaporation from reservoir storage
      res(jres)%flo = res(jres)%flo - res_wat_d(jres)%evap
      if (res(jres)%flo < 0.) then
        res_wat_d(jres)%evap = res_wat_d(jres)%evap + res(jres)%flo
        res(jres)%flo = 0.
      end if
      
      !! subtract seepage from reservoir storage
      res(jres)%flo = res(jres)%flo - res_wat_d(jres)%seep
      if (res(jres)%flo < 0.) then
        res_wat_d(jres)%seep = res_wat_d(jres)%seep + res(jres)%flo
        res(jres)%flo = 0.
      end if

        !! update surface area
        if (res(jres)%flo > 0.) then
          res_wat_d(jres)%area_ha = res_ob(jres)%br1 * res(jres)%flo ** res_ob(jres)%br2
        else
          res_wat_d(jres)%area_ha = 0.
        end if

        !! subtract sediment leaving from reservoir
        res(jres)%sed = res(jres)%sed - ht2%sed
        res(jres)%sil = res(jres)%sil - ht2%sil
        res(jres)%cla = res(jres)%cla - ht2%cla
          
        !! perform reservoir nutrient balance
        inut = res_dat(idat)%nut
        call res_nutrient (jres, inut, iob)

        !! perform reservoir pesticide transformations
        call res_pest (jres)

        !! set values for outflow variables
        ob(icmd)%hd(1) = ht2

        if (time%step > 0) then
          do ii = 1, time%step
            ob(icmd)%ts(1,ii) = ht2 / real(time%step)
          end do
        end if

        !! set inflow and outflow variables for reservoir_output
        if (time%yrs > pco%nyskip) then
          res_in_d(jres) = ht1 
          res_out_d(jres) = ht2
          !res_in_d(jres)%flo = res_in_d(jres)%flo / 10000.          !m^3 -> ha-m
          !res_out_d(jres)%flo = res_out_d(jres)%flo / 10000.        !m^3 -> ha-m
          !res_wat_d(jres)%evap = res_wat_d(jres)%evap / 10000.      !m^3 -> ha-m
          !res_wat_d(jres)%seep = res_wat_d(jres)%seep / 10000.      !m^3 -> ha-m
          !res_wat_d(jres)%precip = res_wat_d(jres)%precip / 10000.  !m^3 -> ha-m
        end if             
        
      else
        !! reservoir has not been constructed yet
        ob(icmd)%hd(1) = ob(icmd)%hin
      end if

      return
      end subroutine res_control