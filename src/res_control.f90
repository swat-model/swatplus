      subroutine res_control (jres)
      
      use basin_module
      use reservoir_data_module 
      use time_module
      use reservoir_module
      use climate_module
      use hydrograph_module
      use conditional_module
      use water_body_module
      use constituent_mass_module  !! added nbs
      
      implicit none

      integer :: ii                   !none          |counter 
      integer :: jres                 !none          |reservoir number
      integer :: idat                 !              |
      integer :: irel                 !              |
      integer :: iob                  !none          |counter
      integer :: ictbl
      integer :: icon                  !! nbs
      real :: pvol_m3
      real :: evol_m3
      real :: dep
      real :: weir_hgt
      real :: alpha_up
      real :: alpha_down

      ht1 = ob(icmd)%hin    !! set incoming flow
      ht2 = resz            !! zero outgoing flow

      if (time%yrc > res_hyd(jres)%iyres .or. (time%mo >= res_hyd(jres)%mores   &
                                   .and. time%yrc == res_hyd(jres)%iyres)) then
        iob = res_ob(jres)%ob
        iwst = ob(iob)%wst
      
        !! adjust precip and temperature for elevation using lapse rates
        w = wst(iwst)%weat
        if (bsn_cc%lapse == 1) call cli_lapse
        wst(iwst)%weat = w
      
        !! set water body pointer to res
        wbody => res(jres)
        wbody_wb => res_wat_d(jres)
        wbody_prm => res_prm(jres)
      
        !! add incoming flow to reservoir
        res(jres) = res(jres) + ht1

        !! perform reservoir water/sediment balance
        idat = res_ob(jres)%props
        if(res_ob(jres)%rel_tbl == "d") then
          !! determine reservoir outflow
          irel = res_dat(idat)%release
          d_tbl => dtbl_res(irel)
          pvol_m3 = 0.5 * res_ob(jres)%pvol
          evol_m3 = 0.5 * res_ob(jres)%evol
          if (res_wat_d(jres)%area_ha > 1.e-6) then
            dep = wbody%flo / res_wat_d(jres)%area_ha / 10000.     !m = m3 / ha / 10000m2/ha
          else
            dep = 0.
          end if
          weir_hgt = res_ob(jres)%weir_hgt
          call conditions (jres, irel)
          call res_hydro (jres, irel, pvol_m3, evol_m3)
          
          !! new lag to smooth condition jumps (volume or month conditions)
          alpha_up = Exp(-res_ob(jres)%lag_up)
          alpha_down = Exp(-res_ob(jres)%lag_down)
          !! lag outflow when flows are receeding
          if (res_ob(jres)%prev_flo < ht2%flo) then
            ht2%flo = ht2%flo * alpha_up + res_ob(jres)%prev_flo * (1. - alpha_up)
          else
            ht2%flo = ht2%flo * alpha_down + res_ob(jres)%prev_flo * (1. - alpha_down)
          end if
          res_ob(jres)%prev_flo = ht2%flo
            
          call res_sediment
	    else
	      ictbl = res_dat(idat)%release                              !! Osvaldo
          call res_rel_conds (ictbl, res(jres)%flo, ht1%flo, 0.)
          
        endif 
        
        !! calculate water balance for day
        res_wat_d(jres)%evap = 10. * res_hyd(jres)%evrsv * wst(iwst)%weat%pet * res_wat_d(jres)%area_ha
        res_wat_d(jres)%precip = 10. * wst(iwst)%weat%precip * res_wat_d(jres)%area_ha
        if(bsn_cc%gwflow == 0) then !if gwflow active, seepage calculated in gwflow_simulate (rtb gwflow)
          res_wat_d(jres)%seep = 240. * res_hyd(jres)%k * res_wat_d(jres)%area_ha
        endif

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
        !res(jres)%sed = max (0., res(jres)%sed - ht2%sed)
        !res(jres)%sil = max (0., res(jres)%sil - ht2%sil)
        !res(jres)%cla = max (0., res(jres)%cla - ht2%cla)
          
        !! perform reservoir nutrient balance
        call res_nutrient (iob)

        !! perform reservoir pesticide transformations
        if (cs_db%num_pests > 0) then
          call res_pest (jres)
          obcs(icmd)%hd(1)%pest = hcs2%pest
        end if

        !! perform reservoir salt process (rtb salt)
        if (cs_db%num_salts > 0) then
          call res_salt(jres)
          obcs(icmd)%hd(1)%salt = hcs2%salt
        endif
        
        !! perform reservoir constituent process (rtb cs)
        if(cs_db%num_cs > 0) then
          icon = res_dat(idat)%cs
          call res_cs(jres, icon, iob)
          obcs(icmd)%hd(1)%cs = hcs2%cs
        endif
        
        !! set values for outflow variables
        ob(icmd)%hd(1) = ht2
        if (cs_db%num_tot > 0) then
          obcs(icmd)%hd(1) = hcs2
        end if

        !! total incoming to output to SWIFT
        ob(icmd)%hin_tot = ob(icmd)%hin_tot + ob(icmd)%hin     
        !! total outgoing to output to SWIFT
        ob(icmd)%hout_tot = ob(icmd)%hout_tot + ht2
        
        if (time%step > 1) then
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

  !!!! for Luis only    
      !if (jres == 1) then
      !  write (7777,*) time%day, time%yrc, jres, res(jres)%flo, ht1%flo, ht2%flo,   &
      !                                           res(jres)%sed, ht1%sed, ht2%sed
      !end if
  !!!! for Luis only
      
      return
      end subroutine res_control