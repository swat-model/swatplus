      subroutine wetland_control
    
      use reservoir_data_module
      use reservoir_module
      use hru_module, only : hru, sedyld, sanyld, silyld, clayld, sagyld, lagyld, grayld, sedminps, sedminpa,   &
        surqno3, sedorgn, sedorgp, ihru, pet_day, surfq, tconc, usle_cfac, cklsp, hhsurfq
      use conditional_module
      use climate_module
      use hydrograph_module
      use time_module
      use basin_module
      use channel_module
      use water_body_module
      use soil_module
      use organic_mineral_mass_module
      use mgt_operations_module
      use constituent_mass_module
      use aquifer_module
      use gwflow_module
      
      implicit none
     
      real :: bypass = 1.             !              | 
      integer :: j = 0                !none          |counter
      real :: x1 = 0.                 !              |
      real :: wet_h = 0.              !              |
      real :: wet_h1 = 0.             !              | 
      integer :: ised = 0             !none          |counter
      integer :: irel = 0             !              |
      integer :: icon = 0             !none          |counter: identifies parameter list in cs_res (rtb cs)
      integer :: ires = 0
      integer :: j1 = 0
      integer :: ii = 0               !none          |sub daily time step counter
      integer :: ihyd = 0             !none          |counter  !Jaehak 2024
      integer :: isched = 0           !none          |counter  !Jaehak 2024
      real :: wet_fr = 0.
      real :: pvol_m3 = 0.
      real :: evol_m3 = 0.
      real :: dep = 0.
      real :: weir_hgt = 0.
      real :: wsa1 = 0.
      real :: sedppm = 0.
      real :: no3ppm = 0.
      real :: seep_rto = 0.
      real :: qp_cms = 0.
      real :: dep_init = 0.
      real :: volseep = 0.
      real :: volex = 0.
      real :: swst(20) = 0.
      j = ihru
      ires = hru(j)%dbs%surf_stor
      ihyd = wet_dat(ires)%hyd
      ised = wet_dat(ires)%sed
      irel = wet_dat(ires)%release
      wsa1 = hru(j)%area_ha * 10.
      isched = hru(j)%mgt_ops
      wet_wat_d(j)%area_ha = hru(j)%area_ha
      
      !! zero outgoing flow 
      ht2 = resz
      
      
      !! set water body pointer to res
      wbody => wet(j)
      wbody_wb => wet_wat_d(j)
      wbody_prm => wet_prm(j)
      

      !! initialize variables for wetland daily simulation
      hru(j)%water_seep = 0.
      wet_ob(j)%depth = wet(j)%flo / wsa1 / 1000. !m
      dep_init = wet_ob(j)%depth !m
      
      !! add precipitation - mm*ha*10.=m3 (used same area for infiltration and soil evap)
      wet_wat_d(j)%precip = w%precip * wsa1 !m3
      wet_ob(j)%depth = wet_ob(j)%depth + w%precip / 1000.  !m  Jaehak 2022
      wet(j)%flo =  wet(j)%flo + wet_wat_d(j)%precip !m3
      
      !! add irrigation water to the paddy/wetland storage 
      wet(j)%flo =  wet(j)%flo + irrig(j)%applied * wsa1 !m3
      wet(j)%no3 = wet(j)%no3 + irrig(j)%no3 * irrig(j)%applied * wsa1 * 0.001 !kg
      wet_wat_d(j)%area_ha = 0.
      if (wet(j)%flo > 0.) then  !paddy is assumed flat
        !! update wetland surface area - solve quadratic to find new depth
        x1 = wet_hyd(j)%bcoef ** 2 + 4. * wet_hyd(j)%ccoef * (1. - wet(j)%flo / (wet_ob(j)%pvol + 1.e-9))
        if (x1 < 1.e-6) then
          wet_h = 0.
        else
          wet_h1 = (-wet_hyd(j)%bcoef - sqrt(x1)) / (2. * wet_hyd(j)%ccoef + 1.e-9)
          wet_h = wet_h1 + wet_hyd(j)%bcoef
        end if
        wet_fr = (1. + wet_hyd(j)%acoef * wet_h)
        wet_fr = min(wet_fr,1.)
        wet_fr = max(wet_fr,0.01)
        
        wet_wat_d(j)%area_ha = hru(j)%area_ha * wet_fr

        !calculate seepage and groundwater interactions
        if(bsn_cc%gwflow == 1) then !rtb gwflow
          call gwflow_wetl(j)
        else !original seepage calculations
          !! infiltration of the standing water to the topsoil layer. 
          !! Any excess infiltration volume estimated here is reverted (back to waterbody) in swr_satexcess.
          wet_wat_d(j)%seep = min(wet(j)%flo, hru(j)%wet_hc * 24. * wsa1) !m3   
        end if !check for gwflow
        
        ! check potential percolation rate to refine daily seepage rate Jaehak 2022
        ! actual soil moisture content is updated in percmain
        volseep = wet_wat_d(j)%seep / wsa1 !mm
        if (volseep>0.1) then
          do j1 = 1, soil(j)%nly
            swst(j1) = soil(j)%phys(j1)%st + volseep
            if (swst(j1)>soil(j)%phys(j1)%ul*0.9) then !oversaturated Jaehak 2022
              volex = swst(j1) - soil(j)%phys(j1)%ul*0.9  !excess water. soil is assumed to remain saturated Jaehak 2022
              volseep = min(volex, soil(j)%phys(j1)%k*24.)
              swst(j1) = swst(j1) - volseep
            else
              volseep = 0
            endif
          end do
            
          ! move excess water upward to calculate daily seepage rate Jaehak 2022
          volex = 0
          do j1 = soil(j)%nly, 1, -1
            swst(j1) = swst(j1) + volex
            if (swst(j1)>soil(j)%phys(j1)%ul*0.9) then !oversaturated
              volex = max(0., swst(j1) - soil(j)%phys(j1)%ul*0.9)  !excess water. 
              swst(j1) = swst(j1) - volex                         !update soil water
            endif
          end do
            
          !update seepage volume
          wet_wat_d(j)%seep = max(0., wet_wat_d(j)%seep - volex * wsa1) !m3
        endif               
                
        wet(j)%flo = wet(j)%flo - wet_wat_d(j)%seep
        wet_wat_d(j)%area_ha = hru(j)%area_ha 
        hru(j)%water_seep = wet_wat_d(j)%seep / wsa1   !mm=m3/(10*ha)
      
        ! calculate dissolved nutrient infiltration Jaehak 2022
        if (wet_wat_d(j)%seep + wet(j)%flo > 0.01)then
          seep_rto = wet_wat_d(j)%seep / (wet_wat_d(j)%seep + wet(j)%flo)
        else 
          seep_rto = 0.
        endif
        soil1(j)%mn(1)%no3 = soil1(j)%mn(1)%no3 + wet(j)%no3 * seep_rto / hru(j)%area_ha !kg/ha
        soil1(j)%mn(1)%nh4 = soil1(j)%mn(1)%nh4 + wet(j)%nh3 * seep_rto / hru(j)%area_ha !kg/ha
        soil1(j)%mp(1)%act = soil1(j)%mp(1)%act + wet(j)%solp * seep_rto / hru(j)%area_ha !kg/ha
        soil1(j)%water(1)%n = soil1(j)%water(1)%n + wet(j)%orgn * seep_rto / hru(j)%area_ha !kg/ha
        soil1(j)%water(1)%p = soil1(j)%water(1)%p + wet(j)%sedp * seep_rto / hru(j)%area_ha !kg/ha
        
        ! nutrient seepage amount 
        wet_seep_day(j)%no3 = wet(j)%no3 * seep_rto !kg
        wet_seep_day(j)%nh3 = wet(j)%nh3 * seep_rto
        wet_seep_day(j)%orgn = wet(j)%orgn * seep_rto
        wet_seep_day(j)%solp = wet(j)%solp * seep_rto
        wet_seep_day(j)%sedp = wet(j)%sedp * seep_rto
        
        ! subtract the seepage amount from the ponding water
        wet(j)%no3 = wet(j)%no3 - wet_seep_day(j)%no3 
        wet(j)%nh3 = wet(j)%nh3 - wet_seep_day(j)%nh3 
        wet(j)%orgn = wet(j)%orgn - wet_seep_day(j)%orgn
        wet(j)%solp = wet(j)%solp - wet_seep_day(j)%solp
        wet(j)%sedp = wet(j)%sedp - wet_seep_day(j)%sedp
      end if 
        
      !! if not a floodplain wetland
      !if (hru(j)%wet_fp == "n") then
        !! calc release from decision table
        d_tbl => dtbl_res(irel)
        wbody => wet(j)
        wbody_wb => wet_wat_d(j)
        pvol_m3 = wet_ob(j)%pvol
        evol_m3 = wet_ob(j)%evol
        !if (wet_ob(j)%area_ha > 1.e-6) then
        if (hru(j)%area_ha > 1.e-6) then
          !dep = wbody%flo / wet_ob(j)%area_ha / 10000.     !m = m3 / ha / 10000m2/ha
          dep = wet(j)%flo / wsa1 / 1000.    !m 
        else
          dep = 0.
        end if
        weir_hgt = wet_ob(j)%weir_hgt   !m
        wet_ob(j)%depth = dep           !m

        !! weir discharge (ht2) by decision tables
        call conditions (j, irel)
        call res_hydro (j, irel, pvol_m3, evol_m3)
        
        if (hru(j)%area_ha > 1.e-6) then
          !dep = wbody%flo / wet_ob(j)%area_ha / 10000.     !m = m3 / ha / 10000m2/ha
          dep = wet(j)%flo / wsa1 / 1000.    !m 
        else
          dep = 0.
        end if
        
        !! subtract outflow from storage
        wet(j)%flo =  wet(j)%flo - ht2%flo
        surfq(j) = ht2%flo / wsa1 !mm
        
        if (time%step > 1) then
          do ii = 1, time%step
            !! daily total runoff
            hhsurfq(j,ii) = surfq(j) / real(time%step)
          end do
        end if
 
      
      wet_ob(j)%depth = wet(j)%flo / wsa1 / 1000. !m                       
       
      !! compute sediment deposition
      call res_sediment
      
      wet(j)%sed = wbody%sed !t
      
      !! perform reservoir nutrient balance
      call res_nutrient (j)
      
      wet(j)%no3 = wbody%no3  
      wet(j)%nh3 = wbody%nh3
      wet(j)%orgn =wbody%orgn
      wet(j)%sedp = wbody%sedp 
      wet(j)%solp = wbody%solp 
      !! perform salt ion constituent balance
      call wet_salt(icmd,j)
      
      !! perform wetland constituent balance
      icon = wet_dat(ires)%cs
      call wet_cs(icmd,icon,j)

      ! calculate sediment/nutrient yield when wetlands are flushed 
      if (dep_init<0.0001 .and. ht2%flo>0.) then 
        call ero_cfactor 
        qp_cms = bsn_prm%prf / 6578.6 * hru(j)%area_ha * surfq(j) / tconc(j) / 35.3
        cklsp(j) = usle_cfac(j) * hru(j)%lumv%usle_mult
        wet(j)%sed = (10. * surfq(j) * qp_cms * hru(j)%area_ha) ** .56 * cklsp(j) !tons
        sedyld(j) = wet(j)%sed * ht2%flo / (wet(j)%flo + ht2%flo)
        wet(j)%sed = max(0., wet(j)%sed - sedyld(j)) !tons
        ht2%sed = sedyld(j) !tons sediment yield
      endif  
        
      if (wet(j)%flo>0) then
        sedppm=wet(j)%sed/wet(j)%flo*1000000.
        no3ppm=wet(j)%no3/wet(j)%flo*1000.
      else
        sedppm=0
        no3ppm=0
      endif
      

  
      
      !! perform reservoir pesticide transformations
      !call res_pest (ires)

      !! set values for routing variables
      ob(icmd)%hd(1)%temp = 0.                  !!undefined

      !qdr(j) = ht2%flo / (10. * hru(j)%area_ha) + ht1%flo * bypass
      sedyld(j) = ht2%sed + sedyld(j) * bypass
      sanyld(j) = ht2%san + sanyld(j) * bypass
      silyld(j) = ht2%sil + silyld(j) * bypass
      clayld(j) = ht2%cla + clayld(j) * bypass 
      sagyld(j) = ht2%sag + sagyld(j) * bypass
      lagyld(j) = ht2%lag + lagyld(j) * bypass
      grayld(j) = ht2%grv + grayld(j) * bypass


      sedorgn(j) = ht2%orgn / hru(j)%area_ha + sedorgn(j) * bypass
      sedorgp(j) = ht2%sedp / hru(j)%area_ha + sedorgp(j) * bypass
      surqno3(j) = ht2%no3/ hru(j)%area_ha  + surqno3(j) * bypass
      !nh3 = resnh3o + 0.  !add ammonium 
      !no2  = resno2o + 0.  !add no2
      sedminps(j) = ht2%solp / hru(j)%area_ha / 2. + sedminps(j) * bypass
      sedminpa(j) = ht2%solp / hru(j)%area_ha / 2. + sedminpa(j) * bypass
      
      !! set inflow and outflow variables for reservoir_output
      if (time%yrs > pco%nyskip) then
        wet_in_d(j) = wet_in_d(j) + ht1 
        wet_out_d(j) = ht2
      end if  

      return
      end subroutine wetland_control