      subroutine wetland_control
    
      use reservoir_data_module
      use reservoir_module
      use hru_module, only : hru, sedyld, sanyld, silyld, clayld, sagyld, lagyld, grayld, sedminps, sedminpa,   &
        surqno3, sedorgn, sedorgp, qdr, ihru, pet_day, qday
      use conditional_module
      use climate_module
      use hydrograph_module
      use time_module
      use basin_module
      use channel_module
      use water_body_module
      
      implicit none
     
      real :: bypass = 1.             !              | 
      real :: fracwet                 !              | 
      integer :: j                    !none          |counter
      integer :: iprop                !              |  
      integer :: iac                  !none          |counter
      character(len=1) :: action           !         |
      integer :: ial                  !none          |counter
      real :: b_lo                    !              |
      real :: res_h                   !              |
      real :: x1                      !              |
      real :: wet_h                   !              |
      real :: wet_h1                  !              |
      real :: flwi                    !m^3 H2O       |water entering pothole on day  
      real :: flwo                    !              |
      real :: sedi                    !metric tons   |sediment entering pothole on day
      real :: sedo                    !metric tons   |sed leaving res 
      integer :: k                    !              | 
      integer :: ii                   !none          |counter 
      integer :: jres                 !none          |reservoir number
      integer :: idat                 !              |
      integer :: ihyd                 !none          |counter
      integer :: ised                 !none          |counter
      integer :: irel                 !              |
      integer :: inut                 !none          |counter
      integer :: ipst                 !none          |counter
      integer :: ires = 0
      real :: wet_fr = 0.
      real :: pvol_m3
      real :: evol_m3

      j = ihru
      ires= hru(j)%dbs%surf_stor
      ihyd = wet_dat(ires)%hyd
      ised = wet_dat(ires)%sed
      irel = wet_dat(ires)%release

      !! initialize variables for wetland daily simulation
      hru(ihru)%water_seep = 0.

      !! add precipitation - mm*ha*10.=m3 (used same area for infiltration and soil evap)
      wet_wat_d(ihru)%precip = w%precip * wet_wat_d(ihru)%area_ha * 10.
      wet(ihru)%flo =  wet(ihru)%flo + wet_wat_d(ihru)%precip
      
      !! subtract evaporation and seepage - mm*ha*10.=m3
      !wet_wat_d(ihru)%evap = pet_day * wet_hyd(ihyd)%evrsv * wet_wat_d(ihru)%area_ha * 10.
      !wet_wat_d(ihru)%evap = min(wet_wat_d(ihru)%evap, wet(ihru)%flo)
      !wet(ihru)%flo =  wet(ihru)%flo - wet_wat_d(ihru)%evap
      !hru(ihru)%water_evap = wet_wat_d(ihru)%evap / (10. * hru(ihru)%area_ha)
        
      !! save hru(ihru)%water_seep to add to infiltration on next day
      wet_wat_d(ihru)%seep = wet_wat_d(ihru)%area_ha * wet_hyd(ihyd)%k * 10.* 24.
      wet_wat_d(ihru)%seep = min(wet(ihru)%flo, wet_wat_d(ihru)%seep)
      wet(ihru)%flo = wet(ihru)%flo - wet_wat_d(ihru)%seep
      hru(ihru)%water_seep = wet_wat_d(ihru)%seep / (10. * hru(ihru)%area_ha)
        
      !! if not a floodplain wetland
      if (hru(ihru)%wet_fp == "n") then
        !! calc release from decision table
        d_tbl => dtbl_res(irel)
        wbody => wet(ihru)
        wbody_wb => wet_wat_d(ihru)
        pvol_m3 = wet_ob(ihru)%pvol
        evol_m3 = wet_ob(ihru)%evol
        call conditions (ihru, irel)
        call res_hydro (ihru, irel, ihyd, pvol_m3, evol_m3)
      
        !! subtract outflow from storage
        wet(ihru)%flo =  wet(ihru)%flo - ht2%flo
      end if

      !! update surface area - solve quadratic to find new depth
      wet_wat_d(ihru)%area_ha = 0.
      if (wet(ihru)%flo > 0.) then
        x1 = wet_hyd(ihyd)%bcoef ** 2 + 4. * wet_hyd(ihyd)%ccoef * (1. - wet(ihru)%flo / wet_ob(ihru)%pvol)
        if (x1 < 1.e-6) then
          wet_h = 0.
        else
          wet_h1 = (-wet_hyd(ihyd)%bcoef - sqrt(x1)) / (2. * wet_hyd(ihyd)%ccoef)
          wet_h = wet_h1 + wet_hyd(ihyd)%bcoef
        end if
        wet_fr = (1. + wet_hyd(ihyd)%acoef * wet_h)
        wet_fr = min(wet_fr,1.)
        wet_wat_d(ihru)%area_ha = hru(ihru)%area_ha * wet_fr
      end if 
 
      !! compute sediment deposition
      call res_sediment (ihru, ihyd, ised)
      
      !! subtract sediment leaving from reservoir
      wet(ihru)%sed = wet(ihru)%sed - ht2%sed
      wet(ihru)%sil = wet(ihru)%sil - ht2%sil
      wet(ihru)%cla = wet(ihru)%cla - ht2%cla
          
      !! perform reservoir nutrient balance
      inut = wet_dat(ires)%nut
      call res_nutrient (ires, inut, ihru)

      !! perform reservoir pesticide transformations
      ipst = wet_dat(ires)%pst
      !call res_pest (ires)

      !! set values for routing variables
      ob(icmd)%hd(1)%temp = 0.                  !!undefined

      qdr(ihru) = ht2%flo / (10. * hru(ihru)%area_ha) + ht1%flo * bypass
      sedyld(ihru) = ht2%sed / hru(ihru)%area_ha + sedyld(ihru) * bypass
      sanyld(ihru) = ht2%san / hru(ihru)%area_ha + sanyld(ihru) * bypass
      silyld(ihru) = ht2%sil / hru(ihru)%area_ha + silyld(ihru) * bypass
	  clayld(ihru) = ht2%cla / hru(ihru)%area_ha + clayld(ihru) * bypass 
	  sagyld(ihru) = ht2%sag / hru(ihru)%area_ha + sagyld(ihru) * bypass
	  lagyld(ihru) = ht2%lag / hru(ihru)%area_ha + lagyld(ihru) * bypass
	  grayld(ihru) = ht2%grv / hru(ihru)%area_ha + grayld(ihru) * bypass

      sedorgn(ihru) = ht2%orgn / hru(ihru)%area_ha + sedorgn(ihru) * bypass
      sedorgp(ihru) = ht2%sedp / hru(ihru)%area_ha + sedorgp(ihru) * bypass
      surqno3(ihru) = ht2%no3/ hru(ihru)%area_ha  + surqno3(ihru) * bypass
      !nh3 = resnh3o + 0.  !add ammonium 
      !no2  = resno2o + 0.  !add no2
      sedminps(ihru) = ht2%solp / hru(ihru)%area_ha / 2. + sedminps(ihru) * bypass
      sedminpa(ihru) = ht2%solp / hru(ihru)%area_ha / 2. + sedminpa(ihru) * bypass
      
      !! set inflow and outflow variables for reservoir_output
      if (time%yrs > pco%nyskip) then
        wet_in_d(ihru) = ht1 
        wet_out_d(ihru) = ht2
        !wet_in_d(ihru)%flo = wet(ihru)%flo / 10000.   !m^3 -> ha-m
        !wet_out_d(ihru)%flo = wet(ihru)%flo / 10000.  !m^3 -> ha-m
      end if  

      return
      end subroutine wetland_control