      subroutine res_weir_release (jres, id, ihyd, evol_m3, dep, weir_hgt)

      use reservoir_data_module
      use reservoir_module
      use conditional_module
      use climate_module
      use time_module
      use hydrograph_module
      use water_body_module
      use soil_module
      use hru_module
      use water_allocation_module
      use basin_module
      
      implicit none
      
      real,  intent (in) :: evol_m3
      real,  intent (in) :: dep       !m 
      real,  intent (in) :: weir_hgt  !m         |height of weir overflow crest from reservoir bottom
      integer,  intent (in) :: jres             !none      |hru number
      integer :: iweir = 0          !none      |weir ID 
      integer :: nstep = 0          !none      |counter
      integer :: tstep = 0          !none      |hru number
      integer :: iac = 0            !none      |counter 
      integer :: ic = 0             !none      |counter
      integer :: weir_flg=0         !none      |counter
      integer,  intent (in) :: id   !none      |hru number
      integer,  intent (in) :: ihyd !          |
      real :: vol = 0.              !          |
      real :: res_h = 0.            !m         |water depth
      real :: wsa1 = 0.             !m2        |water surface area 
      real :: qout = 0.             !m3        |weir discharge during short time step
      real :: hgt_above = 0.        !m         |height of water above the above bottom of weir
      real :: vol_above = 0.             !m3        |water volume above the bottom of weir !Jaehak 2024
      
      !! store initial values
      vol = wbody%flo
      nstep = 1
      iweir = wet_ob(jres)%iweir
      vol_above = 0 !water storage above weir height
      
      if (wet_hyd(ihyd)%name=='paddy') then
        !paddy
        wsa1 = hru(jres)%area_ha * 10000. 
      else
        !wetland
        wsa1 = wbody_wb%area_ha * 10000. !m2
      endif
      
      hgt_above = max(0., dep - weir_hgt)  !m ponding depth above weir crest  
      !sto_max = wsa1 * weir_hgt !m3
      
      !if (vol > sto_max) then
      !  ht2%flo = vol - sto_max
      !  vol = sto_max
      !endif
      !write(*,'(10f10.1)') w%precip,vol/wsa1*1000,ht2%flo/wsa1*1000,hru(jres)%water_seep,soil(jres)%sw
      !! check if reservoir decision table has a weir discharge command
     ! do iac = 1, dtbl_res(id)%acts
     !   if (dtbl_res(id)%act(iac)%option == "weir") then
     !     weir_flg = 1
     !     exit
     !   endif
     ! end do
      
      do tstep = 1, nstep
          
        !! calculate weir discharge from scheduled management
        if (hgt_above > 0 .and. iweir > 0) then
          !emergency spillway discharge Jaehak 2023
          if (vol>evol_m3) then
            ht2%flo = ht2%flo + (wbody%flo - evol_m3) 
            ht2%flo = max(0.,ht2%flo)
            vol = evol_m3
            res_h = vol / wsa1 !m
            hgt_above = max(0.,res_h - weir_hgt)  !m 
          endif
          vol_above = hgt_above * wsa1 !m3

          if (nstep>1) then !revised by Jaehak 2023
            qout = res_weir(iweir)%c * res_weir(iweir)%w * hgt_above ** res_weir(iweir)%k !m3/s
            qout = max(0.,86400. / nstep * qout) !m3
            if (qout > vol_above) then
              ht2%flo = ht2%flo + vol_above !weir discharge volume for the day, m3
              vol = vol - vol_above
              vol_above = 0.
            else
              ht2%flo = ht2%flo + qout 
              vol = vol - qout
              vol_above = vol_above - qout
            end if
            res_h = vol / wsa1 !m
            hgt_above = max(0.,res_h - weir_hgt)  !m Jaehak 2022
            if (vol_above<=0.001.or.hgt_above<=0.0001) exit

          else
            do ic = 1, 24
              qout = res_weir(iweir)%c * res_weir(iweir)%w * hgt_above ** res_weir(iweir)%k !m3/s
              qout = 3600. * qout !m3
              if (qout > vol_above) then
                ht2%flo = ht2%flo + vol_above !weir discharge volume for the day, m3
                vol = vol - vol_above
                vol_above = 0.
              else
                ht2%flo = ht2%flo + qout 
                vol = vol - qout
                vol_above = vol_above - qout
              end if
                          
              if (wsa1 > 1.e-6) then
                res_h = vol / wsa1 !m
              else
                res_h = 0.
              end if
              hgt_above = max(0.,res_h - weir_hgt)  !m Jaehak 2022
              if (vol_above<=0.001.or.hgt_above<=0.0001) exit
            end do
          endif
        else
          ht2%flo = 0.
        endif
        wbody%flo = vol !m3
      end do  

      return
      end subroutine res_weir_release