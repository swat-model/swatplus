      subroutine pl_seed_gro(j)
      
      use plant_data_module
      use basin_module
      use hru_module, only : ep_day,  ipl, pet_day
      use plant_module
      use carbon_module
      use organic_mineral_mass_module
      use climate_module
      USE hydrograph_module
      
      implicit none 
      
      integer, intent (in) :: j     !none               |HRU number
      integer :: idp                !                   |
      real :: ajhi                  !
      real :: ajhi_min              !
      real :: dhi                   !
      real :: temp_dif              !
      real :: temp_adj              !
      real :: etr                   !%          |plant uptake/PET 
      integer :: xyz
      
      idp = pcom(j)%plcur(ipl)%idplt
      iwst = ob(j)%wst

      ajhi = pcom(j)%plcur(ipl)%harv_idx * 100. * pcom(j)%plcur(ipl)%phuacc /          &
                (100. * pcom(j)%plcur(ipl)%phuacc + Exp(11.1 - 10. * pcom(j)%plcur(ipl)%phuacc))
            
      !! calculate plant ET values when heat units exceed 0.5
      if (pcom(j)%plcur(ipl)%phuacc > 0.5) then  ! .and. pcom(j)%plcur(ipl)%phuacc < pldb(idp)%dlai) then 
        pcom(j)%plg(ipl)%plet = pcom(j)%plg(ipl)%plet + ep_day !+ es_day
        pcom(j)%plg(ipl)%plpet = pcom(j)%plg(ipl)%plpet + pet_day
      
        !! adjust harvest index for water stress
        if (pcom(j)%plg(ipl)%plpet > 1.e-6) then
          etr = 100. * pcom(j)%plg(ipl)%plet / pcom(j)%plg(ipl)%plpet
          ajhi_min = ajhi / 2.
          ajhi = (ajhi - ajhi_min) * (etr / (etr + Exp(6.13 - .0883 * etr))) + ajhi_min
          if (ajhi >  pldb(idp)%hvsti) then
            ajhi = pldb(idp)%hvsti
          end if
        else
          etr = 1.
        end if
                     
        if (j == 985) then
          xyz = 0
        end if
                   
        !! calc daily change in hi
        dhi = ajhi - pcom(j)%plg(ipl)%hi_prev
      
        !! adjust harvest index for water stress
        etr = etr / (etr + Exp(6.13 - .0883 * etr))
        etr = amin1 (1., etr)
        etr = max (0., etr)
        
        !! adjust harvest index for temperature stress
        temp_dif = pldb(idp)%t_opt - wst(iwst)%weat%tave
        if (temp_dif < 0. .and. pcom(j)%plcur(ipl)%phuacc > 0.7) then
          temp_adj = Exp (8. * temp_dif / pldb(idp)%t_opt)    ! 8 -> 6
          temp_adj = amin1 (1., temp_adj)
          temp_adj = max (0., temp_adj)
        else
          temp_adj = 1.
        end if
      
        pcom(j)%plg(ipl)%hi_adj = pcom(j)%plg(ipl)%hi_adj + dhi !* temp_adj  ! * etr
        pcom(j)%plg(ipl)%hi_adj = amin1 (pldb(idp)%hvsti, pcom(j)%plg(ipl)%hi_adj)
        pcom(j)%plg(ipl)%hi_adj = max (0., pcom(j)%plg(ipl)%hi_adj)
        pcom(j)%plg(ipl)%hi_prev = ajhi
      
      else
        pcom(j)%plg(ipl)%hi_prev = 0.
      end if

      return
      end subroutine pl_seed_gro