      subroutine pl_mortality
      
      use plant_data_module
      use basin_module
      use hru_module, only : hru, uapd, uno3d, par, bioday, ep_day, es_day,              &
         ihru, ipl, pet_day, rto_no3, rto_solp, sum_no3, sum_solp, uapd_tot, uno3d_tot, vpd
      use plant_module
      use carbon_module
      use organic_mineral_mass_module
      
      implicit none 
      
      integer :: j              !none               |HRU number
      integer :: idp            !                   |
      real :: biomxyr
      real :: rto
      real :: rto1
               
      j = ihru
      idp = pcom(j)%plcur(ipl)%idplt
      
      !keep biomass below maximum - excess to residue (need to include c, n and p adjustments)
      biomxyr = pldb(idp)%bmx_peren * 1000.  !t/ha -> kg/ha
      if (biomxyr > 1.e-6 .and. pl_mass(j)%ab_gr(ipl)%m > biomxyr) then
          
        !! partition all plant components by above ground ratio
        rto = biomxyr / pl_mass(j)%ab_gr(ipl)%m
        rto = amin1 (1., rto)
        pl_mass(j)%tot(ipl) = rto * pl_mass(j)%tot(ipl)
        pl_mass(j)%ab_gr(ipl) = rto * pl_mass(j)%ab_gr(ipl)
        pl_mass(j)%leaf(ipl) = rto * pl_mass(j)%leaf(ipl)
        pl_mass(j)%stem(ipl) = rto * pl_mass(j)%stem(ipl)
        pl_mass(j)%seed(ipl) = rto * pl_mass(j)%seed(ipl)
        pl_mass(j)%root(ipl) = rto * pl_mass(j)%root(ipl)
        
        !! add dead material to residue
        rto1 = 1. - rto
        rto1 = amax1 (0., rto1)
        rsd1(j)%tot(ipl) = rsd1(j)%tot(ipl) + rto1 * pl_mass(j)%tot(ipl)
      end if

      return
      end subroutine pl_mortality