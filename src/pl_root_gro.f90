      subroutine pl_root_gro(j)
      
      use plant_data_module
      use basin_module
      use hru_module, only : ipl
      use plant_module
      use carbon_module
      use organic_mineral_mass_module
      use soil_module
      
      implicit none 
      
      integer, intent (in) :: j     !none               |HRU number
      integer :: idp                !                   |
      real :: rto                   !none               |ratio of current years of growth:years to maturity of perennial
      real :: rto1                  !none               |ratio of current years + 1 of growth:years to maturity of perennial
      real :: rto2                  !none               |ratio of 1 year:years to maturity of perennial
      real :: phumax
      real :: rdmax_yr
             
      idp = pcom(j)%plcur(ipl)%idplt

      !! calculate root depth
      if (pldb(idp)%typ == "warm_annual" .or. pldb(idp)%typ == "cold_annual" .or.  &
             pldb(idp)%typ == "warm_annual_tuber" .or. pldb(idp)%typ == "cold_annual_tuber") then
        pcom(j)%plg(ipl)%root_dep = 2.5 * pcom(j)%plcur(ipl)%phuacc * 1000. * pldb(idp)%rdmx
      else
        pcom(j)%plg(ipl)%root_dep = 2.5 * pcom(j)%plcur(ipl)%phuacc_p * 1000. * pldb(idp)%rdmx
      end if
      if (pcom(j)%plg(ipl)%root_dep > soil(j)%zmx) pcom(j)%plg(ipl)%root_dep = soil(j)%zmx
      if (pcom(j)%plg(ipl)%root_dep < 25.4) pcom(j)%plg(ipl)%root_dep = 25.4

      !! calculate total root mass
      if (pldb(idp)%typ == "perennial") then 

        !! assume tree reaches final root:shoot ratio at 0.2 * years to maturity
        if (pldb(idp)%mat_yrs > 0) then
          rto = float (pcom(j)%plcur(ipl)%curyr_mat) / float (pldb(idp)%mat_yrs)
          if (rto < 0.2) then
            pcom(j)%plg(ipl)%root_frac = pldb(idp)%rsr1 - (pldb(idp)%rsr1 - pldb(idp)%rsr2) * rto / .2
          else
            pcom(j)%plg(ipl)%root_frac = pldb(idp)%rsr2
          end if
        else
          pcom(j)%plg(ipl)%root_frac = pldb(idp)%rsr2
        end if
      else    !!annuals
        !! calculate fraction of total biomass that is in the roots for annuals
        phumax = amin1 (1., pcom(j)%plcur(ipl)%phuacc)
        pcom(j)%plg(ipl)%root_frac = pldb(idp)%rsr1 - (pldb(idp)%rsr1 - pldb(idp)%rsr2) * phumax
      end if
      
      !! root mass
      pl_mass(j)%root(ipl)%m = pcom(j)%plg(ipl)%root_frac * pl_mass(j)%tot(ipl)%m

      return
      end subroutine pl_root_gro