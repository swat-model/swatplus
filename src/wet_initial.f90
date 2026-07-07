      subroutine wet_initial
      
      use reservoir_module
      use reservoir_data_module
      use reservoir_data_module
      use hydrograph_module
      use hru_module, only : hru, ihru
      use maximum_data_module
      use water_body_module
      
      implicit none
      
      integer :: iprop          !              | 
      integer :: ihyd           !none          |counter 
      integer :: init           !              |
      real :: cnv               !none          |conversion factor (mm => m^3)
      real :: x1                !              |
      real :: wet_h             !              |
      real :: wet_h1            !              | 
      real :: wet_fr            !              | 
      integer :: init_om
  
      do ihru = 1, sp_ob%hru
        !! set initial volumes and convert units
        iprop = hru(ihru)%dbs%surf_stor
        if (iprop > 0) then
          ihyd = wet_dat(iprop)%hyd
          !! ha*mm*10. => m**3  - assume entire hru is wet and don't use fractional inputs (for simplicity)
          wet_ob(ihru)%evol = hru(ihru)%area_ha * wet_hyd(ihyd)%edep * 10.  ! * wet_hyd(ihyd)%esa
          wet_ob(ihru)%pvol = hru(ihru)%area_ha * wet_hyd(ihyd)%pdep * 10.  ! * wet_hyd(ihyd)%psa
          wet_ob(ihru)%psa = wet_hyd(ihyd)%psa * hru(ihru)%area_ha 
          wet_ob(ihru)%esa = wet_hyd(ihyd)%esa * hru(ihru)%area_ha 

          !!set initial n and p concentrations --> (ppm) * (m^3) / 1000 = kg    !! ppm = t/m^3 * 10^6
          init = wet_dat(iprop)%init
          init_om = wet_init(init)%org_min
          cnv = om_init_water(init_om)%flo * wet_ob(ihru)%pvol / 1000.
          wet(ihru) = cnv * om_init_water(init_om)
          wet(ihru)%flo = om_init_water(init_om)%flo * wet_ob(ihru)%pvol
          wet_om_init(ihru) = wet(ihru)

          !! update surface area
          !! wetland on hru - solve quadratic to find new depth
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
            wet_wat_d(ihru)%area_ha = hru(ihru)%area_ha * wet_hyd(ihyd)%psa * wet_fr
          end if 

        end if
      
      end do
      close(105)

      return
      end subroutine wet_initial