      subroutine nut_nrain
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine adds nitrate from rainfall to the soil profile
!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use basin_module
      use organic_mineral_mass_module
      use hydrograph_module
      use hru_module, only : hru, ihru 
      use climate_module
      use output_landscape_module
      
      implicit none

      integer :: iadep            !            |
      integer :: j                !none        |counter
      integer :: iob              !            |
      integer :: ist              !            |
      real :: const               !            |constant used for rate, days, etc

      j = ihru
      iob = hru(j)%obj_no
      iwst = ob(iob)%wst
      iadep = wst(iwst)%wco%atmodep
      ist = atmodep_cont%ts
        
      !! calculate nitrogen in precipitation - mg/l *mm -> kg/ha
      !! (mg/l*mm) * kg/1,000,000 mg *1,00 l/m3 * m3/1,000 mm * 10,000 m2/ha = 0.01
      if (ist > 0 .and. ist <= atmodep_cont%num) then
        if (atmodep_cont%timestep == "mo") then
          const = float (ndays(time%mo + 1) - ndays(time%mo))
          hnb_d(j)%no3atmo = .01 * atmodep(iadep)%no3_rfmo(ist) * w%precip + atmodep(iadep)%no3_drymo(ist) / const
          soil1(j)%mn(1)%no3 = hnb_d(j)%no3atmo + soil1(j)%mn(1)%no3
          hnb_d(j)%nh4atmo = .01 * atmodep(iadep)%nh4_rfmo(ist) * w%precip + atmodep(iadep)%nh4_drymo(ist) / const
          soil1(j)%mn(1)%nh4 = soil1(j)%mn(1)%nh4 + hnb_d(j)%nh4atmo
        end if 
        if (atmodep_cont%timestep == "yr") then
          hnb_d(j)%no3atmo = .01 * atmodep(iadep)%no3_rfyr(ist) * w%precip + atmodep(iadep)%no3_dryyr(ist) / 365.
          soil1(j)%mn(1)%no3 = hnb_d(j)%no3atmo + soil1(j)%mn(1)%no3
          hnb_d(j)%nh4atmo = .01 * atmodep(iadep)%nh4_rfyr(ist) * w%precip + atmodep(iadep)%nh4_dryyr(ist) / 365.
          soil1(j)%mn(1)%nh4 = soil1(j)%mn(1)%nh4 + hnb_d(j)%nh4atmo
        endif
      end if
      if (atmodep_cont%timestep == "aa") then
        hnb_d(j)%no3atmo = .01 * atmodep(iadep)%no3_rf * w%precip + atmodep(iadep)%no3_dry / 365.
        soil1(j)%mn(1)%no3 = hnb_d(j)%no3atmo + soil1(j)%mn(1)%no3
        hnb_d(j)%nh4atmo = .01 * atmodep(iadep)%nh4_rf * w%precip + atmodep(iadep)%nh4_dry / 365.
        soil1(j)%mn(1)%nh4 = soil1(j)%mn(1)%nh4 + hnb_d(j)%nh4atmo
      endif
       
      return
      end subroutine nut_nrain