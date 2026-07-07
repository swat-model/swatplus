      subroutine pl_dormant

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine checks the dormant status of the different plant types

      use climate_module
      use hydrograph_module
      use plant_data_module
      use organic_mineral_mass_module
      use hru_module, only : hru, dormhr, ipl, ihru
      use plant_module

      implicit none

      real :: resnew                !              |
      real :: resnew_n              !              |
      integer :: j                  !none          |HRU number
      integer :: idp                !              |
      integer :: iob                !              |
      integer :: iwgn               !              |

      j = ihru
      idp = pcom(j)%plcur(ipl)%idplt
      iob = hru(j)%obj_no
      iwst = ob(iob)%wst
      iwgn = wst(iwst)%wco%wgn

      !! check for beginning of dormant season
      if (pcom(j)%plcur(ipl)%idorm == "n" .and. wst(iwst)%weat%daylength - dormhr(j) < wgn_pms(iwgn)%daylmn) then

        !! beginning of temperature based perennial dormant period - leaf drop
        if (pldb(idp)%typ == "perennial") then
          pcom(j)%plcur(ipl)%idorm = "y"
          !! add leaf mass to residue pool
          rsd1(j)%tot(1) = pl_mass(j)%leaf(ipl) + rsd1(j)%tot(1)
        end if

        !! beginning of temperature based perennial dormant period - mortality
        if (pldb(idp)%typ == "perennial") then
          pcom(j)%plcur(ipl)%idorm = "y"
          !! add stem mass to residue pool
          rsd1(j)%tot(1) = pl_mass(j)%stem(ipl) + rsd1(j)%tot(1)
        end if

        !! beginning of cool season annual dormant period
        if (pldb(idp)%typ == "cold_annual") then
          if (pcom(j)%plcur(ipl)%phuacc < 0.75) then
            pcom(j)%plcur(ipl)%idorm = "y"
            pcom(j)%plstr(ipl)%strsw = 1.
          end if 
        end if
      end if

      !! check if end of dormant period
      if (pcom(j)%plcur(ipl)%idorm == "y" .and. wst(iwst)%weat%daylength - dormhr(j) >=   &
                                                                wgn_pms(iwgn)%daylmn) then
        if (pldb(idp)%typ == "perennial") then
          !! end of perennial dormant period
          pcom(j)%plcur(ipl)%idorm = "n"
          pcom(j)%plcur(ipl)%phuacc = 0.
        end if

        !! end of cool season annual dormant period
        if (pldb(idp)%typ == "cold_annual") then
          pcom(j)%plcur(ipl)%idorm = "n"
          pcom(j)%plcur(ipl)%phuacc = 0.
        end if

      end if

      return
      end subroutine pl_dormant