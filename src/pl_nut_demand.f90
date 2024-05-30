      subroutine pl_nut_demand

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine predicts daily potential growth of total plant
!!    biomass and roots and calculates leaf area index. Incorporates
!!    residue for tillage functions and decays residue on ground
!!    surface. Adjusts daily dry matter based on water stress.

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    phubase(:)  |heat units    |base zero total heat units (used when no
!!                               |land cover is growing)
!!    phutot(:)   |heat units    |total potential heat units for year (used
!!                               |when no crop is growing)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    phubase(:)  |heat units    |base zero total heat units (used when no
!!                               |land cover is growing)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max
!!    SWAT: operatn, swu, grow

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use hru_module, only : ihru, ipl, uapd, uapd_tot, uno3d, uno3d_tot, sum_no3, sum_solp
      use soil_module
      use plant_module
      use plant_data_module
      use organic_mineral_mass_module
      use climate_module

      implicit none      

      integer :: j              !none          |HRU number
      integer :: nly            !none          |soil layer number
      integer :: idp            !none          |plant data base number (plants.plt)
      real :: delg              !              |
      real :: delg_p            !              |

      j = ihru
      
      uno3d(ipl) = 0.
      uno3d_tot = 0.
      uapd(ipl) = 0.
      uapd_tot = 0.
      do ipl = 1, pcom(j)%npl
        idp = pcom(j)%plcur(ipl)%idplt
        if (pcom(j)%plcur(ipl)%idorm == 'n'.and.pcom(j)%plcur(ipl)%gro=="y")    &
                                                                   then
        !! update accumulated heat units for the plant
        delg = 0.
        if (pcom(j)%plcur(ipl)%phumat > 0.1) then
          delg = (w%tave - pldb(idp)%t_base) / pcom(j)%plcur(ipl)%phumat
          delg_p = (w%tave - pldb(idp)%t_base) / pcom(j)%plcur(ipl)%phumat_p
        end if
        if (delg < 0.) delg = 0.
        if (delg_p < 0.) delg_p = 0.
        pcom(j)%plcur(ipl)%phuacc = pcom(j)%plcur(ipl)%phuacc + delg  
        pcom(j)%plcur(ipl)%phuacc_p = pcom(j)%plcur(ipl)%phuacc_p + delg_p  
        call pl_nupd
        call pl_pupd
        uno3d_tot = uno3d_tot + uno3d(ipl)
        uapd_tot = uapd_tot + uapd(ipl)
        end if
      end do
      sum_no3 = 0.
      sum_solp = 0.
      do nly = 1, soil(j)%nly
        sum_no3 = sum_no3 + soil1(j)%mn(nly)%no3
        sum_solp = sum_solp + soil1(j)%mp(nly)%lab
      end do

      return
      
      end subroutine pl_nut_demand