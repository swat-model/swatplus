      subroutine rls_routesurf (iob, tile_fr_surf)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!
!!    ht1== deposition: write to deposition.out
!!    ht2== outflow from inflow: added to hru generated flows

      use hru_module, only : hru, ihru, usle_cfac, ls_overq, precip_eff
      use hydrograph_module
      use climate_module, only : w
      
      implicit none
      
      integer :: j                      !       |
      integer :: iob                    !       |
      integer :: ifield                 !       |
      real, intent (in)  :: tile_fr_surf    !m3     |overland tile flow
      real :: sed                       !       |
      real :: trancap                   !       |

      j = ihru
      ifield = hru(j)%dbs%field

      !! compute infiltration from surface runon and tile flow to next landscape unit
      ls_overq = ob(iob)%hin_sur%flo + (ob(iob)%hin_til%flo * tile_fr_surf) / (10. * hru(j)%area_ha)   ! m3/10*ha = mm
      precip_eff = precip_eff + ls_overq
      
      !! add surface runon to subdaily effective precip
      if (time%step > 1) then
          w%ts(:) = w%ts(:) + ls_overq / time%step
      end if
      
!!    compute infiltration from surface runon to next landscape unit
      !ls_overq = ob(iob)%hin%flo    !/ (10. * hru(j)%area_ha) 
      !if (ls_overq > 1.e-6) then
      !  qs = ls_overq / 24.   !mm/hr
      !  vs = (qs ** .4) * (hru(j)%topo%slope ** .3) / (hru(j)%luse%ovn ** .6)
      !  if (vs > 1.e-6) then
      !    trt = hru(j)%topo%slope_len / (3600. * vs)
      !    inflrout = soil(j)%phys(1)%k * trt
      !    inflrout = Min (inflrout, ls_overq)
      !  else
      !    inflrout = ls_overq
      !  end if
      !  ht1%flo = inflrout
      !  ht2%flo = ls_overq - inflrout
      !end if
      
!!    sediment deposition across the landscape
      sed = ob(iob)%hin_sur%sed / hru(j)%area_ha
      !! use surface runoff (mm) for eiq - m3/(10 * ha) = mm
      trancap = hru(j)%topo%dep_co * usle_cfac(j) * ls_overq *        &
                hru(j)%topo%slope**1.4 * hru(j)%field%wid**1.4
      if (sed > trancap) then
        ht1%sed = (sed - trancap) * hru(j)%area_ha
        ht2%sed = trancap * hru(j)%area_ha
      else
        ht1%sed = 0.
        ht2%sed = sed * hru(j)%area_ha
      end if
      
!!    organic nitrogen
!      orgn = hd(ihout)%orgn 
!      cy = hd(ihout)%sed  / (hd(ihout)%flo  + 1.e-6)
!      if (cy > .01) then
!        enratio = .78 * cy ** (-.2468)
!      else
!        enratio = 3.
!      end if
!      enratio = Min(enratio,3.)
!      dr_er = drls * enratio
!      dr_er = Min(dr_er,1.)
!      hd(ihout)%orgn = orgn * dr_er
      
!!    organic phosphorus
!      orgp = hd(ihout)%sedp 
!      hd(ihout)%sedp = orgp * dr_er
      
!!    nitrate (& nitrite)
!      no3 = (hd(ihout)%no3 + hd(ihout)%no2)
!!    soluble phosphorus
!      slbp = hd(ihout)%solp 
!!    soluble pesticide not routed
!!    sorbed pesticide not routed
!!    chlorophyll-a not routed
!!    ammonium
!      nh3 = hd(ihout)%nh3 
!!    CBOD not routed
!!    dissolved oxygen not routed
!!    persistent bacteria not routed
!!    less persistent bacteria not routed

      return
      end subroutine rls_routesurf