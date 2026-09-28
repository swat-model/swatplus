      subroutine wallo_transfer (ipou)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine transfers water from point of diversion to place of use
!!    by pipe or pump

      use water_allocation_module
      use hydrograph_module
      use constituent_mass_module
      use sd_channel_module
      use aquifer_module
      use reservoir_module
      use time_module
      
      implicit none 

      integer, intent (in):: ipou       !place of use (POU) number
      integer :: ipod = 0               !point of diversion (POD) number
      integer :: iconv = 0              !conveyance object number (pipe or pump number)

      !! transfer water to receiving object from each source
      do ipod = 1, pou(ipou)%pod(ipod)%num
        iconv = pou(ipou)%pod(ipod)%conv_num
        select case (pou(ipou)%pod(ipod)%conv_typ)
        case ("pipe")
          !! organic hydrograph being transfered from the source to the receiving object
          poud_om(ipou)%pod(ipod) = (1. - pipe(iconv)%loss_fr) * poud_om(ipou)%pod(ipod) 
          !! add to aquifers
        case ("pump")
          !! include pump losses here
        end select
      end do
      return
      end subroutine wallo_transfer