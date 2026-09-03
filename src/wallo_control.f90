      subroutine wallo_control (ipod)
      
      use water_allocation_module
      use hydrograph_module
      use constituent_mass_module
      use maximum_data_module
      
      implicit none 

      integer, intent (inout) :: ipod       !point of diversion number
      integer :: ipou                       !place of use number
      integer :: ipous                      !counter for place of use number
      integer :: ipods                      !counter for point of diversion
      integer :: ipoud = 0                  !place of use counter for each POD
      integer :: ipodu = 0                  !point of diversion counter for each POU
      integer :: ipor = 0                   !point of receiving object number
      
      !! withdraw from POD for each POU
      do ipous = 1, pod(ipod)%pous
        ipou = pod(ipod)%pou(ipous)%num
        ipods = pod(ipod)%pou(ipous)%pod_num
        if (pou(ipou)%pod(ipods)%wdraw_cur < pou(ipou)%pod(ipods)%wdraw_max * 86400.) then
          call wallo_withdraw (ipod, ipous)
        end if
      end do
        
      !! check if all PODs are finished for each POU
      do ipou = 1, pod(ipod)%pous
        pou(ipou)%fin = "y"
        do ipodu = 1, pou(ipou)%pods
          if (pou(ipou)%pod(ipodu)%fin == "n") then
            pou(ipou)%fin = "n"
            !! check if compensation is needed for unmet duty
            exit
          end if
        end do
      end do
      
      !! deliver to POUs and PORs
      do ipou = 1, db_mx%wallo_pou
      if (pou(ipou)%fin == "y") then
        !do ipou = 1, db_mx%wallo_pou
          !! deliver water to POU
          call wallo_pou_deliv (ipou)
            
          !! return to receiving objects and update water and constituent mass
          call wallo_return (ipou)
        !end do
      end if
      end do
      
      return
      end subroutine wallo_control