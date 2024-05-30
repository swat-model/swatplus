      subroutine wallo_treatment (iwallo, idmd)
      
      use water_allocation_module
      use hydrograph_module
      
      implicit none 

      integer, intent (in):: iwallo         !water allocation object number
      integer, intent (in) :: idmd          !water demand object number
      integer :: itrt               !none       |treatment database number
      integer :: irec               !none       |recall database number
      
      !! treatment option for treating water
      if (wallo(iwallo)%dmd(idmd)%treat_typ == "treat") then
        itrt = wallo(iwallo)%dmd(idmd)%trt_num
        !! treated outflow is a fraction of withdrawal
        ht5 = trt(itrt)
        ht5%flo = trt(itrt)%flo * wallo(iwallo)%dmd(idmd)%hd%flo
        !! convert concentration to mass
        call hyd_convert_conc_to_mass (ht5)
        wallo(iwallo)%dmd(idmd)%trt = ht5
      end if
      
      !! recall option for treated water
      if (wallo(iwallo)%dmd(idmd)%treat_typ == "recall") then
        irec = wallo(iwallo)%dmd(idmd)%trt_num
        select case (recall(irec)%typ)
        case (1)    !daily
          wallo(iwallo)%dmd(idmd)%trt = recall(irec)%hd(time%day,time%yrs)
        case (2)    !monthly
          wallo(iwallo)%dmd(idmd)%trt = recall(irec)%hd(time%mo,time%yrs)
        case (3)    !annual
          wallo(iwallo)%dmd(idmd)%trt = recall(irec)%hd(1,time%yrs)
        end select
      end if
      
      return
      end subroutine wallo_treatment