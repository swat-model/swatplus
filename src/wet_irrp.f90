      subroutine wet_irrp()
      !! this subroutine checks manual continuous irrigation (irrp) setting for wetland/paddy in the management.sch
      !! apply irrigation depth based on target ponding depth and the current depth

      use reservoir_data_module
      use reservoir_module
      use hydrograph_module
      use constituent_mass_module
      use aquifer_module
      use mgt_operations_module
      use hru_module, only : hru, ihru
      
      implicit none
      
      integer :: iob              !none      |hru or wro number
      integer :: iru              !none      |routing unit number
      integer :: isrc             !none      |irrigation source ID
      real :: wsa1                !m2        |water surface area 
      integer :: j,i
      real :: rto
      
      j = ihru
      wsa1 = hru(j)%area_ha * 10.
      
      !! store initial values
      irrig(j)%demand = max(0., hru(j)%irr_hmax - wet_ob(j)%depth*1000.) * wsa1 !m3
            
      rto = 0.
      if (.not. allocated(ob(j)%ru)) then 
        hru(j)%irr_src = 'unlim'
        isrc = 0
      else
        iru = ob(j)%ru(1)
        iob = sp_ob%hru + iru 
      endif
      
      if (hru(j)%irr_src == 'null') then
        hru(j)%irr_src = 'unlim'
        isrc = 0
      else
        !irrigation source is given in OP2 management.sch
        if (hru(j)%irr_isc > 0) then
          isrc = hru(j)%irr_isc
        else
          
          do i = 1, ob(iob)%src_tot
            if (hru(j)%irr_src == ob(iob)%obtyp_out(i)) then !
              isrc = ob(iob)%obtypno_out(i) ! Irrigation source is the current cha/res/aqu that the HRU is linked to in the rout_unit.con if it exists.
              exit
            else
              isrc = 0
            endif
          end do
          
        endif
      endif        
       
                    
      if (hru(j)%irr_src == 'cha'.or. hru(j)%irr_src == 'sdc') then ! irrigation source set in the management schedule
        
        if (ubound(ch_stor,1) > 0 .and. isrc>0) then 
          if (ch_stor(isrc)%flo > 0.001) then
            rto = min(0.99, irrig(j)%demand / ch_stor(isrc)%flo)                ! ratio of water removed from channel volume
          end if
          irrig(j)%water = rto * ch_stor(isrc)                       ! irrigation water
          cs_irr(isrc) = rto * ch_water(isrc)                         ! constituents in irrigation water
          ch_stor(isrc) = (1. - rto) * ch_stor(isrc)                        ! remainder stays in channel
          ch_water(isrc) = (1. - rto) * ch_water(isrc)                      
        else
          irrig(j)%water%flo = irrig(j)%demand
        endif
            
      elseif (hru(j)%irr_src == 'res') then
        if (ubound(res,1) > 0 .and. isrc>0) then 
          if (res(isrc)%flo > 0.001) then
            rto = min(0.99, irrig(j)%demand / res(isrc)%flo)                    ! ratio of water removed from res volume
          end if
          irrig(j)%water = rto * res(isrc)                           ! organics in irrigation water
          cs_irr(isrc) = rto * res_water(isrc)                        ! constituents in irrigation water
          res(isrc) = (1. - rto) * res(isrc)                                ! remainder stays in reservoir
          res_water(isrc) = (1. - rto) * res_water(isrc)                    
        else
          irrig(j)%water%flo = irrig(j)%demand
        endif
            
      elseif (hru(j)%irr_src == 'aqu') then
        if (ubound(aqu_d,1) > 0 .and. isrc>0) then 
          if (aqu_d(isrc)%stor > 0.001) then
            rto = min(0.99, irrig(j)%demand / aqu_d(isrc)%stor)            ! ratio of water removed from aquifer volume
          end if
          irrig(j)%water%flo = rto * aqu_d(isrc)%flo                 ! organics in irrigation water
          cs_irr(isrc) = rto * cs_aqu(isrc)                           ! constituents in irrigation water
          aqu_d(isrc)%stor = (1. - rto) * aqu_d(isrc)%stor                  ! remainder stays in aquifer
          cs_aqu(isrc) = (1. - rto) * cs_aqu(isrc)  
        else
          irrig(j)%water%flo = irrig(j)%demand
        endif
            
      else !unlimited source
        irrig(j)%water%flo = irrig(j)%demand
            
      end if
            
      !irrig(j)%eff = irrop_db(mgt%op1)%eff
      !irrig(j)%frac_surq = irrop_db(mgt%op1)%surq
      irrig(j)%applied = irrig(j)%water%flo / wsa1 * irrig(j)%eff * (1. - irrig(j)%frac_surq) !mm
      irrig(j)%runoff = irrig(j)%water%flo / wsa1 * irrig(j)%eff * irrig(j)%frac_surq !mm
      return
      end subroutine wet_irrp