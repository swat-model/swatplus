      subroutine cs_divert(iwallo,idmd,dem_id) !rtb cs
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine adds cs mass to the channel, and and removes cs mass
!!    from the source object
      
      use water_allocation_module
      use water_body_module
      use aquifer_module
      use reservoir_data_module
      use hydrograph_module, only : irrig,res,sp_ob1,ob
      use hru_module, only : hru
      use basin_module, only : bsn_cc
      use cs_module !rtb cs
      use cs_aquifer !rtb cs
      use ch_cs_module !rtb cs
      use res_cs_module !rtb cs
      use constituent_mass_module

      implicit none 

      integer, intent (inout) :: iwallo     !water allocation object number
      integer, intent (inout) :: idmd       !water demand object number
      integer, intent (inout) :: dem_id     !ID of demand object that receives diverted water
      character*10 :: obj_type              !object type of source object
      character*10 :: obj_type_dem          !object type of demand object          
      integer :: obj_num                    !object number of source object
      integer :: isrc												!irrigation source counter
      integer :: nsource						        !number of irrigation sources for the object
      integer :: ires                       !reservoir ID
      integer :: iaq                        !aquifer ID
      integer :: ichan                      !channel ID
      integer :: obnum                      !object number
      integer :: ics                        !cs ion counter
      real :: vol_total											!total volume (m3) withdrawn from source objects
      real :: vol_fraction                  !fraction of total volume from each source object
      real :: obj_vol                       !volume (m3) withdrawn from each source object
      real :: mass_diff,ion_mass,res_mass,mass_initial,irrig_mass,gw_volume
      
      
      !determine number of water sources
      nsource = wallo(iwallo)%dmd(idmd)%dmd_src_obs
	    
      !demand object type
      obj_type_dem = wallo(iwallo)%dmd(idmd)%rcv_ob
      
      !loop through the water sources
      do isrc=1,nsource
        
        !determine the object type and object number
        obj_type = wallo(iwallo)%dmd(idmd)%src_ob(isrc)%ob_typ
        obj_num = wallo(iwallo)%dmd(idmd)%src_ob(isrc)%ob_num
        
        !total water volume removed from source object
        obj_vol = wallod_out(iwallo)%dmd(idmd)%src(isrc)%withdr
        
        !calculate cs mass added to channel (and cs mass removed from source object)
        if(obj_vol > 0) then
        
        !reservoir
        if(obj_type.eq.'res') then 
          ires = obj_num
          !remove cs mass from reservoir; add to channel
          do ics=1,cs_db%num_cs
            ion_mass = (res_water(ires)%csc(ics)*obj_vol) / 1000. !kg/day  
            res_mass = res_water(ires)%cs(ics) !kg
            mass_diff = 0.
            if(ion_mass.gt.res_mass) then
              mass_diff = ion_mass - res_mass
            endif  
						ion_mass = ion_mass - mass_diff
            if(ion_mass.lt.0) ion_mass = 0.
            !remove cs mass from the reservoir
            res_water(ires)%cs(ics) = res_water(ires)%cs(ics) - ion_mass
            !rescs_d(ires)%cs(ics)%div = rescs_d(ires)%cs(ics)%div + (ion_mass*-1) !kg - include in reservoir cs balance
            rescs_d(ires)%cs(ics)%div = rescs_d(ires)%cs(ics)%div + (-ion_mass) !kg - include in reservoir cs balance

            !add cs mass to receiving demand object
            if(obj_type_dem == "cha") then
              ch_water(dem_id)%cs(ics) = ch_water(dem_id)%cs(ics) + ion_mass !kg
              chcs_d(dem_id)%cs(ics)%div = chcs_d(dem_id)%cs(ics)%div + ion_mass !kg - include in channel cs balance
						elseif(obj_type_dem == "res") then
              res_water(dem_id)%cs(ics) = res_water(dem_id)%cs(ics) + ion_mass !kg
              rescs_d(dem_id)%cs(ics)%div = rescs_d(dem_id)%cs(ics)%div + ion_mass !kg - include in reservoir cs balance
            endif
          enddo
        
        !aquifer  
        elseif(obj_type.eq.'aqu' .and. bsn_cc%gwflow.eq.0) then !aquifer (if gwflow active: handled in gwflow_pumping)
          obnum = sp_ob1%aqu + obj_num - 1 
          iaq = obj_num
          !remove cs mass from aquifer; add to soil profile; include in daily cs mass balance
          do ics=1,cs_db%num_cs
            mass_initial = cs_aqu(iaq)%cs(ics)
            ion_mass = (cs_aqu(iaq)%csc(ics)*obj_vol) / 1000. !kg/day  
            mass_diff = 0.
            if(ion_mass.gt.cs_aqu(iaq)%cs(ics)) then
              mass_diff = ion_mass - cs_aqu(iaq)%cs(ics)
            endif  
						ion_mass = ion_mass - mass_diff
            if(ion_mass.lt.0) ion_mass = 0.
            !remove cs mass from the aquifer
            cs_aqu(iaq)%cs(ics) = cs_aqu(iaq)%cs(ics) - ion_mass !kg
            !acsb_d(iaq)%cs(ics)%div = acsb_d(iaq)%cs(ics)%div + (ion_mass*-1) !kg
            acsb_d(iaq)%cs(ics)%div = acsb_d(iaq)%cs(ics)%div + (-ion_mass) !kg      
            !add cs mass to receiving demand object
            if(obj_type_dem == "cha") then
              ch_water(dem_id)%cs(ics) = ch_water(dem_id)%cs(ics) + ion_mass !kg
              chcs_d(dem_id)%cs(ics)%div = chcs_d(dem_id)%cs(ics)%div + ion_mass !kg - include in channel cs balance
						elseif(obj_type_dem == "res") then
              res_water(dem_id)%cs(ics) = res_water(dem_id)%cs(ics) + ion_mass !kg
              rescs_d(dem_id)%cs(ics)%div = rescs_d(dem_id)%cs(ics)%div + ion_mass !kg - include in reservoir cs balance
            endif
            !convert aquifer mass to concentration
            gw_volume = (aqu_d(iaq)%stor/1000.) * (ob(obnum)%area_ha*10000.) !m3 of groundwater
            if(gw_volume.gt.0) then
              cs_aqu(iaq)%csc(ics) = (cs_aqu(iaq)%cs(ics) * 1000.) / gw_volume !g/m3 = mg/L
            else
              cs_aqu(iaq)%cs(ics) = 0.
              cs_aqu(iaq)%csc(ics) = 0.
            endif
          enddo
          
        !stream channel  
        elseif(obj_type.eq.'cha') then 
          obnum = sp_ob1%chandeg + obj_num - 1 !channel object
          ichan = ob(obnum)%num
          !remove cs mass from channel; add to soil profile; include in daily cs mass balance (rtb cs)
          do ics=1,cs_db%num_cs
            if(ob(obnum)%hd(1)%flo.gt.0) then
              mass_initial = ch_water(ichan)%cs(ics)
              ion_mass = (((ch_water(ichan)%cs(ics)*1000.)/ob(obnum)%hd(1)%flo)*obj_vol) / 1000. !kg/day  
              mass_diff = 0.
              if(ion_mass.gt.ch_water(ichan)%cs(ics)) then
                mass_diff = ion_mass - ch_water(ichan)%cs(ics) 
              endif  
							ion_mass = ion_mass - mass_diff
              if(ion_mass.lt.0) ion_mass = 0.
              !remove cs mass from channel
              ch_water(ichan)%cs(ics) = ch_water(ichan)%cs(ics) - ion_mass !kg
              !chcs_d(ichan)%cs(ics)%div = chcs_d(ichan)%cs(ics)%div + (ion_mass*-1) !kg - include in channel cs balance
              chcs_d(ichan)%cs(ics)%div = chcs_d(ichan)%cs(ics)%div + (-ion_mass) !kg - include in channel cs balance
              !add cs mass to receiving demand object
              if(obj_type_dem == "cha") then
                ch_water(dem_id)%cs(ics) = ch_water(dem_id)%cs(ics) + ion_mass !kg
                chcs_d(dem_id)%cs(ics)%div = chcs_d(dem_id)%cs(ics)%div + ion_mass !kg - include in channel cs balance
						  elseif(obj_type_dem == "res") then
                res_water(dem_id)%cs(ics) = res_water(dem_id)%cs(ics) + ion_mass !kg
                rescs_d(dem_id)%cs(ics)%div = rescs_d(dem_id)%cs(ics)%div + ion_mass !kg - include in reservoir cs balance
              endif
						endif
          enddo
          
        endif
        
        endif
        
      enddo !go to next source object
      

      return
      end !cs_divert
