      subroutine cs_irrig(iwallo,idmd,ihru) !rtb cs
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine adds constituent mass from irrigation water into the soil profile, and removes constituent mass
!!    from the source object
      
      use water_allocation_module
      use water_body_module
      use aquifer_module
      use reservoir_data_module
      use hydrograph_module, only : irrig,res,sp_ob1,ob
      use hru_module, only : hru
      use cs_module !rtb cs
      use cs_aquifer !rtb cs
      use ch_cs_module !rtb cs
      use res_cs_module !rtb cs
      use basin_module, only : bsn_cc
      use constituent_mass_module

      implicit none 

      integer, intent (inout) :: iwallo     !water allocation object number
      integer, intent (inout) :: idmd       !water demand object number
      integer, intent (inout) :: ihru       !HRU that receives irrigation water
      character*10 :: irrig_type  
      integer :: isrc												!irrigation source counter
      integer :: irrig_nsource						  !number of irrigation sources for the object
      integer :: irrig_ob                   !object number of irrigation source
      integer :: ires                       !reservoir ID
      integer :: iaq                        !aquifer ID
      integer :: ichan                      !channel ID
      integer :: obnum                      !object number
      integer :: obnum_chan                 !channel object number
      integer :: ics                        !constituent counter
      integer :: wetland                    !wetland flag
      real :: irrig_total                   !irrigation removed from source (m3)
      real :: irrig_fraction							  !fraction of irrigation water
      real :: irrig_volume                  !volume (m3) of irrigation water
      real :: mass_diff,ion_mass,res_mass,mass_initial,irrig_mass,gw_volume,cs_conc
      

      !determine number of irrigation sources
      irrig_nsource = wallo(iwallo)%dmd(idmd)%dmd_src_obs
      
      !loop through the irrigation sources
      do isrc=1,irrig_nsource
      
        !determine the object type and object number (of water source)
        irrig_type = wallo(iwallo)%dmd(idmd)%src_ob(isrc)%ob_typ
        irrig_ob = wallo(iwallo)%dmd(idmd)%src_ob(isrc)%ob_num
        
        !total water volume (m3) removed from source object
        irrig_volume = wallod_out(iwallo)%dmd(idmd)%src(isrc)%withdr
        
        !calculate salt mass added to soil profile via irrigation (and salt mass removed from source object)
        if(irrig_volume > 0) then
        
        !reservoir
        if(irrig_type.eq.'res') then 
          ires = irrig_ob
          !remove constituent mass from reservoir; add to soil profile; include in daily constituent mass balance
          do ics=1,cs_db%num_cs
            ion_mass = (res_water(ires)%csc(ics)*irrig_volume) / 1000. !kg/day  
            res_mass = res_water(ires)%cs(ics) !kg
            mass_diff = 0.
            if(ion_mass.gt.res_mass) then
              mass_diff = ion_mass - res_mass
            endif  
					  ion_mass = ion_mass - mass_diff
            if(ion_mass.lt.0) ion_mass = 0.
            !remove constituent mass from the reservoir
            res_water(ires)%cs(ics) = res_water(ires)%cs(ics) - ion_mass !kg
            rescs_d(ires)%cs(ics)%irrig = rescs_d(ires)%cs(ics)%irrig + ion_mass !kg - include in reservoir constituent balance
            !add constituent mass to demand object
            wetland = hru(ihru)%dbs%surf_stor !check if HRU is a wetland
            if(wetland > 0) then !add to wetland
              wet_water(ihru)%cs(ics) = wet_water(ihru)%cs(ics) + ion_mass !kg  
              wetcs_d(ihru)%cs(ics)%irrig = wetcs_d(ihru)%cs(ics)%irrig + ion_mass !kg
            else !add to soil profile
              cs_soil(ihru)%ly(1)%cs(ics) = cs_soil(ihru)%ly(1)%cs(ics) + (ion_mass/hru(ihru)%area_ha) !kg/ha - add to soil layer
              hcsb_d(ihru)%cs(ics)%irsw = hcsb_d(ihru)%cs(ics)%irsw + (ion_mass/hru(ihru)%area_ha) !kg/ha - include in soil constituent balance  
            endif
          enddo
        
        !aquifer  
        elseif(irrig_type.eq.'aqu' .and. bsn_cc%gwflow.eq.0) then !aquifer (if gwflow active: handled in gwflow_ppag)
          obnum = sp_ob1%aqu + irrig_ob - 1 
          iaq = irrig_ob
          !remove constituent mass from aquifer; add to soil profile; include in daily constituent mass balance
          do ics=1,cs_db%num_cs
            mass_initial = cs_aqu(iaq)%cs(ics)
            ion_mass = (cs_aqu(iaq)%csc(ics)*irrig_volume) / 1000. !kg/day  
            mass_diff = 0.
            if(ion_mass.gt.cs_aqu(iaq)%cs(ics)) then
              mass_diff = ion_mass - cs_aqu(iaq)%cs(ics)
            endif  
					  ion_mass = ion_mass - mass_diff
            if(ion_mass.lt.0) ion_mass = 0.
            cs_aqu(iaq)%cs(ics) = cs_aqu(iaq)%cs(ics) - ion_mass !kg - remove from storage
            acsb_d(iaq)%cs(ics)%irr = acsb_d(iaq)%cs(ics)%irr + ion_mass !kg - include in aquifer constituent balance
            !add constituent mass to demand object
            wetland = hru(ihru)%dbs%surf_stor !check if HRU is a wetland
            if(wetland > 0) then !add to wetland
              wet_water(ihru)%cs(ics) = wet_water(ihru)%cs(ics) + ion_mass !kg  
              wetcs_d(ihru)%cs(ics)%irrig = wetcs_d(ihru)%cs(ics)%irrig + ion_mass !kg
            else !add to soil profile
              cs_soil(ihru)%ly(1)%cs(ics) = cs_soil(ihru)%ly(1)%cs(ics) + (ion_mass/hru(ihru)%area_ha) !kg/ha - add to soil layer
              hcsb_d(ihru)%cs(ics)%irgw = hcsb_d(ihru)%cs(ics)%irgw + (ion_mass/hru(ihru)%area_ha) !kg/ha - include in soil constituent balance 
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
        elseif(irrig_type.eq.'cha') then 
          obnum = sp_ob1%chandeg + irrig_ob - 1 !channel object
          ichan = ob(obnum)%num
          !remove constituent mass from channel; add to soil profile; include in daily constituent mass balance
          do ics=1,cs_db%num_cs
            if(ob(obnum)%hd(1)%flo > 10.) then
              mass_initial = ch_water(ichan)%cs(ics)
              ion_mass = (((ch_water(ichan)%cs(ics)*1000.)/ob(obnum)%hd(1)%flo)*irrig_volume) / 1000. !kg/day  
              mass_diff = 0.
              if(ion_mass.gt.ch_water(ichan)%cs(ics)) then
                mass_diff = ion_mass - ch_water(ichan)%cs(ics) 
              endif  
						  ion_mass = ion_mass - mass_diff
              if(ion_mass.lt.0) ion_mass = 0.
              ch_water(ichan)%cs(ics) = ch_water(ichan)%cs(ics) - ion_mass !kg - remove from storage
              chcs_d(ichan)%cs(ics)%irr = chcs_d(ichan)%cs(ics)%irr + ion_mass !kg - include in channel constituent balance
              !add constituent mass to demand object
              wetland = hru(ihru)%dbs%surf_stor !check if HRU is a wetland
              if(wetland > 0) then !add to wetland
                wet_water(ihru)%cs(ics) = wet_water(ihru)%cs(ics) + ion_mass !kg  
                wetcs_d(ihru)%cs(ics)%irrig = wetcs_d(ihru)%cs(ics)%irrig + ion_mass !kg
              else !add to soil profile
                cs_soil(ihru)%ly(1)%cs(ics) = cs_soil(ihru)%ly(1)%cs(ics) + (ion_mass/hru(ihru)%area_ha) !kg/ha - add to soil layer
                hcsb_d(ihru)%cs(ics)%irsw = hcsb_d(ihru)%cs(ics)%irsw + (ion_mass/hru(ihru)%area_ha) !kg/ha - include in soil constituent balance
              endif
					  endif
          enddo
        
        !canal diversion sources  
        elseif(irrig_type.eq.'div') then  
          !determine source channel of canal water
          obnum = sp_ob1%recall + irrig_ob - 1 !recall object  
          if(ob(obnum)%obtyp_out(1) == 'sdc') then
            ichan = ob(obnum)%obtypno_out(1)
            obnum_chan = sp_ob1%chandeg + ichan - 1 !channel object
            !add to soil profile; include in daily constituent mass balance
            if(ob(obnum_chan)%hd(1)%flo > 10.) then !only proceed if channel has water
            do ics=1,cs_db%num_cs
              !concentration of source channel water
              cs_conc = (ch_water(ichan)%cs(ics)*1000.)/ob(obnum_chan)%hd(1)%flo !g/m3
              ion_mass = (cs_conc*irrig_volume) / 1000. !kg/day  
              !add constituent mass to demand object
              wetland = hru(ihru)%dbs%surf_stor !check if HRU is a wetland
              if(wetland > 0) then !add to wetland
                wet_water(ihru)%cs(ics) = wet_water(ihru)%cs(ics) + ion_mass !kg  
                wetcs_d(ihru)%cs(ics)%irrig = wetcs_d(ihru)%cs(ics)%irrig + ion_mass !kg
              else !add to soil profile
                cs_soil(ihru)%ly(1)%cs(ics) = cs_soil(ihru)%ly(1)%cs(ics) + (ion_mass/hru(ihru)%area_ha) !kg/ha - add to soil layer
                hcsb_d(ihru)%cs(ics)%irsw = hcsb_d(ihru)%cs(ics)%irsw + (ion_mass/hru(ihru)%area_ha) !kg/ha - include in soil constituent balance
              endif
            enddo !go to next constituent
            endif
          endif
          
        !unlimited (outside watershed)
        elseif(irrig_type.eq.'unl') then 
          !add to soil profile; include in daily constituent mass balance
          do ics=1,cs_db%num_cs
            irrig_mass = 0.
            irrig_mass = (cs_irr(ihru)%csc(ics)/1000.) * irrig_volume !kg
            irrig_mass = irrig_mass / hru(ihru)%area_ha !kg/ha
            !add constituent mass to demand object
            wetland = hru(ihru)%dbs%surf_stor !check if HRU is a wetland
            if(wetland > 0) then !add to wetland
              wet_water(ihru)%cs(ics) = wet_water(ihru)%cs(ics) + irrig_mass !kg  
              wetcs_d(ihru)%cs(ics)%irrig = wetcs_d(ihru)%cs(ics)%irrig + irrig_mass !kg
            else !add to soil profile
              cs_soil(ihru)%ly(1)%cs(ics) = cs_soil(ihru)%ly(1)%cs(ics) + irrig_mass !kg/ha
              hcsb_d(ihru)%cs(ics)%irwo = hcsb_d(ihru)%cs(ics)%irwo + irrig_mass !kg/ha
            endif
          enddo  
        endif
        
        endif

      enddo !go to next irrigation source

      return
      end !cs_irrig
