      subroutine salt_fert_wet(jj,ifrt,frt_kg) !rtb salt
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine adds salt fertilizer to a wetland

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    jj          |none          |HRU number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      
      use mgt_operations_module
      use salt_module
      use constituent_mass_module
      use fertilizer_data_module
      use hru_module, only : hru
      use res_salt_module, only : wetsalt_d
      
      implicit none

      integer, intent (in) :: jj          !none           |HRU
      integer, intent (in) :: ifrt        !               |fertilizer type from fert data base
      real, intent (in) :: frt_kg         !kg/ha          |amount of fertilizer applied 
      integer :: isalt                    !               |salt ion counter

      !only proceed if salt ions are included in simulation
      if(cs_db%num_salts > 0) then
      
        !only proceed if valid fertilizer ID is selected
        if(ifrt > 0) then
        
          !add fertilizer mass to wetland salt ion mass
          wet_water(jj)%salt(1) = wet_water(jj)%salt(1) + (frt_kg * fert_salt(ifrt)%so4 * hru(jj)%area_ha) !kg 
          wet_water(jj)%salt(2) = wet_water(jj)%salt(2) + (frt_kg * fert_salt(ifrt)%ca * hru(jj)%area_ha) !kg 
          wet_water(jj)%salt(3) = wet_water(jj)%salt(3) + (frt_kg * fert_salt(ifrt)%mg * hru(jj)%area_ha) !kg 
          wet_water(jj)%salt(4) = wet_water(jj)%salt(4) + (frt_kg * fert_salt(ifrt)%na * hru(jj)%area_ha) !kg 
          wet_water(jj)%salt(5) = wet_water(jj)%salt(5) + (frt_kg * fert_salt(ifrt)%k * hru(jj)%area_ha) !kg 
          wet_water(jj)%salt(6) = wet_water(jj)%salt(6) + (frt_kg * fert_salt(ifrt)%cl * hru(jj)%area_ha) !kg 
          wet_water(jj)%salt(7) = wet_water(jj)%salt(7) + (frt_kg * fert_salt(ifrt)%co3 * hru(jj)%area_ha) !kg 
          wet_water(jj)%salt(8) = wet_water(jj)%salt(8) + (frt_kg * fert_salt(ifrt)%hco3 * hru(jj)%area_ha) !kg 
          
          !add to wetland salt balance arrays (kg)
          wetsalt_d(jj)%salt(1)%fert = (frt_kg * fert_salt(ifrt)%so4 * hru(jj)%area_ha) !kg 
          wetsalt_d(jj)%salt(2)%fert = (frt_kg * fert_salt(ifrt)%ca * hru(jj)%area_ha) !kg
          wetsalt_d(jj)%salt(3)%fert = (frt_kg * fert_salt(ifrt)%mg * hru(jj)%area_ha) !kg 
          wetsalt_d(jj)%salt(4)%fert = (frt_kg * fert_salt(ifrt)%na * hru(jj)%area_ha) !kg 
          wetsalt_d(jj)%salt(5)%fert = (frt_kg * fert_salt(ifrt)%k * hru(jj)%area_ha) !kg 
          wetsalt_d(jj)%salt(6)%fert = (frt_kg * fert_salt(ifrt)%cl * hru(jj)%area_ha) !kg 
          wetsalt_d(jj)%salt(7)%fert = (frt_kg * fert_salt(ifrt)%co3 * hru(jj)%area_ha) !kg 
          wetsalt_d(jj)%salt(8)%fert = (frt_kg * fert_salt(ifrt)%hco3 * hru(jj)%area_ha) !kg 
          
        endif
        
      endif
      
      return
      end subroutine salt_fert_wet !rtb salt   