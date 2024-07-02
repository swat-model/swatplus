      subroutine cs_fert_wet(jj,ifrt,frt_kg) !rtb cs
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine adds constituent fertilizer to a wetland

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    jj          |none          |HRU number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      
      use mgt_operations_module
      use cs_module
      use constituent_mass_module
      use fertilizer_data_module
      use hru_module, only : hru
      use res_cs_module, only : wetcs_d
      
      implicit none

      integer, intent (in) :: jj          !none           |HRU
      integer, intent (in) :: ifrt        !               |fertilizer type from fert data base
      real, intent (in) :: frt_kg         !kg/ha          |amount of fertilizer applied 
      integer :: ics                      !               |constituent counter

      !only proceed if constituents are included in simulation
      if(cs_db%num_cs > 0) then
      
        !only proceed if valid fertilizer ID is selected
        if(ifrt > 0) then
        
          !add fertilizer mass to wetland constituent mass
          wet_water(jj)%cs(1) = wet_water(jj)%cs(1) + (frt_kg * fert_cs(ifrt)%seo4 * hru(jj)%area_ha) !kg 
          wet_water(jj)%cs(2) = wet_water(jj)%cs(2) + (frt_kg * fert_cs(ifrt)%seo3 * hru(jj)%area_ha) !kg 
          wet_water(jj)%cs(3) = wet_water(jj)%cs(3) + (frt_kg * fert_cs(ifrt)%boron * hru(jj)%area_ha) !kg 
          
          !add to wetland constituents balance arrays (kg)
          wetcs_d(jj)%cs(1)%fert = frt_kg * fert_cs(ifrt)%seo4 * hru(jj)%area_ha !kg 
          wetcs_d(jj)%cs(2)%fert = frt_kg * fert_cs(ifrt)%seo3 * hru(jj)%area_ha !kg 
          wetcs_d(jj)%cs(3)%fert = frt_kg * fert_cs(ifrt)%boron * hru(jj)%area_ha !kg 
        endif
        
      endif
      
      return
      end subroutine cs_fert_wet !rtb cs
      