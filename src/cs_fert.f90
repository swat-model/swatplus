      subroutine cs_fert(jj,ifrt,frt_kg,fertop) !rtb cs
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine adds constituent fertilizer to the soil profile

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    jj          |none          |HRU number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

      use mgt_operations_module
      use cs_module
      use constituent_mass_module
      use fertilizer_data_module
      
      implicit none

      integer, intent (in) :: jj          !none           |HRU
      integer, intent (in) :: ifrt        !               |fertilizer type from fert data base
      real, intent (in) :: frt_kg         !kg/ha          |amount of fertilizer applied
      integer, intent (in) :: fertop      !               | 
      real :: xx                          !               |surface application fraction 
      integer :: ics                      !               |constituent counter
      integer :: l                        !none           |counter 

      !only proceed if constituents are included in simulation
      if (cs_db%num_cs > 0 .and. fert_cs_flag == 1) then
      
        !only proceed if valid fertilizer ID is selected
        if(ifrt > 0) then
        
          !loop through the top two layers - split application according to surf_frac parameter
          do l=1,2
            
            !determine fraction for the current layer; use surface application fraction
            xx = 0.
            if (l == 1) then
              xx = chemapp_db(fertop)%surf_frac
            else
              xx = 1. - chemapp_db(fertop)%surf_frac                     
            endif
        
            !for each constituent: add mass from fertilizer to the top layer of the soil (kg/ha) 
            cs_soil(jj)%ly(1)%cs(1) = cs_soil(jj)%ly(1)%cs(1) + (xx * frt_kg * fert_cs(ifrt)%seo4)
            cs_soil(jj)%ly(1)%cs(2) = cs_soil(jj)%ly(1)%cs(2) + (xx * frt_kg * fert_cs(ifrt)%seo3)
            cs_soil(jj)%ly(1)%cs(3) = cs_soil(jj)%ly(1)%cs(3) + (xx * frt_kg * fert_cs(ifrt)%boron)
        
            !add to constituent balance arrays (kg/ha)
            hcsb_d(jj)%cs(1)%fert = hcsb_d(jj)%cs(1)%fert + (xx * frt_kg * fert_cs(ifrt)%seo4)
            hcsb_d(jj)%cs(2)%fert = hcsb_d(jj)%cs(2)%fert + (xx * frt_kg * fert_cs(ifrt)%seo3)
            hcsb_d(jj)%cs(3)%fert = hcsb_d(jj)%cs(3)%fert + (xx * frt_kg * fert_cs(ifrt)%boron)
          
          enddo
        
        endif
        
      endif
      
      return
      end subroutine cs_fert !rtb cs
      