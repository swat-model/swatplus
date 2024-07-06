      subroutine salt_fert(jj,ifrt,frt_kg,fertop) !rtb salt
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine adds salt fertilizer to the soil profile

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    jj          |none          |HRU number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

      use mgt_operations_module
      use salt_module
      use constituent_mass_module
      use fertilizer_data_module
      
      implicit none

      integer, intent (in) :: jj          !none           |HRU
      integer, intent (in) :: ifrt        !               |fertilizer type from fert data base
      real, intent (in) :: frt_kg         !kg/ha          |amount of fertilizer applied
      integer, intent (in) :: fertop      !               | 
      character(len=16) :: fert_type
      real :: xx                          !               |surface application fraction 
      integer :: isalt                    !               |salt ion counter
      integer :: l                        !none           |counter 

      
      !only proceed if salt ions are included in simulation
      if(cs_db%num_salts > 0 .and. fert_salt_flag == 1) then
      
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

            !for each salt ion: add salt mass from fertilizer to the top layer of the soil (kg/ha) 
            cs_soil(jj)%ly(l)%salt(1) = cs_soil(jj)%ly(l)%salt(1) + (xx * frt_kg * fert_salt(ifrt)%so4)
            cs_soil(jj)%ly(l)%salt(2) = cs_soil(jj)%ly(l)%salt(2) + (xx * frt_kg * fert_salt(ifrt)%ca)
            cs_soil(jj)%ly(l)%salt(3) = cs_soil(jj)%ly(l)%salt(3) + (xx * frt_kg * fert_salt(ifrt)%mg)
            cs_soil(jj)%ly(l)%salt(4) = cs_soil(jj)%ly(l)%salt(4) + (xx * frt_kg * fert_salt(ifrt)%na)
            cs_soil(jj)%ly(l)%salt(5) = cs_soil(jj)%ly(l)%salt(5) + (xx * frt_kg * fert_salt(ifrt)%k)
            cs_soil(jj)%ly(l)%salt(6) = cs_soil(jj)%ly(l)%salt(6) + (xx * frt_kg * fert_salt(ifrt)%cl)
            cs_soil(jj)%ly(l)%salt(7) = cs_soil(jj)%ly(l)%salt(7) + (xx * frt_kg * fert_salt(ifrt)%co3)
            cs_soil(jj)%ly(l)%salt(8) = cs_soil(jj)%ly(l)%salt(8) + (xx * frt_kg * fert_salt(ifrt)%hco3)
        
            !add to salt balance arrays (kg/ha)
            fert_type = fertdb(ifrt)%fertnm
            if(fert_type(1:1) == 'a') then !salt amendment
              hsaltb_d(jj)%salt(1)%amnd = hsaltb_d(jj)%salt(1)%amnd + (xx * frt_kg * fert_salt(ifrt)%so4)
              hsaltb_d(jj)%salt(2)%amnd = hsaltb_d(jj)%salt(2)%amnd + (xx * frt_kg * fert_salt(ifrt)%ca)
              hsaltb_d(jj)%salt(3)%amnd = hsaltb_d(jj)%salt(3)%amnd + (xx * frt_kg * fert_salt(ifrt)%mg)
              hsaltb_d(jj)%salt(4)%amnd = hsaltb_d(jj)%salt(4)%amnd + (xx * frt_kg * fert_salt(ifrt)%na)
              hsaltb_d(jj)%salt(5)%amnd = hsaltb_d(jj)%salt(5)%amnd + (xx * frt_kg * fert_salt(ifrt)%k)
              hsaltb_d(jj)%salt(6)%amnd = hsaltb_d(jj)%salt(6)%amnd + (xx * frt_kg * fert_salt(ifrt)%cl)
              hsaltb_d(jj)%salt(7)%amnd = hsaltb_d(jj)%salt(7)%amnd + (xx * frt_kg * fert_salt(ifrt)%co3)
              hsaltb_d(jj)%salt(8)%amnd = hsaltb_d(jj)%salt(8)%amnd + (xx * frt_kg * fert_salt(ifrt)%hco3)
            else !regular fertilizer
              hsaltb_d(jj)%salt(1)%fert = hsaltb_d(jj)%salt(1)%fert + (xx * frt_kg * fert_salt(ifrt)%so4)
              hsaltb_d(jj)%salt(2)%fert = hsaltb_d(jj)%salt(2)%fert + (xx * frt_kg * fert_salt(ifrt)%ca)
              hsaltb_d(jj)%salt(3)%fert = hsaltb_d(jj)%salt(3)%fert + (xx * frt_kg * fert_salt(ifrt)%mg)
              hsaltb_d(jj)%salt(4)%fert = hsaltb_d(jj)%salt(4)%fert + (xx * frt_kg * fert_salt(ifrt)%na)
              hsaltb_d(jj)%salt(5)%fert = hsaltb_d(jj)%salt(5)%fert + (xx * frt_kg * fert_salt(ifrt)%k)
              hsaltb_d(jj)%salt(6)%fert = hsaltb_d(jj)%salt(6)%fert + (xx * frt_kg * fert_salt(ifrt)%cl)
              hsaltb_d(jj)%salt(7)%fert = hsaltb_d(jj)%salt(7)%fert + (xx * frt_kg * fert_salt(ifrt)%co3)
              hsaltb_d(jj)%salt(8)%fert = hsaltb_d(jj)%salt(8)%fert + (xx * frt_kg * fert_salt(ifrt)%hco3)
            endif
          
          enddo !go to second layer
          
        endif
        
      endif
      
      return
      end subroutine salt_fert !rtb salt   