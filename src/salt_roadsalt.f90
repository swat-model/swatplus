      subroutine salt_roadsalt
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine adds salt from applied road salt to the soil profile

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

      use basin_module
      use organic_mineral_mass_module
      use hydrograph_module
      use hru_module, only : hru, ihru, timest 
      use climate_module
      use output_landscape_module
      use salt_module
      use constituent_mass_module
      
      implicit none

      integer :: iadep            !            |
      integer :: j                !none        |counter
      integer :: iob              !            |
      integer :: ist              !            |
      integer :: isalt            !            |salt ion counter
      integer :: dum
      real :: const               !            |constant used for rate, days, etc

      j = ihru
      
      if(cs_db%num_salts > 0) then
      
      iob = hru(j)%obj_no
      iwst = ob(iob)%wst
      iadep = wst(iwst)%wco%atmodep
      ist = atmodep_cont%ts
        
      !add applied road salt to the soil profile
      if (ist > 0 .and. ist <= atmodep_cont%num) then
      
        !monthly values
        if (atmodep_cont%timestep == "mo") then
          const = float (ndays(time%mo + 1) - ndays(time%mo))
          !loop through the salt ions
          do isalt=1,cs_db%num_salts 
            !add to salt balance arrays
            hsaltb_d(j)%salt(isalt)%road = rdapp_salt(iadep)%salt(isalt)%roadmo(ist) / const  !kg/ha
            !add both to soil profile  
            cs_soil(j)%ly(1)%salt(isalt) = cs_soil(j)%ly(1)%salt(isalt) + hsaltb_d(j)%salt(isalt)%road
					enddo
        endif 
        
        !yearly values
        if (atmodep_cont%timestep == "yr") then
          !loop through the salt ions
          do isalt=1,cs_db%num_salts 
            !add to salt balance arrays
            hsaltb_d(j)%salt(isalt)%road = rdapp_salt(iadep)%salt(isalt)%roadday(time%day,ist)  !kg/ha
            !add both to soil profile  
            cs_soil(j)%ly(1)%salt(isalt) = cs_soil(j)%ly(1)%salt(isalt) + hsaltb_d(j)%salt(isalt)%road
					enddo
        endif
        
      endif
      
      !annual average values
      if (atmodep_cont%timestep == "aa") then
        !loop through the salt ions
        do isalt=1,cs_db%num_salts 
          !add to salt balance arrays
          hsaltb_d(j)%salt(isalt)%road = rdapp_salt(iadep)%salt(isalt)%road / 365.  !kg/ha
          !add both to soil profile  
          cs_soil(j)%ly(1)%salt(isalt) = cs_soil(j)%ly(1)%salt(isalt) + hsaltb_d(j)%salt(isalt)%road
				enddo
      endif
       
      endif
      
      return
      end subroutine salt_roadsalt