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

      integer :: iadep = 0        !            |
      integer :: j = 0            !none        |counter
      integer :: iob = 0          !            |
      integer :: ist = 0          !            |
      integer :: isalt = 0        !            |salt ion counter
      integer :: dum = 0
      real :: const = 0.          !            |constant used for rate, days, etc
      real :: salt_mass = 0.      !kg/ha 

      j = ihru
      
	  !only proceed if road salt is included 
      if(salt_road_flag == 1) then
			
	  if(cs_db%num_salts > 0) then
      
	  !determine the hru's weather station
      iob = hru(j)%obj_no
      iwst = ob(iob)%wst
        
      !daily values (based on yearly road salt applications) 
      !loop through the salt ions
      do isalt=1,cs_db%num_salts 
        !add to salt balance arrays
        salt_mass = rdapp_salt(iwst)%salt(isalt)%roadday(time%day,time%yrs)
        if(salt_mass > 0) then
				  dum = 0
        endif
        hsaltb_d(j)%salt(isalt)%road = rdapp_salt(iwst)%salt(isalt)%roadday(time%day,time%yrs)  !kg/ha
        !add both to soil profile  
        cs_soil(j)%ly(1)%salt(isalt) = cs_soil(j)%ly(1)%salt(isalt) + hsaltb_d(j)%salt(isalt)%road
	    enddo
        
      endif
			
      endif !check if active
      
      return
    end subroutine salt_roadsalt
    