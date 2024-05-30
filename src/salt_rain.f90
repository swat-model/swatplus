      subroutine salt_rain
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine adds salt from atmospheric deposition (rainfall, dry) to the soil profile

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
      real :: const               !            |constant used for rate, days, etc

      j = ihru
      
      if(cs_db%num_salts > 0) then
      
      iob = hru(j)%obj_no
      iwst = ob(iob)%wst
      iadep = wst(iwst)%wco%atmodep
      ist = atmodep_cont%ts
        
      !add salt in atmospheric deposition to the soil profile
      !atmospheric deposition = rainfall (mg/L) + dry deposition (kg/ha)
      !for rainfall: (mg/l*mm) * kg/1,000,000 mg *1,00 l/m3 * m3/1,000 mm * 10,000 m2/ha = 0.01
      if (ist > 0 .and. ist <= atmodep_cont%num) then
      
        !monthly values
        if (atmodep_cont%timestep == "mo") then
          const = float (ndays(time%mo + 1) - ndays(time%mo))
          !loop through the salt ions
          do isalt=1,cs_db%num_salts 
            !add to salt balance arrays
            hsaltb_d(j)%salt(isalt)%rain = .01 * atmodep_salt(iadep)%salt(isalt)%rfmo(ist) * w%precip  !kg/ha
            hsaltb_d(j)%salt(isalt)%dryd = atmodep_salt(iadep)%salt(isalt)%drymo(ist) / const  !kg/ha
            !add both to soil profile  
            cs_soil(j)%ly(1)%salt(isalt) = cs_soil(j)%ly(1)%salt(isalt) + (hsaltb_d(j)%salt(isalt)%rain + & 
                hsaltb_d(j)%salt(isalt)%dryd)
					enddo
        end if 
        
        !yearly values
        if (atmodep_cont%timestep == "yr") then
          !loop through the salt ions
          do isalt=1,cs_db%num_salts 
            !add to salt balance arrays
            hsaltb_d(j)%salt(isalt)%rain = .01 * atmodep_salt(iadep)%salt(isalt)%rfyr(ist) * w%precip  !kg/ha
            hsaltb_d(j)%salt(isalt)%dryd = atmodep_salt(iadep)%salt(isalt)%dryyr(ist) / 365.  !kg/ha
            !add both to soil profile  
            cs_soil(j)%ly(1)%salt(isalt) = cs_soil(j)%ly(1)%salt(isalt) + (hsaltb_d(j)%salt(isalt)%rain +   &
               hsaltb_d(j)%salt(isalt)%dryd)
					enddo  
        endif
        
      end if
      
      !annual average values
      if (atmodep_cont%timestep == "aa") then
        !loop through the salt ions
        do isalt=1,cs_db%num_salts 
          !add to salt balance arrays
          hsaltb_d(j)%salt(isalt)%rain = .01 * atmodep_salt(iadep)%salt(isalt)%rf * w%precip  !kg/ha
          hsaltb_d(j)%salt(isalt)%dryd = atmodep_salt(iadep)%salt(isalt)%dry / 365.  !kg/ha
          !add both to soil profile  
          cs_soil(j)%ly(1)%salt(isalt) = cs_soil(j)%ly(1)%salt(isalt) + (hsaltb_d(j)%salt(isalt)%rain +  &
             hsaltb_d(j)%salt(isalt)%dryd)
				enddo
      endif
		
      endif
       
      return
      end subroutine salt_rain