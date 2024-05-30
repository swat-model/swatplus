      subroutine cs_rain !rtb cs
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine adds constituent mass from atmospheric deposition (rainfall, dry) to the soil profile

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
      use cs_module
      use constituent_mass_module
      
      implicit none

      integer :: iadep            !            |
      integer :: j                !none        |counter
      integer :: iob              !            |
      integer :: ist              !            |
      integer :: ics              !            |constituent counter
      real :: const               !            |constant used for rate, days, etc

      j = ihru
      
      if(cs_db%num_cs > 0) then
      
      iob = hru(j)%obj_no
      iwst = ob(iob)%wst
      iadep = wst(iwst)%wco%atmodep
      ist = atmodep_cont%ts
        
      !add constituent mass in atmospheric deposition to the soil profile
      !atmospheric deposition = rainfall (mg/L) + dry deposition (kg/ha)
      !for rainfall: (mg/l*mm) * kg/1,000,000 mg *1,00 l/m3 * m3/1,000 mm * 10,000 m2/ha = 0.01
      if (ist > 0 .and. ist <= atmodep_cont%num) then
      
        !monthly values
        if (atmodep_cont%timestep == "mo") then
          const = float (ndays(time%mo + 1) - ndays(time%mo))
          !loop through the constituents
          do ics=1,cs_db%num_cs 
            !add to constituent balance arrays
            hcsb_d(j)%cs(ics)%rain = .01 * atmodep_cs(iadep)%cs(ics)%rfmo(ist) * w%precip  !kg/ha
            hcsb_d(j)%cs(ics)%dryd = atmodep_cs(iadep)%cs(ics)%drymo(ist) / const  !kg/ha
            !add both to soil profile  
            cs_soil(j)%ly(1)%cs(ics) = cs_soil(j)%ly(1)%cs(ics) + (hcsb_d(j)%cs(ics)%rain + hcsb_d(j)%cs(ics)%dryd)
					enddo
        end if 
        
        !yearly values
        if (atmodep_cont%timestep == "yr") then
          !loop through the constituents
          do ics=1,cs_db%num_cs 
            !add to constituent balance arrays
            hcsb_d(j)%cs(ics)%rain = .01 * atmodep_cs(iadep)%cs(ics)%rfyr(ist) * w%precip  !kg/ha
            hcsb_d(j)%cs(ics)%dryd = atmodep_cs(iadep)%cs(ics)%dryyr(ist) / 365.  !kg/ha
            !add both to soil profile  
            cs_soil(j)%ly(1)%cs(ics) = cs_soil(j)%ly(1)%cs(ics) + (hcsb_d(j)%cs(ics)%rain + hcsb_d(j)%cs(ics)%dryd)
					enddo  
        endif
        
      end if
      
      !annual average values
      if (atmodep_cont%timestep == "aa") then
        !loop through the constituents
        do ics=1,cs_db%num_cs 
          !add to constituent balance arrays
          hcsb_d(j)%cs(ics)%rain = .01 * atmodep_cs(iadep)%cs(ics)%rf * w%precip  !kg/ha
          hcsb_d(j)%cs(ics)%dryd = atmodep_cs(iadep)%cs(ics)%dry / 365.  !kg/ha
          !add both to soil profile  
          cs_soil(j)%ly(1)%cs(ics) = cs_soil(j)%ly(1)%cs(ics) + (hcsb_d(j)%cs(ics)%rain + hcsb_d(j)%cs(ics)%dryd)
				enddo
      endif
		
      endif
      
      
      return
      end subroutine cs_rain