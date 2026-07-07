      subroutine pest_soil_tot

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates the total amount of pesticide in the soil

      use pesticide_data_module
      use hru_module, only : hru, ihru
      use soil_module
      use constituent_mass_module
      use output_ls_pesticide_module
      use plant_module
      
      implicit none       
      
      integer :: j        !none          |HRU number
      integer :: k        !none          |sequential pesticide number 
      integer :: ly       !none          |soil layer   

      j = ihru

      if (cs_db%num_pests == 0) return

      do k = 1, cs_db%num_pests
        hpestb_d(j)%pest(k)%plant = cs_pl(j)%pest(k)    !store for each plant in future
        hpestb_d(j)%pest(k)%soil = 0.
        do ly = 1, soil(j)%nly
          hpestb_d(j)%pest(k)%soil = hpestb_d(j)%pest(k)%soil + cs_soil(j)%ly(ly)%pest(k)
        end do
      end do

      return
      end subroutine pest_soil_tot