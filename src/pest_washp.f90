      subroutine pest_washp

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates the amount of pesticide washed off the plant
!!    foliage and onto the soil

      use pesticide_data_module
      use output_ls_pesticide_module
      use hru_module, only : hru, ihru
      use soil_module
      use constituent_mass_module
      use plant_module
      
      implicit none       
      
      integer :: j        !none          |HRU number
      integer :: k        !none          |counter
      integer :: ipest_db !none          |pesticide number from pest.dat
      real :: pest_soil   !kg/ha         |amount of pesticide in soil   
      integer :: icmd     !              |

      j = ihru

      if (cs_db%num_pests == 0) return

      do k = 1, cs_db%num_pests
        ipest_db = cs_db%pest_num(k)
        if (cs_pl(j)%pest(k) >= 0.0001) then
          if (ipest_db > 0) then
            pest_soil = pestdb(ipest_db)%washoff * cs_pl(j)%pest(k)
            if (pest_soil > cs_pl(j)%pest(k)) pest_soil = cs_pl(j)%pest(k)
            cs_soil(j)%ly(1)%pest(k) = cs_soil(j)%ly(1)%pest(k) + pest_soil
            cs_pl(j)%pest(k) = cs_pl(j)%pest(k) - pest_soil
            hpestb_d(j)%pest(k)%wash = pest_soil
          end if
        end if
      end do

      return
      end subroutine pest_washp