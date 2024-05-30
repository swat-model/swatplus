      subroutine pest_pl_up

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates the amount of pesticide washed off the plant
!!    foliage and onto the soil

      use pesticide_data_module
      use output_ls_pesticide_module
      use hru_module, only :  ihru
      use soil_module
      use constituent_mass_module
      use plant_module
      
      implicit none       
      
      integer :: j        !none          |HRU number
      integer :: k        !none          |pesticide number
      integer :: ly       !none          |soil layer number
      integer :: ipl      !none          |plant number
      integer :: ipest_db !none          |pesticide number from pest.dat
      real :: pest_up     !kg/ha         |amount of pesticide in soil   

      j = ihru

      if (cs_db%num_pests == 0) return

      do k = 1, cs_db%num_pests
        ipest_db = cs_db%pest_num(k)
        hpestb_d(j)%pest(k)%pl_uptake = 0.
	    !! adjust foliar pesticide for wash off
        do ipl = 1, pcom(j)%npl
          if (cs_pl(j)%pl_on(ipl)%pest(k) >= 0.0001) then
            if (ipest_db > 0) then
              do ly = 1, soil(j)%nly
                pest_up = pestdb(ipest_db)%pl_uptake * pcom(j)%plcur(ipl)%uptake(ly) / soil(j)%phys(k)%st   &
                                                                                * cs_soil(j)%ly(ly)%pest(k)
                if (pest_up > cs_soil(j)%ly(ly)%pest(k)) pest_up = cs_soil(j)%ly(ly)%pest(k)
                cs_soil(j)%ly(ly)%pest(k) = cs_soil(j)%ly(ly)%pest(k) - pest_up
                cs_pl(j)%pl_in(ipl)%pest(k) = cs_pl(j)%pl_in(ipl)%pest(k) + pest_up
                hpestb_d(j)%pest(k)%pl_uptake = hpestb_d(j)%pest(k)%pl_uptake + pest_up
              end do
            end if
          end if 
        end do
      end do

      return
      end subroutine pest_pl_up