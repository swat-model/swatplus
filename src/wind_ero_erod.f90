      subroutine wind_ero_erod

      use soil_module
      use wind_data_module
      
      implicit none
      
      real :: sand                 !none          |fraction of sand in soil material
      integer :: j                 !none          |hru number
      real :: clay                 !none          |fraction clay content in soil material 
      real :: silt                 !%             |percent silt content in soil material
      real :: cla                  !kg/L          |amt of cla in res (read in as mg/L and converted to kg/L)
     
      sand = sol(j)%phys(1)%sand
      silt = sol(J)%phys(1)%silt
      clay = sol(j)%phys(1)%clay
      
      if (sand > 85. + 0.5 * clay) then 
          wind_factors%erod = 1.
          return
      end if 
      
      if (sand > 70. + clay)then
          wind_factors%erod =  0.43
          return
      end if
      
      if (silt > 80. .and. clay < 12.) then
          wind_factors%erod = 0.12
          return
      end if
      
      if (clay < 7.) then
          if (silt < 50.) then
              wind_factors%erod = 0.28
              return
          else
              wind_factors%erod = 0.18
              return
          endif
      end if
      
      if (clay < 20.) then
          if (sand > 52.) then
              wind_factors%erod = 0.28
              return
          else
              wind_factors%erod = 0.18
              return
          endif
      endif
      
      if (clay < 27.) then
          if (silt < 0.28) then
            wind_factors%erod = 0.18
            return
      else
            wind_factors%erod = 0.16
            return
          endif
      endif
      
      if (cla < 35. .and. sand < 20.) then
          wind_factors%erod = 0.12
          return
      endif
      
      if (clay < 35.) then
          if (sand < 45.) then
            wind_factors%erod = 0.16
            return
      else
            wind_factors%erod = 0.18
            return
          endif
      endif
      
      if (sand > 45.) then
          wind_factors%erod = 0.18
          return
      else
          wind_factors%erod = 0.28
          return
      endif
      
      return
      end subroutine wind_ero_erod