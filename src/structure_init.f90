      subroutine structure_init

      use hydrograph_module, only : sp_ob
      use hru_module, only : hru
      use landuse_data_module
      
      implicit none
      
      integer :: j                   !none       |counter
      integer :: ilum                !none       |counter 
      
      do j = 1, sp_ob%hru
        ilum = hru(j)%land_use_mgt
           
        !! set parameters for structural land use/managment
        if (lum(ilum)%tiledrain /= "null") then
          call structure_set_parms("tiledrain       ", lum_str(ilum)%tiledrain, j)
        end if
      
        if (lum(ilum)%fstrip /= "null") then
          call structure_set_parms("fstrip          ", lum_str(ilum)%fstrip, j)
        end if
        
        if (lum(ilum)%grassww /= "null") then
          call structure_set_parms("grassww         ", lum_str(ilum)%grassww, j)
        end if

        if (lum(ilum)%bmpuser /= "null") then
          call structure_set_parms("user_def         ", lum_str(ilum)%bmpuser, j)
        end if
        
      end do

    return
    end subroutine structure_init