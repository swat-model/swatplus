      subroutine calsoft_plant_zero
    
      use calibration_data_module
      use maximum_data_module
      
      implicit none

      integer :: ireg               !none       |calibration region number 
      integer :: ilum               !           |region land use number

      do ireg = 1, db_mx%plcal_reg
        do ilum = 1, plcal(ireg)%lum_num
          plcal(ireg)%lum(ilum)%nbyr = 0
          plcal(ireg)%lum(ilum)%precip_aa = 0.
          plcal(ireg)%lum(ilum)%aa = plcal_z
          plcal(ireg)%lum(ilum)%sim%yield = 0.
          plcal(ireg)%lum(ilum)%ha = 0.
        end do
      end do

      return
      end subroutine calsoft_plant_zero