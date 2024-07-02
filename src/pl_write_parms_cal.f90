       subroutine pl_write_parms_cal
      
       use maximum_data_module
       use calibration_data_module
       use hydrograph_module
       use input_file_module
       use plant_module
       
       implicit none        
      
       integer :: eof                  !           |end of file
       integer :: mreg                 !none       |end of loop
       integer :: i                    !none       |counter
       integer :: ilum
       integer :: ilum_mx 
       
       mreg = 0
       eof = 0

       open (107,file="plant_parms.cal",recl = 1500)

        write (107,*) "Calibrated plant parameter file"
        write (107,*) db_mx%plcal_reg
        write (107,*) "header"

      do i = 1, db_mx%plcal_reg

        write (107,*) pl_prms(i)%name, pl_prms(i)%lum_num, pl_prms(i)%parms, "  0"  
        write (107,*) "header"

        !! read landscape soft calibration data for each land use and parameter
        if (pl_prms(i)%lum_num > 0) then
          ilum_mx = pl_prms(i)%lum_num * pl_prms(i)%parms
          do ilum = 1, ilum_mx
            select case (pl_prms(i)%prm(ilum)%var)
              case ("pest_stress")
                pl_prms(i)%prm(ilum)%init_val = pl_prms(i)%prm(ilum)%init_val + plcal(i)%lum(ilum)%prm%pest_stress
                pl_prms(i)%prm(ilum)%init_val = amin1 (pl_prms(i)%prm(ilum)%init_val, pl_prms(i)%prm(ilum)%up)
                pl_prms(i)%prm(ilum)%init_val = Max (pl_prms(i)%prm(ilum)%init_val, pl_prms(i)%prm(ilum)%lo)
              case ("epco")
                pl_prms(i)%prm(ilum)%init_val = plcal(i)%lum(ilum-pl_prms(i)%lum_num)%prm%epco
                pl_prms(i)%prm(ilum)%init_val = amin1 (pl_prms(i)%prm(ilum)%init_val, pl_prms(i)%prm(ilum)%up)
                pl_prms(i)%prm(ilum)%init_val = Max (pl_prms(i)%prm(ilum)%init_val, pl_prms(i)%prm(ilum)%lo)
              case ("lai_pot")
                pl_prms(i)%prm(ilum)%init_val = pl_prms(i)%prm(ilum)%init_val + plcal(i)%lum(ilum-2*pl_prms(i)%lum_num)%prm%lai_pot
                pl_prms(i)%prm(ilum)%init_val = amin1 (pl_prms(i)%prm(ilum)%init_val, pl_prms(i)%prm(ilum)%up)
                pl_prms(i)%prm(ilum)%init_val = Max (pl_prms(i)%prm(ilum)%init_val, pl_prms(i)%prm(ilum)%lo)
              case ("harv_idx")
                pl_prms(i)%prm(ilum)%init_val = pl_prms(i)%prm(ilum)%init_val + plcal(i)%lum(ilum-3*pl_prms(i)%lum_num)%prm%harv_idx
                pl_prms(i)%prm(ilum)%init_val = amin1 (pl_prms(i)%prm(ilum)%init_val, pl_prms(i)%prm(ilum)%up)
                pl_prms(i)%prm(ilum)%init_val = Max (pl_prms(i)%prm(ilum)%init_val, pl_prms(i)%prm(ilum)%lo)
            end select
            !! write parameters to file
            write (107,*) pl_prms(i)%prm(ilum)
          end do
        end if 

      end do    !mreg

       close(107)
       return
       end subroutine pl_write_parms_cal