      subroutine pl_read_regions_cal
   
      use input_file_module
      use maximum_data_module
      use calibration_data_module
      use hydrograph_module
      use hru_module, only : hru
      
      implicit none 

      character (len=80) :: titldum   !           |title of file
      character (len=80) :: header    !           |header of file
      integer :: eof                  !           |end of file
      logical :: i_exist              !none       |check to determine if file exists
      integer :: imax                 !none       |determine max number for array (imax) and total number in file
      integer :: nspu                 !           | 
      integer :: mcal                 !           |
      integer :: mreg                 !           |
      integer :: i                    !none       |counter 
      integer :: isp                  !none       |counter 
      integer :: ielem1               !none       |counter  
      integer :: ihru                 !none       |counter 
      integer :: iihru                !none       |counter 
      integer :: ilum_mx              !           | 
      integer :: ilum                 !none       |counter  
      
      imax = 0
      mcal = 0
      mreg = 0
 
    inquire (file=in_chg%plant_gro_sft, exist=i_exist)
    if (.not. i_exist .or. in_chg%plant_gro_sft == "null" ) then
      allocate (plcal(0:0))	
    else
      do
        open (107,file=in_chg%plant_gro_sft)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) mreg
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        allocate (plcal(mreg))

      do i = 1, mreg

        read (107,*,iostat=eof) plcal(i)%name, plcal(i)%lum_num, nspu       
        if (eof < 0) exit
        if (nspu > 0) then
          allocate (elem_cnt(nspu))
          backspace (107)
          read (107,*,iostat=eof) plcal(i)%name, plcal(i)%lum_num,  nspu, (elem_cnt(isp), isp = 1, nspu)
          if (eof < 0) exit

          call define_unit_elements (nspu, ielem1)
          
          allocate (plcal(i)%num(ielem1))
          plcal(i)%num = defunit_num
          plcal(i)%num_tot = ielem1
          do ihru = 1, plcal(i)%num_tot
            iihru = plcal(i)%num(ihru)
            hru(iihru)%crop_reg = i
          end do
          deallocate (defunit_num)
        else
          !!all hrus are in region
          allocate (plcal(i)%num(sp_ob%hru))
          plcal(i)%num_tot = sp_ob%hru
          do ihru = 1, sp_ob%hru
            plcal(i)%num(ihru) = ihru
            hru(ihru)%crop_reg = i
          end do      
        end if
        
        !! read landscape soft calibration data for each land use
        !read (107,*,iostat=eof) header
        !if (eof < 0) exit
        if (plcal(i)%lum_num > 0) then
          ilum_mx = plcal(i)%lum_num
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          allocate (plcal(i)%lum(ilum_mx))
          do ilum = 1, ilum_mx
            read (107,*,iostat=eof) plcal(i)%lum(ilum)%meas
            if (eof < 0) exit
          end do
        end if 

      end do    !mreg
      exit
         
      end do 
      end if	  
        
      db_mx%plcal_reg = mreg
	  
      close(107)
      return
      end subroutine pl_read_regions_cal