       subroutine pl_read_parms_cal
      
       use maximum_data_module
       use calibration_data_module
       use hydrograph_module
       use hru_module, only : hru
       use input_file_module
       use plant_module
       
       implicit none        
      
       character (len=80) :: titldum   !           |title of file
       character (len=80) :: header    !           |header of file
       integer :: eof                  !           |end of file
       logical :: i_exist              !none       |check to determine if file exists
       integer :: mreg                 !none       |end of loop
       integer :: i                    !none       |counter
       integer :: ilum
       integer :: ilum_mx
       integer :: isp                  !none       |counter 
       integer :: ielem1               !none       |counter 
       integer :: iihru                !none       |counter  
       integer :: ihru                 !none       |counter 
       integer :: nspu                 !           | 
       integer :: ipl                  !           | 
       
       mreg = 0
       eof = 0

       inquire (file=in_chg%plant_parms_sft, exist=i_exist)
       if (.not. i_exist .or. in_chg%plant_parms_sft == "null") then
        allocate (pl_prms(0:0))	   	   
       else   
      do
        open (107,file=in_chg%plant_parms_sft)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) mreg
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        allocate (pl_prms(mreg))

      do i = 1, mreg

        read (107,*,iostat=eof) pl_prms(i)%name, pl_prms(i)%lum_num, pl_prms(i)%parms, nspu       
        if (eof < 0) exit
        if (nspu > 0) then
          allocate (elem_cnt(nspu))
          backspace (107)
          read (107,*,iostat=eof) pl_prms(i)%name, pl_prms(i)%lum_num,  nspu, (elem_cnt(isp), isp = 1, nspu)
          if (eof < 0) exit

          call define_unit_elements (nspu, ielem1)
          
          allocate (pl_prms(i)%num(ielem1))
          pl_prms(i)%num = defunit_num
          pl_prms(i)%num_tot = ielem1
          do ihru = 1, pl_prms(i)%num_tot
            iihru = pl_prms(i)%num(ihru)
            hru(iihru)%crop_reg = i
          end do
          deallocate (defunit_num)
        else
          !!all hrus are in region
          allocate (pl_prms(i)%num(sp_ob%hru))
          pl_prms(i)%num_tot = sp_ob%hru
          do ihru = 1, sp_ob%hru
            pl_prms(i)%num(ihru) = ihru
            hru(ihru)%crop_reg = i
          end do      
        end if
        
        !! read landscape soft calibration data for each land use and parameter
        if (pl_prms(i)%lum_num > 0) then
          ilum_mx = pl_prms(i)%lum_num * pl_prms(i)%parms
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          allocate (pl_prms(i)%prm(ilum_mx))
          do ilum = 1, ilum_mx
            read (107,*,iostat=eof) pl_prms(i)%prm(ilum)
            if (eof < 0) exit
          end do
        end if 

        !! set parms for each plant
        ilum_mx = pl_prms(i)%lum_num * pl_prms(i)%parms
        do ilum = 1, ilum_mx
          do ihru = 1, pl_prms(i)%num_tot
            iihru = pl_prms(i)%num(ihru)
            do ipl = 1, pcom(iihru)%npl
              if (pl_prms(i)%prm(ilum)%name == pcom(iihru)%pl(ipl)) then
                select case (pl_prms(i)%prm(ilum)%var)
                case ("epco")
                  pcom(iihru)%plcur(ipl)%epco = pl_prms(i)%prm(ilum)%init_val
                case ("pest_stress")
                  pcom(iihru)%plcur(ipl)%pest_stress = pl_prms(i)%prm(ilum)%init_val
                case ("lai_pot")
                  pcom(iihru)%plcur(ipl)%lai_pot = pl_prms(i)%prm(ilum)%init_val
                case ("harv_idx")
                  pcom(iihru)%plcur(ipl)%harv_idx = pl_prms(i)%prm(ilum)%init_val
                end select
              end if
            end do
          end do
        end do
        
      end do    !mreg
      exit
         
      end do 
      end if	  
        
      db_mx%plcal_reg = mreg
	  
       close(107)
       return
       end subroutine pl_read_parms_cal