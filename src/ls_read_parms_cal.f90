       subroutine ls_read_lsparms_cal
      
       use maximum_data_module
       use calibration_data_module
       use input_file_module
       
       implicit none
      
       character (len=80) :: titldum   !           |title of file
       character (len=80) :: header    !           |header of file
       integer :: eof                  !           |end of file
       logical :: i_exist              !none       |check to determine if file exists
       integer :: mlsp                 !none       |end of loop
       integer :: i                    !none       |counter
       
       eof = 0

      inquire (file=in_chg%wb_parms_sft, exist=i_exist)
      if (.not. i_exist .or. in_chg%wb_parms_sft == "null") then
        allocate (ls_prms(0:0))	   	   
      else   
       do 
         open (107,file = in_chg%wb_parms_sft)
         read (107,*,iostat=eof) titldum
         if (eof < 0) exit
         read (107,*,iostat=eof) mlsp
         if (eof < 0) exit
         read (107,*,iostat=eof) header
         allocate (ls_prms(mlsp))
         if (eof < 0) exit
         exit
       enddo
           
       db_mx%lscal_prms = mlsp
      
       do i = 1, mlsp
         read (107,*,iostat=eof) ls_prms(i)%name, ls_prms(i)%chg_typ, ls_prms(i)%neg, ls_prms(i)%pos, ls_prms(i)%lo, ls_prms(i)%up
         if (eof < 0) exit 
       end do

      end if	   

      close(107)
      return
      end subroutine ls_read_lsparms_cal