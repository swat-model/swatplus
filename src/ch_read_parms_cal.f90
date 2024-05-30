       subroutine ch_read_parms_cal
      
       use calibration_data_module
       use input_file_module
      
      implicit none
       
      character (len=80) :: titldum    !             |title of file
      character (len=80) :: header     !             |header of file
      integer :: eof                   !             |end of file
      logical :: i_exist               !             |check to determine if file exists
      integer :: mchp                  !             |ending of loop
      integer :: i                     !none         |counter 
       
       eof = 0
       
      inquire (file=in_chg%ch_sed_parms_sft, exist=i_exist)
      if (.not. i_exist .or. in_chg%ch_sed_parms_sft == "null") then
           allocate (ch_prms(0:0))         
      else    
        do 
          open (107,file=in_chg%ch_sed_parms_sft)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) mchp
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          allocate (ch_prms(mchp))
          exit
        enddo
       
        do i = 1, mchp
          read (107,*,iostat=eof) ch_prms(i)%name, ch_prms(i)%chg_typ, ch_prms(i)%neg, ch_prms(i)%pos, ch_prms(i)%lo, ch_prms(i)%up
          if (eof < 0) exit 
        end do 
    
      endif
      
      close(107)
      return
      end subroutine ch_read_parms_cal