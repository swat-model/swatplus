      subroutine sep_read
      
      use input_file_module
      use maximum_data_module
      use septic_data_module
      
      implicit none
  
      character (len=80) :: titldum   !           |title of file
      character (len=80) :: header    !           |header of file
      integer :: eof                  !           |end of file
      integer :: imax                 !none       |determine max number for array (imax) and total number in file
      logical :: i_exist              !none       |check to determine if file exists
      integer :: isep                 !none       |counter
      
      eof = 0
      imax = 0
      
      inquire (file=in_str%septic_str,exist=i_exist)                  
      if (.not. i_exist .or. in_str%septic_str == "null") then 
        allocate (sep(0:0)) 
      else
        do 
          open (172,file=in_str%septic_str)
          read (172,*,iostat=eof) titldum
          if (eof < 0) exit
          read (172,*,iostat=eof) header
          if (eof < 0) exit
          do while (eof == 0)
            read (172,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1
          end do
          
          allocate (sep(0:imax))
          rewind (172)
          read (172,*,iostat=eof) titldum
          if (eof < 0) exit
          read (172,*,iostat=eof) header 
          if (eof < 0) exit
                
          do isep = 1, imax
            read(172,*,iostat=eof) sep(isep)        
            if (eof < 0) exit
          end do    
          exit
        enddo
        end if
 
      close(172)
      
      db_mx%septic = imax
      
      return  
      end subroutine sep_read