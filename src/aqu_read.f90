       subroutine aqu_read 
      
       use input_file_module
       use aquifer_module
       use basin_module !rtb gwflow
       use maximum_data_module
       
       implicit none
      
       character (len=500) :: header
       character (len=80) :: titldum
       integer :: eof             !                |end of file
       integer :: i               !none            |counter
       integer :: imax            !                |maximum count
       integer :: msh_aqp         !none            |counter
       logical :: i_exist         !                |check to determine if file exists
       integer :: ish_aqp         !none            |counter  
       integer :: k               !                |index
       
       msh_aqp = 0
       eof = 0
       imax = 0

       !! read shallow aquifer property data from aquifer.aqu
       inquire (file=in_aqu%aqu, exist=i_exist)
       if (.not. i_exist .or. in_aqu%aqu == "null") then
            allocate (aqudb(0:0))
          else
       do
          open (107,file=in_aqu%aqu)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
            do while (eof == 0)
              read (107,*,iostat=eof) i
              if (eof < 0) exit
              imax = Max(imax,i)
              msh_aqp = msh_aqp + 1
            end do 
               
          db_mx%aqudb = msh_aqp
          allocate (aqudb(0:imax))
          rewind (107)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          
          do ish_aqp = 1, msh_aqp
            read (107,*,iostat=eof) i
            if (eof < 0) exit
            backspace (107)
            !! read from the aquifer database file named aquifer.aqu
            read (107,*,iostat=eof) k, aqudb(i)
            if (eof < 0) exit
          end do

          close (107)
          exit
          
          bsn_cc%gwflow = 0 ! rtb set gwflow module flag to 0
       enddo
       endif
          
       return
       end subroutine aqu_read         