       subroutine scen_read_grwway
      
       use input_file_module
       use maximum_data_module
       use mgt_operations_module
       
       implicit none       
      
       character (len=80) :: titldum   !           |title of file
       character (len=80) :: header    !           |header of file
       integer :: eof                  !           |end of file
       integer :: imax                 !none       |determine max number for array (imax) and total number in file
       logical :: i_exist              !none       |check to determine if file exists
       integer :: igrwwop              !none       |counter
       
       eof = 0
       imax = 0
      
       !! read grass waterways operations
       inquire (file=in_str%grassww_str, exist=i_exist)
       if (.not. i_exist .or. in_str%grassww_str == "null") then
         allocate (grwaterway_db(0:0))
       else
       do
         open (107,file=in_str%grassww_str)
         read (107,*,iostat=eof) titldum
         if (eof < 0) exit
         read (107,*,iostat=eof) header
         if (eof < 0) exit
         do while (eof == 0)
           read (107,*,iostat=eof) titldum
           if (eof < 0) exit
           imax = imax + 1
         end do
         
         allocate (grwaterway_db(0:imax))
         
         rewind (107)
         read (107,*,iostat=eof) titldum
         if (eof < 0) exit
         read (107,*,iostat=eof) header
         if (eof < 0) exit
         
         do igrwwop = 1, imax
           read (107,*,iostat=eof) grwaterway_db(igrwwop)          
           if (eof < 0) exit
         end do

         exit
       enddo
       endif
       
       db_mx%grassop_db = imax
       close(107)
       return         
      end subroutine scen_read_grwway