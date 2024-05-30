       subroutine basin_read_cc
      
       use input_file_module
       use basin_module
      
       implicit none
       
       character (len=80) :: titldum    !             |title of file
       character (len=80) :: header     !             |header
       integer :: eof                   !             |end of file
       logical :: i_exist               !             |check to determine if file exists
       
       eof = 0
      
       !! read basin
       inquire (file=in_basin%codes_bas, exist=i_exist)
       if (i_exist .or. in_basin%codes_bas /= "null") then      
       do 
         open (107,file=in_basin%codes_bas)
         read (107,*,iostat=eof) titldum
         if (eof < 0) exit
         read (107,*,iostat=eof) header
         if (eof < 0) exit
         read (107,*,iostat=eof) bsn_cc
         if (eof < 0) exit
         exit
       enddo
       endif
       
       if (bsn_cc%pet == 3) then 
        open (140,file = 'pet.cli')
       do
        read (140,*,iostat=eof) titldum
        if (eof < 0) exit
        read (140,*,iostat=eof) header
        if (eof < 0) exit
        read (140,*,iostat = eof) titldum
        exit
       end do
       end if

    
       close(107)
       
       return
      end subroutine basin_read_cc