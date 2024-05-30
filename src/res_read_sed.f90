      subroutine res_read_sed
      
      use input_file_module
      use maximum_data_module
      use reservoir_data_module

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads data from the lake water quality input file (.lwq).
!!    This file contains data related to initial pesticide and nutrient levels
!!    in the lake/reservoir and transformation processes occuring within the 
!!    lake/reservoir. Data in the lake water quality input file is assumed to
!!    apply to all reservoirs in the watershed.   

      implicit none

      character (len=80) :: titldum     !             |title of file
      character (len=80) :: header      !             |header of file
      integer :: eof                    !             |end of file
      integer :: imax                   !             |determine max number for array (imax) and total number in file
      logical :: i_exist                !none         |check to determine if file exists
      integer :: ires                   !none         |counter
      
      eof = 0
      imax = 0

      inquire (file=in_res%sed_res,exist=i_exist)
      if (.not. i_exist .or. in_res%sed_res == "null") then
        allocate (res_sed(0:0))
      else
      do
        open (105,file=in_res%sed_res)
        read (105,*,iostat=eof) titldum
        if (eof < 0) exit
        read (105,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof == 0)
            read (105,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1
          end do  
          
       db_mx%res_sed = imax
        
        allocate (res_sed(0:imax))
        rewind (105)
        read (105,*,iostat=eof) titldum
        if (eof < 0) exit
        read (105,*,iostat=eof) header
        if (eof < 0) exit
          
        do ires = 1, imax
          read (105,*,iostat=eof) titldum
          if (eof < 0) exit
          backspace (105)
          read (105,*,iostat=eof) res_sed(ires)
          if (eof < 0) exit
        end do
        exit
      end do
      end if
      close(105)

      return
      end subroutine res_read_sed