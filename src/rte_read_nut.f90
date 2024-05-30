      subroutine rte_read_nut

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads data from the lake water quality input file (.lwq).
!!    This file contains data related to initial pesticide and nutrient levels
!!    in the lake/reservoir and transformation processes occuring within the 
!!    lake/reservoir. Data in the lake water quality input file is assumed to
!!    apply to all reservoirs in the watershed.    

      use channel_data_module
      
      implicit none      
      
      character (len=80) :: titldum     !             |title of file
      character (len=80) :: header      !             |header of file
      integer :: eof                    !             |end of file
      integer :: imax                   !             |determine max number for array (imax) and total number in file
      logical :: i_exist                !none         |check to determine if file exists
      integer :: ich                    !none         |counter

      eof = 0
      imax = 0

      inquire (file="nutrients.rte",exist=i_exist)
      if (.not. i_exist) then
        allocate (rte_nut(0:0))
      else
      do
        open (105,file="nutrients.rte")
        read (105,*,iostat=eof) titldum
        if (eof < 0) exit
        read (105,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof == 0)
            read (105,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1
          end do        
        
        allocate (rte_nut(0:imax))
        rewind (105)
        read (105,*,iostat=eof) titldum
        if (eof < 0) exit
        read (105,*,iostat=eof) header
        if (eof < 0) exit
          
        do ich = 1, imax
          read (105,*,iostat=eof) titldum
          if (eof < 0) exit
          backspace (105)
          read (105,*,iostat=eof) rte_nut(ich)
          if (eof < 0) exit
        end do
        exit
      enddo
      endif
      close(105)

      return
      end subroutine rte_read_nut