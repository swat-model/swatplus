      subroutine res_read_nut
      
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

      inquire (file=in_res%nut_res,exist=i_exist)
      if (.not. i_exist .or. in_res%nut_res == "null") then
        allocate (res_nut(0:0))
      else
      do
        open (105,file=in_res%nut_res)
        read (105,*,iostat=eof) titldum
        if (eof < 0) exit
        read (105,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof == 0)
            read (105,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1
          end do   
          
        db_mx%res_nut = imax
        
        allocate (res_nut(0:imax))
        rewind (105)
        read (105,*,iostat=eof) titldum
        if (eof < 0) exit
        read (105,*,iostat=eof) header
        if (eof < 0) exit
          
        do ires = 1, imax
          read (105,*,iostat=eof) titldum
          if (eof < 0) exit
          backspace (105)
          read (105,*,iostat=eof) res_nut(ires)
          if (eof < 0) exit
        end do
        exit
      enddo
      endif
      close(105)
      
      !! convert units
      do ires = 1, imax
        res_nut(ires)%psetlr1 = res_nut(ires)%psetlr1 / 365.         !m/yr -> m/day
        res_nut(ires)%psetlr2 = res_nut(ires)%psetlr2 / 365.
        res_nut(ires)%nsetlr1 = res_nut(ires)%nsetlr1 / 365.
        res_nut(ires)%nsetlr2 = res_nut(ires)%nsetlr2 / 365.
      end do

      return
      end subroutine res_read_nut