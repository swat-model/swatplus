      subroutine cal_parm_read
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this function computes new paramter value based on 
!!    user defined change

      use input_file_module
      use maximum_data_module
      use calibration_data_module
      
      implicit none

      character (len=80) :: titldum                        !         |title of file
      character (len=80) :: header                         !         |header of file
      integer :: eof                                       !         |end of file
      integer :: imax                                      !         |determine max number for array (imax) and total number in file
      integer :: mchg_par                                  !         |
      logical :: i_exist                                   !         |check to determine if file exists
      integer :: i                                         !none     |counter
            
      imax = 0
      mchg_par = 0
        
      !!read parameter change values for calibration
      inquire (file=in_chg%cal_parms, exist=i_exist)
      if (.not. i_exist .or. in_chg%cal_parms == "null") then
        allocate (cal_parms(0:0))
      else
        do
          open (107,file=in_chg%cal_parms)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) mchg_par
          if (eof < 0) exit
          
          allocate (cal_parms(0:mchg_par))

          read (107,*,iostat=eof) header
          if (eof < 0) exit

          do i = 1, mchg_par
            read (107,*,iostat=eof) cal_parms(i)
            if (eof < 0) exit
          end do
          exit
        end do
      end if         
     
      db_mx%cal_parms = mchg_par
      
      return
      end subroutine cal_parm_read