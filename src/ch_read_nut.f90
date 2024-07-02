      subroutine ch_read_nut
      
      use input_file_module
      use basin_module
      use time_module
      use maximum_data_module
      use channel_data_module

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads data from the lake water quality input file (.lwq).
!!    This file contains data related to initial pesticide and nutrient levels
!!    in the lake/reservoir and transformation processes occuring within the 
!!    lake/reservoir. Data in the lake water quality input file is assumed to
!!    apply to all reservoirs in the watershed.     

      implicit none

      integer :: eof                   !          |end of file
      integer :: imax                  !units     |description
      character (len=80) :: titldum    !          |title of file
      character (len=80) :: header     !          |header of file
      logical :: i_exist               !          |check to determine if file exists
      integer :: ich                   !none      |counter

      eof = 0
      imax = 0

      inquire (file=in_cha%nut,exist=i_exist)
      if (.not. i_exist .or. in_cha%nut == "null") then
        allocate (ch_nut(0:0))
      else
      do
        open (105,file=in_cha%nut)
        read (105,*,iostat=eof) titldum
        if (eof < 0) exit
        read (105,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof == 0)
            read (105,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1
          end do
          
        db_mx%ch_nut = imax  
        
        allocate (ch_nut(0:imax))
        rewind (105)
        read (105,*,iostat=eof) titldum
        if (eof < 0) exit
        read (105,*,iostat=eof) header
        if (eof < 0) exit
         
        do ich = 1, db_mx%ch_nut
          read (105,*,iostat=eof) titldum
          if (eof < 0) exit
          backspace (105)
          read (105,*,iostat=eof) ch_nut(ich)
          if (eof < 0) exit
          
!!    set default values for undefined parameters

      if (ch_nut(ich)%lao <= 0) ch_nut(ich)%lao = 2
      if (ch_nut(ich)%igropt <= 0) ch_nut(ich)%igropt = 2
      if (ch_nut(ich)%ai0 <= 0.) ch_nut(ich)%ai0 = 50.
      if (ch_nut(ich)%ai1 <= 0.) ch_nut(ich)%ai1 = 0.08
      if (ch_nut(ich)%ai2 <= 0.) ch_nut(ich)%ai2 = 0.015
      if (ch_nut(ich)%ai3 <= 0.) ch_nut(ich)%ai3 = 1.60
      if (ch_nut(ich)%ai4 <= 0.) ch_nut(ich)%ai4 = 2.0
      if (ch_nut(ich)%ai5 <= 0.) ch_nut(ich)%ai5 = 3.5
      if (ch_nut(ich)%ai6 <= 0.) ch_nut(ich)%ai6 = 1.07
      if (ch_nut(ich)%mumax <= 0.) ch_nut(ich)%mumax = 2.0
      if (ch_nut(ich)%rhoq <= 0.) ch_nut(ich)%rhoq = 2.5      !! previous 0.3
      if (ch_nut(ich)%tfact <= 0.) ch_nut(ich)%tfact = 0.3
      if (ch_nut(ich)%k_l <= 0.) ch_nut(ich)%k_l = 0.75
      if (ch_nut(ich)%k_n <= 0.) ch_nut(ich)%k_n = 0.02
      if (ch_nut(ich)%k_p <= 0.) ch_nut(ich)%k_p = 0.025
      if (ch_nut(ich)%lambda0 <= 0.) ch_nut(ich)%lambda0 = 1.0
      if (ch_nut(ich)%lambda1 <= 0.) ch_nut(ich)%lambda1 = 0.03
      if (ch_nut(ich)%lambda2 <= 0.) ch_nut(ich)%lambda2 = 0.054
      if (ch_nut(ich)%p_n <= 0.) ch_nut(ich)%p_n = 0.5
      
!! convert units on k_l:read in as kJ/(m2*min), use as MJ/(m2*hr)
      ch_nut(ich)%k_l = ch_nut(ich)%k_l * 1.e-3 * 60.

!! change units from day to hour if hourly (subdaily) routing is performed
      !if (time%step > 0) then
        ch_nut(ich)%mumax = ch_nut(ich)%mumax / real(time%step)
        ch_nut(ich)%rhoq = ch_nut(ich)%rhoq / real(time%step)
      !end if
     
!!    set default values for undefined parameters
      if (ch_nut(ich)%rs1 <= 0.) ch_nut(ich)%rs1 = 1.0
      if (ch_nut(ich)%rs2 <= 0.) ch_nut(ich)%rs2 = 0.05
      if (ch_nut(ich)%rs3 <= 0.) ch_nut(ich)%rs3 = 0.5
      if (ch_nut(ich)%rs4 <= 0.) ch_nut(ich)%rs4 = 0.05
      if (ch_nut(ich)%rs5 <= 0.) ch_nut(ich)%rs5 = 0.05
      if (ch_nut(ich)%rs6 <= 0.) ch_nut(ich)%rs6 = 2.5
      if (ch_nut(ich)%rs7 <= 0.) ch_nut(ich)%rs7 = 2.5
      if (ch_nut(ich)%rk1 <= 0.) ch_nut(ich)%rk1 = 1.71
      if (ch_nut(ich)%rk2 <= 0.) ch_nut(ich)%rk2 = 1.0    ! previous 50.0
      if (ch_nut(ich)%rk4 <= 0.) ch_nut(ich)%rk4 = 2.0
      if (ch_nut(ich)%rk5 <= 0.) ch_nut(ich)%rk5 = 2.0
      if (ch_nut(ich)%rk6 <= 0.) ch_nut(ich)%rk6 = 1.71
      if (ch_nut(ich)%bc1 <= 0.) ch_nut(ich)%bc1 = 0.55 
      if (ch_nut(ich)%bc2 <= 0.) ch_nut(ich)%bc2 = 1.1
      if (ch_nut(ich)%bc3 <= 0.) ch_nut(ich)%bc3 = 0.21
      if (ch_nut(ich)%bc4 <= 0.) ch_nut(ich)%bc4 = 0.35
      
!! change units from day to hour if hourly routing is performed
      !if (time%step > 0) then
        ch_nut(ich)%rs1 = ch_nut(ich)%rs1 / real(time%step)
        ch_nut(ich)%rs2 = ch_nut(ich)%rs2 / real(time%step)
        ch_nut(ich)%rs3 = ch_nut(ich)%rs3 / real(time%step)
        ch_nut(ich)%rs4 = ch_nut(ich)%rs4 / real(time%step)
        ch_nut(ich)%rs5 = ch_nut(ich)%rs5 / real(time%step)
        ch_nut(ich)%rk1 = ch_nut(ich)%rk1 / real(time%step)
        ch_nut(ich)%rk2 = ch_nut(ich)%rk2 / real(time%step)
        ch_nut(ich)%rk3 = ch_nut(ich)%rk3 / real(time%step)
        ch_nut(ich)%rk4 = ch_nut(ich)%rk4 / real(time%step)
        ch_nut(ich)%bc1 = ch_nut(ich)%bc1 / real(time%step)
        ch_nut(ich)%bc2 = ch_nut(ich)%bc2 / real(time%step)
        ch_nut(ich)%bc3 = ch_nut(ich)%bc3 / real(time%step)
        ch_nut(ich)%bc4 = ch_nut(ich)%bc4 / real(time%step)
      !end if
        end do
        exit
      enddo
      endif
      close(105)

      return
      end subroutine ch_read_nut