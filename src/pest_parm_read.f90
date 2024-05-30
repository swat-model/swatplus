      subroutine pest_parm_read
      
      use basin_module
      use input_file_module
      use maximum_data_module
      use pesticide_data_module
      
      implicit none

      character (len=80) :: titldum   !           |title of file
      character (len=80) :: header    !           |header of file
      integer :: eof                  !           |end of file
      integer :: imax                 !none       |determine max number for array (imax) and total number in file
      logical :: i_exist              !none       |check to determine if file exists
      integer :: ip                   !none       |counter 
      
      eof = 0
      imax = 0
      
      inquire (file=in_parmdb%pest, exist=i_exist)
      if (.not. i_exist .or. in_parmdb%pest == "null") then
        allocate (pestdb(0:0))
        allocate (pestcp(0:0))
      else
      do
        open (106,file=in_parmdb%pest)
        read (106,*,iostat=eof) titldum
        if (eof < 0) exit
        read (106,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof == 0)
            read (106,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1
          end do
          
        allocate (pestdb(0:imax))
        allocate (pestcp(0:imax))

        rewind (106)
        read (106,*,iostat=eof) titldum
        if (eof < 0) exit
        read (106,*,iostat=eof) header
        if (eof < 0) exit
        
        do ip = 1, imax
           read (106,*,iostat=eof) pestdb(ip)
           if (eof < 0) exit
      
          !! calculations: the first-order rate law for the decay of pesticides
          !! is dP/dt = -kP where P is the amount of pesticide, 
          !! t is the time and k is the rate constant for degradation. To calculate
          !! the amount of pesticide at any time after application the equation
          !! P(t) = P_o*Exp(-kt) is used where P_o is the original amount of 
          !! pesticide. k can be calculate with the equation k = 0.693/hlife.
          !! decay_f or decay_s = Exp(-k)
          if (pestdb(ip)%foliar_hlife > 0.) then
            pestcp(ip)%decay_f = Exp(-.693 / pestdb(ip)%foliar_hlife)
          else
            pestcp(ip)%decay_f = 0.
          endif
          if (pestdb(ip)%soil_hlife > 0.) then
            pestcp(ip)%decay_s = Exp(-.693 / pestdb(ip)%soil_hlife)
          else
            pestcp(ip)%decay_s = 0.
          endif
          
          if (pestdb(ip)%aq_hlife > 0.) then
            pestcp(ip)%decay_a = Exp(-.693 / pestdb(ip)%aq_hlife)
          else
            pestcp(ip)%decay_a = 0.
          endif
          if (pestdb(ip)%ben_hlife > 0.) then
            pestcp(ip)%decay_b = Exp(-.693 / pestdb(ip)%ben_hlife)
          else
            pestcp(ip)%decay_b = 0.
          endif

        end do
        exit
      enddo
      endif
      
      db_mx%pestparm = imax

      close (106)
      return
      end subroutine pest_parm_read