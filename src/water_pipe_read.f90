      subroutine water_pipe_read
      
      use input_file_module
      use water_allocation_module
      use mgt_operations_module
      use maximum_data_module
      use hydrograph_module
      use constituent_mass_module
      
      implicit none 
      
      character (len=80) :: titldum = ""!           |title of file
      character (len=80) :: header = "" !           |header of file
      integer :: eof = 0              !           |end of file
      integer :: imax = 0             !none       |determine max number for array (imax) and total number in file
      logical :: i_exist              !none       |check to determine if file exists
      integer :: i = 0                !none       |counter
      integer :: ipipe = 0
      integer :: num_aqu = 0
      integer :: iaq = 0
      
      eof = 0
      imax = 0
      
      !! read water allocation inputs

      inquire (file='water_pipe.wal', exist=i_exist)
      if (.not. i_exist .or. 'water_pipe.wal' == "null") then
        allocate (pipe(0:0))
      else
      do 
        open (107,file='water_pipe.wal')
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) imax
        read (107,*,iostat=eof) header
        !db_mx%water_treat = imax
        if (eof < 0) exit
        
        allocate (pipe(imax))

        do ipipe = 1, imax
          read (107,*,iostat=eof) header
          if (eof < 0) exit 
          read (107,*,iostat=eof) i, pipe(ipipe)%name, pipe(ipipe)%stor_mx,                &
                                     pipe(ipipe)%lag_days, pipe(ipipe)%loss_fr, num_aqu
          if (eof < 0) exit
          
          !! allocate and read aquifer loss data
          allocate (pipe(ipipe)%aqu_loss(num_aqu))
          
          read (107,*,iostat=eof) i, pipe(ipipe)%name, pipe(ipipe)%stor_mx, pipe(ipipe)%lag_days,   &
                pipe(ipipe)%loss_fr, pipe(ipipe)%num_aqu, (pipe(ipipe)%aqu_loss(iaq), iaq = 1, num_aqu)
        end do
        
      end do
      end if
      
      close(107)

      return
    end subroutine water_pipe_read