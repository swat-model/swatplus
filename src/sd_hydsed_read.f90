      subroutine sd_hydsed_read
      
      use input_file_module
      use sd_channel_module
      use channel_velocity_module
      use maximum_data_module
      use hydrograph_module
      use time_module
      
      implicit none      
      
      integer :: iisd
      real :: kh
      
      character (len=80) :: titldum   !             |title of file
      character (len=80) :: header    !             |header of file
      character (len=16) :: namedum   !             |
      integer :: eof                  !             |end of file
      integer :: imax                 !none         |determine max number for array (imax) and total number in file
      logical :: i_exist              !none         |check to determine if file exists
      integer :: idb                  !             |
      integer :: i                    !none         |counter  
      real :: aa                      !none         |area/area=1 (used to calculate velocity with
                                      !             |Manning"s equation)
      real :: a                       !m^2          |cross-sectional area of channel
      real :: b                       !m            |bottom width of channel
      real :: d                       !m            |depth of flow
      real :: p                       !m            |wetting perimeter
      real :: chside                  !none         |change in horizontal distance per unit
                                      !             |change in vertical distance on channel side
                                      !             |slopes; always set to 2 (slope=1/2)
      real :: fps                     !none         |change in horizontal distance per unit
                                      !             |change in vertical distance on floodplain side
                                      !             |slopes; always set to 4 (slope=1/4)
      integer :: max                  !             |
      real :: rh                      !m            |hydraulic radius
      real :: qman                    !m^3/s or m/s |flow rate or flow velocity
      real :: tt1                     !km s/m       |time coefficient for specified depth
      real :: tt2                     !km s/m       |time coefficient for bankfull depth
      real :: qq1                     !m^3/s        |flow rate for a specified depth
         
      eof = 0
      imax = 0
      maxint = 10
      
      allocate (timeint(10))    !***jga
      if (bsn_cc%i_fpwet == 0) then
        allocate (hyd_rad(10))
        allocate (trav_time(10))
        allocate (flo_dep(10))
      else
        allocate (hyd_rad(time%step))
        allocate (trav_time(time%step))
        allocate (flo_dep(time%step))
      end if
      
      inquire (file=in_cha%hyd_sed, exist=i_exist)
      if (.not. i_exist .or. in_cha%hyd_sed == "null") then
        allocate (sd_chd(0:0))
      else
      do
        open (1,file=in_cha%hyd_sed)
        read (1,*,iostat=eof) titldum
        if (eof < 0) exit
        read (1,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof == 0)
            read (1,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1
          end do  
          
        db_mx%ch_lte = imax
           
        allocate (sd_chd(0:imax))
        
        !rtb floodplain
        !allocate(flood_freq(imax))
        !flood_freq = 0

        rewind (1)
        read (1,*,iostat=eof) titldum
        if (eof < 0) exit
        read (1,*,iostat=eof) header
        if (eof < 0) exit
        
        do idb = 1, db_mx%ch_lte
          read (1,*,iostat=eof) sd_chd(idb)
          if (eof < 0) exit
        end do

        exit
      end do
      end if

      close (1)
      return
      end subroutine sd_hydsed_read