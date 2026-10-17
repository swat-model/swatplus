      subroutine path_cha_res_read

      use constituent_mass_module
      use input_file_module
      use maximum_data_module
      use channel_data_module
      use hydrograph_module
      use sd_channel_module
      use organic_mineral_mass_module

      implicit none

      character (len=80) :: titldum = ""
      character (len=80) :: header = ""
      character (len=500) :: line = ""
      character (len=16) :: name = ""
      integer :: eof = 0
      integer :: imax = 0
      logical :: i_exist              !none       |check to determine if file exists
      logical :: rows = .false.       !none       |.true. if each set holds one 'pathogen water benthic' row per pathogen
      logical :: at_end = .false.     !none       |.true. once a read has met the end of the file
      logical :: found = .false.      !none       |.true. if a set was read
      integer :: ipathi = 0
      integer :: ip = 0
      real :: w = 0.
      real :: b = 0.
      real, dimension(:), allocatable :: wat
      real, dimension(:), allocatable :: ben

      eof = 0

      !read all export coefficient data
      inquire (file=in_init%path_water, exist=i_exist)
      if (i_exist .or. in_init%path_water /= "null") then
        do
          open (107,file=in_init%path_water)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          allocate (wat(cs_db%num_paths), source = 0.)
          allocate (ben(cs_db%num_paths), source = 0.)

          !! the layout, from the first set: the SWAT+ Editor writes one 'pathogen water benthic' row per pathogen.
          !! The other layout is one list-directed read of 'name water(1:n) benthic(1:n)', which may span records. The
          !! sets are read as rows when one of the first set's row-shaped records names a simulated pathogen, unless the
          !! record after them starts with a number (it then continues a list-directed read).
          rows = .false.
          read (107,*,iostat=eof) titldum
          if (eof == 0) then
            do
              read (107,'(a)',iostat=eof) line
              if (eof /= 0) exit
              ip = path_row (line)
              if (ip < 0) exit
              if (ip > 0) rows = .true.
            end do
            if (eof == 0) then
              if (lead_number (line)) rows = .false.
            end if
          end if
          rewind (107)
          read (107,*,iostat=eof) titldum
          read (107,*,iostat=eof) header

          !! count the sets with the same reads that store them
          imax = 0
          at_end = .false.
          do
            call path_water_set (found)
            if (.not. found) exit
            imax = imax + 1
          end do

          db_mx%pathw_ini = imax

          allocate (path_water_ini(imax))
          allocate (path_init_name(imax))

          rewind (107)
          read (107,*,iostat=eof) titldum
          read (107,*,iostat=eof) header
          at_end = .false.
          do ipathi = 1, imax
            call path_water_set (found)
            path_init_name(ipathi) = name
            allocate (path_water_ini(ipathi)%water(cs_db%num_paths), source = wat)
            allocate (path_water_ini(ipathi)%benthic(cs_db%num_paths), source = ben)
          end do
          close (107)
          exit
        end do
      end if

      return

      contains

      !! one set: its name record, then its values into wat and ben; got is .false. when the file ends before them
      subroutine path_water_set (got)
      logical, intent (out) :: got
      integer :: ip
      got = .false.
      wat = 0.
      ben = 0.
      if (at_end) return
      read (107,*,iostat=eof) name
      if (eof /= 0) then
        at_end = .true.
        return
      end if
      if (rows) then
        !! rows up to the next record that is not one (the next set's name) or the end of the file;
        !! a row naming a pathogen that is not simulated is skipped
        do
          read (107,'(a)',iostat=eof) line
          if (eof /= 0) then
            at_end = .true.
            exit
          end if
          ip = path_row (line)
          if (ip < 0) then
            backspace (107)
            exit
          end if
          if (ip > 0) then
            wat(ip) = w
            ben(ip) = b
          end if
        end do
      else
        read (107,*,iostat=eof) titldum, wat, ben
        if (eof < 0) then
          at_end = .true.
          return
        end if
      end if
      got = .true.
      end subroutine path_water_set

      !! a 'pathogen water benthic' row: the simulated pathogen's number, with its values in w and b; 0 for a pathogen
      !! that is not simulated; -1 if str does not hold exactly three items, the last two numbers
      integer function path_row (str)
      character (len=*), intent (in) :: str
      character (len=16) :: pname
      character (len=16) :: extra
      integer :: ios
      integer :: i
      path_row = -1
      read (str,*,iostat=ios) pname, w, b
      if (ios /= 0) return
      read (str,*,iostat=ios) pname, w, b, extra
      if (ios == 0) return
      read (str,*,iostat=ios) pname, w, b
      path_row = 0
      do i = 1, cs_db%num_paths
        if (cs_db%paths(i) == pname) path_row = i
      end do
      end function path_row

      !! .true. if the first item of str is a number
      logical function lead_number (str)
      character (len=*), intent (in) :: str
      real :: x
      integer :: ios
      read (str,*,iostat=ios) x
      lead_number = ios == 0
      end function lead_number

      end subroutine path_cha_res_read
