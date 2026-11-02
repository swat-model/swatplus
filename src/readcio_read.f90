       subroutine readcio_read 
    
       use input_file_module
       use output_path_module

       implicit none
           
       character (len=80) :: titldum = ""
       character (len=15) :: name = ""
       character (len=256) :: out_path_value = ""
       character (len=512) :: line_buffer
       integer :: eof = 0
       integer :: idx = 0
       logical :: i_exist              !none       |check to determine if file exists
       integer :: i = 0
       
       eof = 0
       
       !! read file.cio
       inquire (file="file.cio", exist=i_exist)
       if (i_exist ) then
         open (107,file="file.cio")
         read (107,*) titldum
      do i = 1, 31
         read (107,*,iostat=eof) name, in_sim  
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_basin
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_cli
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_con
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_cha
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_res
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_ru
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_hru
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_exco
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_rec
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_delr
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_aqu
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_herd
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_wallo
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_link
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_hyd
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_str
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_parmdb
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_ops
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_lum
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_chg
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_init
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_sol
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_cond
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_regs
         if (eof < 0) exit
!!!!!weather path code
         ! Read each path line whole: a list-directed read stops an unquoted path at its first '/' or blank
         call read_cio_path (107, "pcp_path", in_path_pcp%pcp, eof)
         if (eof < 0) exit
         call read_cio_path (107, "tmp_path", in_path_tmp%tmp, eof)
         if (eof < 0) exit
         call read_cio_path (107, "slr_path", in_path_slr%slr, eof)
         if (eof < 0) exit
         call read_cio_path (107, "hmd_path", in_path_hmd%hmd, eof)
         if (eof < 0) exit
         call read_cio_path (107, "wnd_path", in_path_wnd%wnd, eof)
         if (eof < 0) exit
!!!!!weather path code
!!!!!output path code
         ! Read the whole line to handle spaces and avoid '/' termination in list-directed input
         read (107,'(A)',iostat=eof) line_buffer
         if (eof < 0) then
           out_path_value = ""
         else
           ! Parse the line: skip the label (first word), then get the rest
           line_buffer = adjustl(line_buffer)
           idx = index(line_buffer, ' ')
           if (idx > 0) then
             ! Found space after label, get the rest
             out_path_value = adjustl(line_buffer(idx+1:))
           else
             ! No value found
             out_path_value = ""
           end if
         end if
!!!!!output path code
         exit
      enddo
      endif

       close (107)
       
       !! Initialize output path (will use current dir if null/empty)
       call init_output_path(out_path_value)
            
       return

      contains

      !! Reads one weather path line of file.cio whole. The value after the label may be quoted
      !! ('...' or "...", which keeps blanks); an unquoted value ends at the first blank. A missing
      !! trailing separator is added, as init_output_path does for the output path, so the path
      !! can be joined to a file name. "null" and an empty value are kept as they are.
      subroutine read_cio_path (iunit, label, path, ios)

      integer, intent (in) :: iunit
      character (len=*), intent (in) :: label
      character (len=*), intent (inout) :: path
      integer, intent (out) :: ios
      character (len=1024) :: line, value
      character (len=32) :: os_env
      character (len=1) :: sep
      character (len=3) :: blank
      integer :: i1, i2, n, stat

      read (iunit,'(a)',iostat=ios) line
      if (ios /= 0) return
      if (len_trim(line) == len(line)) then
        write (*,'(3a,i0,a)') "! ERROR: file.cio line for ", label, " is longer than ", len(line), " characters"
        stop 1
      end if
      ! blanks are spaces, tabs and the carriage return of a file saved on Windows
      blank = " "//char(9)//char(13)
      i1 = verify(line, blank)
      if (i1 == 0) then
        path = ""
        return
      end if
      line = line(i1:)
      i1 = scan(line, blank)
      i2 = verify(line(i1:), blank)
      if (i2 == 0) then
        path = ""
        return
      end if
      line = line(i1+i2-1:)
      if (line(1:1) == '"' .or. line(1:1) == "'") then
        i2 = index(line(2:), line(1:1))
        if (i2 == 0) then
          write (*,'(4a)') "! ERROR: file.cio ", label, " has no closing quote: ", trim(line)
          stop 1
        end if
        value = line(2:i2)
      else
        i2 = scan(line, blank)
        value = line(1:i2-1)
      end if
      n = len_trim(value)
      if (n > 0 .and. trim(value) /= "null") then
        if (value(n:n) /= "/" .and. value(n:n) /= "\") then
          sep = "/"
          call get_environment_variable("OS", os_env, status=stat)
          if (stat == 0 .and. index(os_env, "Windows") > 0) sep = "\"
          n = n + 1
          value(n:n) = sep
        end if
      end if
      if (n > len(path)) then
        write (*,'(3a,i0,a)') "! ERROR: file.cio ", label, " is longer than ", len(path), " characters"
        stop 1
      end if
      path = value(1:n)

      end subroutine read_cio_path

      end subroutine readcio_read  