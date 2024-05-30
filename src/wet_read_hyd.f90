      subroutine wet_read_hyd
      
      use basin_module
      use input_file_module
      use maximum_data_module
      use reservoir_data_module
      use output_landscape_module
      use gwflow_module, only : in_wet_cell,wet_thick,gw_wet_flag,out_gw !rtb
      
      implicit none
      
      character (len=80) :: titldum   !           |title of file
      character (len=80) :: header    !           |header of file
      integer :: eof                  !           |end of file
      integer :: imax                 !none       |determine max number for array (imax) and total number in file
      logical :: i_exist              !none       |check to determine if file exists
      integer :: ires                 !none       |counter 
      integer :: dum1                 !none       |

      eof = 0
      imax = 0

      inquire (file=in_res%hyd_wet, exist=i_exist)
      if (.not. i_exist .or. in_res%hyd_wet == "null") then
        allocate (wet_hyddb(0:0))
      else   
      do
       open (105,file=in_res%hyd_wet)
       read (105,*,iostat=eof) titldum
       if (eof < 0) exit
       read (105,*,iostat=eof) header
       if (eof < 0) exit
        do while (eof == 0)
          read (105,*,iostat=eof) titldum
          if (eof < 0) exit
          imax = imax + 1
        end do
        
      db_mx%wet_hyd = imax
      
      allocate (wet_hyddb(0:imax))
      rewind (105)
      read (105,*,iostat=eof) titldum
      if (eof < 0) exit
      read (105,*,iostat=eof) header
      if (eof < 0) exit
      
       do ires = 1, imax
         read (105,*,iostat=eof) titldum
         if (eof < 0) exit
         backspace (105)
         read (105,*,iostat=eof) wet_hyddb(ires)
         if (eof < 0) exit

        if (wet_hyddb(ires)%psa <= 0.0) wet_hyddb(ires)%psa = 0.08 * wet_hyd(ires)%pdep
        if (wet_hyddb(ires)%esa <= 0.0) wet_hyddb(ires)%esa = 1.5 * wet_hyd(ires)%psa
        if (wet_hyddb(ires)%evrsv <= 0.) wet_hyddb(ires)%evrsv = 0.6
        
        

       end do
       close (105)
      exit
      enddo
      endif
  
      !rtb: if gwflow, then read wetland bottom sediment thickness from gwflow.wetland
      if (bsn_cc%gwflow == 1 .and. gw_wet_flag == 1) then
        inquire(file='gwflow.wetland',exist=i_exist)
        if(i_exist) then
          write(out_gw,*) '          found gwflow.wetland; use wetland specified bed thickness'
		      open(in_wet_cell,file='gwflow.wetland')
          read(in_wet_cell,*) header
          read(in_wet_cell,*) header
          read(in_wet_cell,*) header
          read(in_wet_cell,*) header
          !read in wetland bed thickness (m)
          read(in_wet_cell,*) header
          do ires=1,imax
            read(in_wet_cell,*) dum1,wet_thick(ires)
          enddo
        endif
      endif
      
      
      return
      end subroutine wet_read_hyd