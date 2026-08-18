      subroutine wet_read_hyd
      
      use basin_module
      use input_file_module
      use maximum_data_module
      use reservoir_data_module
      use output_landscape_module
      use gwflow_module, only : in_wet_cell,wet_thick,gw_wet_flag,out_gw !rtb
      
      implicit none
      
      character (len=80) :: titldum = ""!           |title of file
      character (len=80) :: header = "" !           |header of file
      integer :: eof = 0              !           |end of file
      integer :: imax = 0             !none       |determine max number for array (imax) and total number in file
      logical :: i_exist              !none       |check to determine if file exists
      integer :: ires = 0             !none       |counter
      integer :: dum1 = 0             !none       |
      integer :: hru_idx = 0          !none       |HRU index parsed from wetland name (e.g. wet019 -> 19)
      integer :: idig = 0             !none       |position of first digit in wetland name
      character(len=20) :: wet_name = "" !none    |wetland name read from gwflow.wetland
      real :: thick_val = 0.          !m          |wetland bed thickness read from gwflow.wetland

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
      !file format (SWAT+ editor v3.1.4+): 2 header lines, then rows of <wet_name> <thickness>
      !where wet_name is like "wet019" and the trailing digits are the HRU index
      if (bsn_cc%gwflow == 1 .and. gw_wet_flag == 1) then
        inquire(file='gwflow.wetland',exist=i_exist)
        if(i_exist) then
          write(out_gw,*) '          found gwflow.wetland; use wetland specified bed thickness'
          open(in_wet_cell,file='gwflow.wetland')
          read(in_wet_cell,*,iostat=eof) header  !title line
          read(in_wet_cell,*,iostat=eof) header  !blank or secondary header
          do
            read(in_wet_cell,*,iostat=eof) wet_name, thick_val
            if (eof /= 0) exit
            idig = scan(wet_name, '0123456789')
            if (idig > 0) then
              read(wet_name(idig:), *, iostat=eof) hru_idx
              if (eof == 0 .and. hru_idx >= 1 .and. hru_idx <= size(wet_thick)) then
                wet_thick(hru_idx) = thick_val
              endif
            endif
            eof = 0
          enddo
          close(in_wet_cell)
        endif
      endif
      
      
      return
      end subroutine wet_read_hyd