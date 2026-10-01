      subroutine plant_parm_read

      !! reads the plant database (plants.plt) into pldb, one row per plant
      !!
      !! plants.plt layout:
      !!   line 1     title (skipped)
      !!   line 2     column headers (skipped)
      !!   line 3...  one plant per line; the columns are read in the same order as the
      !!              fields of plant_db (plant_data_module), plantnm through desc (55 columns)
      !!
      !! the read is list-directed (read (104,*) pldb(ic)), so it fills every field of plant_db
      !! in order. if plant_db has more fields than the line has columns, the read keeps going
      !! onto the next plant's line and every plant after that is shifted - so plant_db must
      !! match the plants.plt columns exactly
      !!
      !! carbon (codes.bsn carbon = 2) also fills res_part_fracs, the lignin/structural/metabolic
      !! fractions used to split plant residue into carbon pools (cbn_rsd_transfer)

      use input_file_module
      use maximum_data_module
      use plant_data_module
      use basin_module

      implicit none 

      external :: search
      integer :: ic = 0                   !none       |plant counter
      character (len=80) :: titldum = ""  !           |title of file
      character (len=80) :: header = ""   !           |header of file
      integer :: eof = 0              !           |end of file
      integer :: imax = 0             !none       |determine max number for array (imax) and total number in file
      integer :: mpl = 0              !           | 
      logical :: i_exist              !none       |check to determine if file exists


      eof = 0
      imax = 0
      mpl = 0

      !! no plants.plt - allocate one empty plant (index 0) so later code can still index the arrays
      inquire (file=in_parmdb%plants_plt, exist=i_exist)
      if (.not. i_exist .or. in_parmdb%plants_plt == " null") then
        allocate (pldb(0:0))
        allocate (plcp(0:0))
        allocate (pl_class(0:0))
        if (bsn_cc%cswat == 2) allocate (res_part_fracs(0:0))
      else
      do
        !! first pass - count the plant lines (imax) so the arrays can be allocated
        open (104,file=in_parmdb%plants_plt)
        read (104,*,iostat=eof) titldum
        if (eof < 0) exit
        read (104,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof == 0)
            read (104,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1
          end do
        allocate (pldb(0:imax))
        allocate (plcp(0:imax))
        allocate (pl_class(0:imax))
        !! res_part_fracs starts at the type defaults (lignin 0.12, structural 0.15, metabolic 0.85)
        if (bsn_cc%cswat == 2) allocate (res_part_fracs(0:imax))

        !! second pass - go back to the top, skip title and header, read each plant
        rewind (104)
        read (104,*,iostat=eof) titldum
        if (eof < 0) exit
        read (104,*,iostat=eof) header
        if (eof < 0) exit

        do ic = 1, imax
          !! nam1 /= 0 - the line has one more column after desc, read into pl_class
          if (bsn_cc%nam1 == 0) then
            read (104,*,iostat=eof) pldb(ic)
          else
            read (104,*,iostat=eof) pldb(ic), pl_class(ic)
          end if
          if (eof < 0) exit
          !! years to maturity is at least 1
          pldb(ic)%mat_yrs = Max (1, pldb(ic)%mat_yrs)

          !! carbon on - set the residue partition fractions for this plant
          !! lignin stays at the 0.12 default here; plants.plt (55 columns) has no lignin columns
          !! structural = lignin / 0.8 and metabolic = the rest
          if (bsn_cc%cswat == 2) then
            res_part_fracs(ic)%str_frac_abg = res_part_fracs(ic)%lig_frac_abg / .80 
            res_part_fracs(ic)%str_frac_blg = res_part_fracs(ic)%lig_frac_blg / .80 
            res_part_fracs(ic)%meta_frac_abg = 1.0 - res_part_fracs(ic)%str_frac_abg  
            res_part_fracs(ic)%meta_frac_blg = 1.0 - res_part_fracs(ic)%str_frac_blg 
          endif

        end do

        exit
      enddo
      endif

      db_mx%plantparm = imax

      close (104)
      return

      end subroutine plant_parm_read
