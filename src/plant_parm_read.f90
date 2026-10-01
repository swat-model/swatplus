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
      !! carbon (codes.bsn carbon = 2) uses a different layout with 58 columns: three lignin
      !! columns are added as columns 54-56, between bio_cov and CLASS:
      !!   ... 53 bio_cov | 54 avg_lig_frac | 55 ab_lig_frac | 56 bg_lig_frac | 57 CLASS | 58 DESCRIPTION
      !! those three are taken out of the line before it is read into pldb, so pldb is read
      !! the same way with carbon on or off. the lignin fractions go into res_part_fracs, which
      !! is used to split plant residue into carbon pools (cbn_rsd_transfer)

      use input_file_module
      use maximum_data_module
      use plant_data_module
      use basin_module
      use utils, only : split_line

      implicit none 

      external :: search
      integer :: ic = 0                   !none       |plant counter
      character (len=80) :: titldum = ""  !           |title of file
      character (len=80) :: header = ""   !           |header of file
      integer :: eof = 0              !           |end of file
      integer :: imax = 0             !none       |determine max number for array (imax) and total number in file
      integer :: mpl = 0              !           | 
      logical :: i_exist              !none       |check to determine if file exists
      character (len=2500) :: line = ""       !   |one plant line
      character (len=50) :: fields(100) = ""  !   |columns of one plant line
      integer :: nf = 0               !none       |number of columns in the plant line
      integer :: k = 0                !none       |column counter
      integer :: ios = 0              !none       |read status of the carbon layout
      type (input_lignin_partition_fracs) :: lig_in   !  |lignin fractions from columns 54-56 (carbon layout)


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
        if (bsn_cc%cswat == 2) allocate (res_part_fracs(0:imax))

        !! second pass - go back to the top, skip title and header, read each plant
        rewind (104)
        read (104,*,iostat=eof) titldum
        if (eof < 0) exit
        read (104,*,iostat=eof) header
        if (eof < 0) exit

        !! carbon off - stop if the file has the carbon lignin columns (CLASS and DESCRIPTION would get lignin values)
        !! the lignin columns are numbers; in the 55-column layout column 54 is CLASS (text), so a
        !! description with extra words can not trigger this
        !! looks at the first plant line only, then backspaces so the loop below reads it again
        if (bsn_cc%cswat /= 2) then
          read (104,'(a)',iostat=eof) line
          if (eof < 0) exit
          call split_line (line, fields, nf)
          ios = 1
          if (nf >= 58) read (fields(54:56),*,iostat=ios) lig_in
          if (ios == 0) then
            write (*,*) "ERROR: ", trim(in_parmdb%plants_plt), " has lignin columns (54-56) but codes.bsn carbon /= 2;", &
                        " use the 55-column plants.plt or set carbon = 2"
            write (9001,*) "ERROR: ", trim(in_parmdb%plants_plt), " has lignin columns (54-56) but codes.bsn carbon /= 2;", &
                        " use the 55-column plants.plt or set carbon = 2"
            error stop
          end if
          backspace (104)
        end if

        do ic = 1, imax
          if (bsn_cc%cswat == 2) then
            !! carbon layout - avg_lig_frac, ab_lig_frac, bg_lig_frac are columns 54-56, between bio_cov and CLASS
            !! take them out and read the remaining 55 columns the same way as without carbon

            !! read the whole line as text and split it into columns
            read (104,'(a)',iostat=eof) line
            if (eof < 0) exit
            call split_line (line, fields, nf)

            !! read the three lignin columns; ios stays 1 (error) if the line is too short to have them
            ios = 1
            if (nf >= 58) read (fields(54:56),*,iostat=ios) lig_in

            !! rebuild the line without columns 54-56 and read it into pldb as usual
            if (ios == 0) then
              line = ""
              do k = 1, nf
                if (k < 54 .or. k > 56) line = trim(line) // " " // trim(fields(k))
              end do
              if (bsn_cc%nam1 == 0) then
                read (line,*,iostat=ios) pldb(ic)
              else
                read (line,*,iostat=ios) pldb(ic), pl_class(ic)
              end if
            end if

            !! missing lignin columns or a bad value - stop and name the plant instead of
            !! silently running with default lignin fractions
            if (ios /= 0) then
              write (*,*) "ERROR: ", trim(in_parmdb%plants_plt), " plant ", trim(fields(1)), " could not be read;", &
                          " codes.bsn carbon = 2 needs avg_lig_frac, ab_lig_frac, bg_lig_frac as columns 54-56 (before CLASS)"
              write (9001,*) "ERROR: ", trim(in_parmdb%plants_plt), " plant ", trim(fields(1)), " could not be read;", &
                          " codes.bsn carbon = 2 needs avg_lig_frac, ab_lig_frac, bg_lig_frac as columns 54-56 (before CLASS)"
              error stop
            end if

            !! residue partition fractions for this plant
            !! lignin from plants.plt (avg_lig_frac is read but not used),
            !! structural = lignin / 0.8 and metabolic = the rest
            res_part_fracs(ic)%lig_frac_abg = lig_in%lig_frac_abg
            res_part_fracs(ic)%lig_frac_blg = lig_in%lig_frac_blg
            res_part_fracs(ic)%str_frac_abg = res_part_fracs(ic)%lig_frac_abg / .80 
            res_part_fracs(ic)%str_frac_blg = res_part_fracs(ic)%lig_frac_blg / .80 
            res_part_fracs(ic)%meta_frac_abg = 1.0 - res_part_fracs(ic)%str_frac_abg  
            res_part_fracs(ic)%meta_frac_blg = 1.0 - res_part_fracs(ic)%str_frac_blg 
          else
            !! carbon off - read the 55 columns straight into pldb
            !! nam1 /= 0 - the line has one more column after desc, read into pl_class
            if (bsn_cc%nam1 == 0) then
              read (104,*,iostat=eof) pldb(ic)
            else
              read (104,*,iostat=eof) pldb(ic), pl_class(ic)
            end if
            if (eof < 0) exit
          end if
          !! years to maturity is at least 1
          pldb(ic)%mat_yrs = Max (1, pldb(ic)%mat_yrs)

        end do

        exit
      enddo
      endif

      db_mx%plantparm = imax

      close (104)
      return

      end subroutine plant_parm_read
