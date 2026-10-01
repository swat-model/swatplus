      subroutine plant_parm_read

      !! reads the plant database (plants.plt) into pldb, one row per plant
      !!
      !! plants.plt layout:
      !!   line 1     title (skipped)
      !!   line 2     column headers (skipped)
      !!   line 3...  one plant per line; the columns are read in the same order as the
      !!              fields of plant_db (plant_data_module), plantnm through desc (55 columns)
      !!
      !! each plant line is read as text first, checked, and then read into pldb from that
      !! text (read (line,*) pldb(ic)). reading from the line instead of the file means a line
      !! with too few columns is a read error - read straight from the file, a short line would
      !! keep going onto the next plant's line and shift every plant after it
      !!
      !! a line that does not match the layout stops the run with an error naming the plant:
      !!   - fewer than 55 columns (e.g. the older 54-column plants.plt without CLASS)
      !!   - a number in column 54 with carbon off (lignin columns, carbon layout)
      !!   - no lignin columns with carbon = 2
      !! the column order itself is not checked - an older 55-column file with different
      !! columns in the same positions (frac_sw_gro, rsd_pctcov, rsd_covfac) is not caught
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
      integer :: ios = 0              !none       |read status of the plant line
      real :: col54 = 0.              !none       |column 54 read as a number, to tell CLASS (text) from lignin
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

        do ic = 1, imax
          !! read the whole line as text, skipping blank lines, and split it into columns
          !! a carriage return left by Windows line endings is removed so it does not end up in desc
          do
            read (104,'(a)',iostat=eof) line
            if (eof /= 0) exit
            k = len_trim(line)
            if (k > 0) then
              if (line(k:k) == achar(13)) line(k:k) = " "
            end if
            if (len_trim(line) > 0) exit
          end do
          if (eof < 0) exit
          call split_line (line, fields, nf)

          if (bsn_cc%cswat == 2) then
            !! carbon layout - avg_lig_frac, ab_lig_frac, bg_lig_frac are columns 54-56, between bio_cov and CLASS
            !! take them out and read the remaining 55 columns the same way as without carbon

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
            !! carbon off - column 54 is CLASS (text). a number there means the line has lignin
            !! columns (carbon layout), and CLASS and DESCRIPTION would get lignin values
            if (nf >= 54) then
              read (fields(54),*,iostat=ios) col54
              if (ios == 0) then
                write (*,*) "ERROR: ", trim(in_parmdb%plants_plt), " plant ", trim(fields(1)), &
                            " has lignin columns (54-56) but codes.bsn carbon /= 2;", &
                            " use the 55-column plants.plt or set carbon = 2"
                write (9001,*) "ERROR: ", trim(in_parmdb%plants_plt), " plant ", trim(fields(1)), &
                            " has lignin columns (54-56) but codes.bsn carbon /= 2;", &
                            " use the 55-column plants.plt or set carbon = 2"
                error stop
              end if
            end if

            !! read the 55 columns into pldb from the line; ios stays 1 (error) if the line is too short
            !! nam1 /= 0 - the line has one more column after desc, read into pl_class
            ios = 1
            if (nf >= 55) then
              if (bsn_cc%nam1 == 0) then
                read (line,*,iostat=ios) pldb(ic)
              else
                read (line,*,iostat=ios) pldb(ic), pl_class(ic)
              end if
            end if
            if (ios /= 0) then
              write (*,*) "ERROR: ", trim(in_parmdb%plants_plt), " plant ", trim(fields(1)), " could not be read;", &
                          " codes.bsn carbon /= 2 needs 55 columns, ending rt_depco, aeration, rsd_cov, bio_cov,", &
                          " CLASS, DESCRIPTION"
              write (9001,*) "ERROR: ", trim(in_parmdb%plants_plt), " plant ", trim(fields(1)), " could not be read;", &
                          " codes.bsn carbon /= 2 needs 55 columns, ending rt_depco, aeration, rsd_cov, bio_cov,", &
                          " CLASS, DESCRIPTION"
              error stop
            end if
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
