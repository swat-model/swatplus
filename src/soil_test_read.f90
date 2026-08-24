      subroutine soil_test_read

      !! reads soil_test.sol -- measured soil test results that override the
      !! soils.sol database values for the depths they cover.
      !!
      !! soil_test.sol format:
      !!   line 1 - title (free text)
      !!   line 2 - column header (free text, ignored)
      !!   then one row per tested layer:
      !!     soil_name  depth_mm  bd  cbn  sand  silt  clay
      !!
      !!   soil_name  must match a name in soils.sol
      !!   depth_mm   depth (mm) of the BOTTOM of the tested layer; the layer runs
      !!              from the previous row's depth for that soil (0 for the first)
      !!   bd         Mg/m^3  bulk density
      !!   cbn        %       organic carbon
      !!   sand/silt/clay  %  texture fractions, must total 100.0 when all supplied
      !!
      !!   -1 in any of bd/cbn/sand/silt/clay means "not measured" -- soils_test_adjust
      !!   leaves the soils.sol value in place for that property.
      !!
      !!   blank lines and lines whose first non-blank character is '#' are ignored,
      !!   so a deck can carry comments alongside the data rows.
      !!
      !! rows for one soil must be CONTIGUOUS and in ASCENDING depth order --
      !! soils_test_adjust walks the array that way to find each layer's top depth.
      !!
      !! the file is read only when codes.bsn carbon = 1, and only when it exists.
      !! it is discovered via inquire and has no entry in file.cio, the same as
      !! carbon_lyr.bsn and carbon_layers.prt. when it is absent sol_test is left
      !! unallocated and soils_init skips soils_test_adjust entirely.

      use basin_module
      use soil_module
      use soil_data_module
      use maximum_data_module

      implicit none

      character (len=*), parameter :: test_file = "soil_test.sol"
      character (len=*), parameter :: blanks    = " "//char(9)   !! space and tab

      character (len=80)  :: titldum = ""
      character (len=500) :: header  = ""
      character (len=500) :: line    = ""
      integer             :: eof     = 0
      logical             :: i_exist = .false.
      integer             :: imax    = 0     !! number of data rows found
      integer             :: itest   = 0     !! counter over data rows
      integer             :: iprev   = 0     !! counter over earlier data rows
      integer             :: isol    = 0     !! counter over the soil database
      integer             :: ipos    = 0     !! first non-blank (or non-tab) character in a line
      real                :: txt_sum = 0.    !! sand + silt + clay of one row
      logical             :: found   = .false.

      nmbr_soil_test_layers = 0

      !! discrete per-mode branching, not a negated test: project convention is that a
      !! future cswat mode must fail loudly rather than be silently absorbed by a negated
      !! test. carbon_bsn_read (proc_bsn) has already rejected any mode other than 0 or 1.
      select case (bsn_cc%cswat)
      case (0)
        return                     !! static soil carbon -- soil_test.sol is not used
      case (1)
        continue                   !! CENTURY -- read soil_test.sol below
      case default
        return                     !! unreachable; carbon_bsn_read error stops first
      end select

      !! the file is optional -- an absent soil_test.sol simply means no soil tests
      inquire (file=test_file, exist=i_exist)
      if (.not. i_exist) return

      open (107, file=test_file, iostat=eof)
      if (eof /= 0) then
        write (*,*)    "ERROR: ", test_file, " exists but could not be opened"
        write (9001,*) "ERROR: ", test_file, " exists but could not be opened"
        error stop
      end if

      !! title and column-header lines are read with '(a)' so a blank line consumes
      !! exactly one record instead of being skipped by a list-directed read
      read (107, '(a)', iostat=eof) titldum
      read (107, '(a)', iostat=eof) header

      !! pass 1 -- count the data rows so sol_test can be allocated
      imax = 0
      do
        read (107, '(a)', iostat=eof) line
        if (eof /= 0) exit
        if (len_trim(line) == 0) cycle
        ipos = verify(line, blanks)
        if (ipos == 0) cycle                    !! tab-only line
        if (line(ipos:ipos) == "#") cycle
        imax = imax + 1
      end do

      if (imax == 0) then
        write (9001,*) "WARNING: ", test_file, " holds no soil test rows; soils.sol values are unchanged"
        close (107)
        return
      end if

      allocate (sol_test(imax))
      nmbr_soil_test_layers = imax

      !! pass 2 -- read the data rows
      rewind (107)
      read (107, '(a)', iostat=eof) titldum
      read (107, '(a)', iostat=eof) header

      itest = 0
      do
        read (107, '(a)', iostat=eof) line
        if (eof /= 0) exit
        if (len_trim(line) == 0) cycle
        ipos = verify(line, blanks)
        if (ipos == 0) cycle                    !! tab-only line
        if (line(ipos:ipos) == "#") cycle

        itest = itest + 1
        read (line, *, iostat=eof) sol_test(itest)%snam,    &
                                   sol_test(itest)%d,       &
                                   sol_test(itest)%bd,      &
                                   sol_test(itest)%cbn,     &
                                   sol_test(itest)%sand,    &
                                   sol_test(itest)%silt,    &
                                   sol_test(itest)%clay
        if (eof /= 0) then
          write (*,*)    "ERROR: ", test_file, " row ", itest,     &
                         " could not be parsed (expected soil_name depth_mm bd cbn sand silt clay)"
          write (9001,*) "ERROR: ", test_file, " row ", itest,     &
                         " could not be parsed (expected soil_name depth_mm bd cbn sand silt clay)"
          close (107)
          error stop
        end if
      end do

      close (107)

      !! validate the rows -- soils_test_adjust assumes all of this holds
      do itest = 1, nmbr_soil_test_layers

        if (sol_test(itest)%d <= 0.) then
          write (*,*)    "ERROR: ", test_file, " row ", itest, " soil ", trim(sol_test(itest)%snam),   &
                         ": depth_mm must be greater than 0; got ", sol_test(itest)%d
          write (9001,*) "ERROR: ", test_file, " row ", itest, " soil ", trim(sol_test(itest)%snam),   &
                         ": depth_mm must be greater than 0; got ", sol_test(itest)%d
          error stop
        end if

        if (itest > 1) then
          if (sol_test(itest)%snam == sol_test(itest-1)%snam) then
            !! same soil as the row above -- depths must ascend
            if (sol_test(itest)%d <= sol_test(itest-1)%d) then
              write (*,*)    "ERROR: ", test_file, " soil ", trim(sol_test(itest)%snam),               &
                             ": depths must ascend; row ", itest, " depth ", sol_test(itest)%d,        &
                             " does not exceed the previous depth ", sol_test(itest-1)%d
              write (9001,*) "ERROR: ", test_file, " soil ", trim(sol_test(itest)%snam),               &
                             ": depths must ascend; row ", itest, " depth ", sol_test(itest)%d,        &
                             " does not exceed the previous depth ", sol_test(itest-1)%d
              error stop
            end if
          else
            !! new soil -- its rows must not have appeared earlier in the file
            do iprev = 1, itest - 2
              if (sol_test(iprev)%snam == sol_test(itest)%snam) then
                write (*,*)    "ERROR: ", test_file, " soil ", trim(sol_test(itest)%snam),             &
                               ": rows for one soil must be contiguous; row ", itest,                  &
                               " restarts a soil last seen at row ", iprev
                write (9001,*) "ERROR: ", test_file, " soil ", trim(sol_test(itest)%snam),             &
                               ": rows for one soil must be contiguous; row ", itest,                  &
                               " restarts a soil last seen at row ", iprev
                error stop
              end if
            end do
          end if
        end if

        !! texture must close on 100% when all three fractions were measured
        if (sol_test(itest)%sand /= -1. .and. sol_test(itest)%silt /= -1.       &
            .and. sol_test(itest)%clay /= -1.) then
          txt_sum = sol_test(itest)%sand + sol_test(itest)%silt + sol_test(itest)%clay
          if (abs(txt_sum - 100.) > 0.01) then
            write (*,*)    "ERROR: ", test_file, " row ", itest, " soil ", trim(sol_test(itest)%snam),  &
                           ": sand + silt + clay must total 100.0; got ", txt_sum
            write (9001,*) "ERROR: ", test_file, " row ", itest, " soil ", trim(sol_test(itest)%snam),  &
                           ": sand + silt + clay must total 100.0; got ", txt_sum
            error stop
          end if
        end if

        !! a name that matches no soil in soils.sol is a typo the run would otherwise
        !! swallow silently -- soils_test_adjust simply never matches it. warn, do not stop.
        found = .false.
        do isol = 1, db_mx%soil
          if (soildb(isol)%s%snam == sol_test(itest)%snam) then
            found = .true.
            exit
          end if
        end do
        if (.not. found) then
          write (9001,*) "WARNING: ", test_file, " row ", itest, " names soil ",       &
                         trim(sol_test(itest)%snam), " which is not in soils.sol; the row is ignored"
        end if

      end do

      return
      end subroutine soil_test_read
