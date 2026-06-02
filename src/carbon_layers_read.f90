      subroutine carbon_layers_read

      !! reads carbon_layers.prt and sets cb_n_layers (number of soil layers
      !! to include in per-layer carbon outputs).
      !!
      !! carbon_layers.prt format:
      !!   line 1 - title (free text)
      !!   line 2 - header (free text, ignored)
      !!   line 3 - single integer: number of layers
      !!
      !! the file is OPTIONAL. discovered via inquire; if missing, the routine
      !! returns silently and cb_n_layers is left for output_landscape_init to
      !! default to the largest soil layer count across all HRUs. no entry in
      !! file.cio.

      use carbon_module, only : cb_n_layers, cb_n_layers_explicit

      implicit none

      character (len=80) :: titldum = ""
      character (len=80) :: header  = ""
      integer            :: eof     = 0
      integer            :: n_lyr   = 0
      logical            :: i_exist = .false.

      inquire (file='carbon_layers.prt', exist=i_exist)
      if (.not. i_exist) return

      open (107, file='carbon_layers.prt', iostat=eof)
      if (eof /= 0) return

      read (107, *, iostat=eof) titldum
      if (eof /= 0) goto 99
      read (107, *, iostat=eof) header
      if (eof /= 0) goto 99

      read (107, *, iostat=eof) n_lyr
      if (eof /= 0) goto 99

      if (n_lyr < 1) then
        write (9001,*) "carbon_layers.prt: n_layers must be >= 1; got ", n_lyr, "; default ", cb_n_layers, " kept"
        goto 99
      end if

      cb_n_layers = n_lyr
      cb_n_layers_explicit = .true.

   99 close (107)
      return
      end subroutine carbon_layers_read
