      module output_filter_module
      use hydrograph_module, only : sp_ob, sp_ob1, ob
      implicit none
      private
      public :: output_filter_init, output_filter_read
      public :: output_filter_use_ch_sd_list, output_filter_write_ch_sd
      public :: output_filter_ch_sd_nc_slot, output_filter_ch_sd_nwrite
      public :: output_filter_open_ch_sd_output

      logical, save :: ofilt_use_list = .false.
      logical, allocatable, save :: ofilt_ch_sd(:)
      integer, allocatable, save :: ofilt_ch_sd_slot(:)

      contains

      subroutine output_filter_init(nchandeg)
        integer, intent(in) :: nchandeg
        integer :: ich
        if (allocated(ofilt_ch_sd)) deallocate(ofilt_ch_sd, ofilt_ch_sd_slot)
        if (nchandeg <= 0) return
        allocate(ofilt_ch_sd(nchandeg), ofilt_ch_sd_slot(nchandeg))
        ofilt_ch_sd = .false.
        ofilt_ch_sd_slot = 0
        ofilt_use_list = .false.
      end subroutine output_filter_init

      logical function output_filter_use_ch_sd_list()
        output_filter_use_ch_sd_list = ofilt_use_list
      end function output_filter_use_ch_sd_list

      integer function output_filter_ch_sd_nwrite()
        integer :: ich
        output_filter_ch_sd_nwrite = 0
        if (.not. allocated(ofilt_ch_sd)) return
        do ich = 1, size(ofilt_ch_sd)
          if (ofilt_ch_sd(ich)) output_filter_ch_sd_nwrite = output_filter_ch_sd_nwrite + 1
        end do
      end function output_filter_ch_sd_nwrite

      logical function output_filter_open_ch_sd_output()
        if (.not. ofilt_use_list) then
          output_filter_open_ch_sd_output = .true.
        else
          output_filter_open_ch_sd_output = (output_filter_ch_sd_nwrite() > 0)
        end if
      end function output_filter_open_ch_sd_output

      logical function output_filter_write_ch_sd(ichan)
        integer, intent(in) :: ichan
        if (ichan < 1 .or. ichan > size(ofilt_ch_sd)) then
          output_filter_write_ch_sd = .false.
          return
        end if
        if (.not. ofilt_use_list) then
          output_filter_write_ch_sd = .true.
        else
          output_filter_write_ch_sd = ofilt_ch_sd(ichan)
        end if
      end function output_filter_write_ch_sd

      integer function output_filter_ch_sd_nc_slot(ichan)
        integer, intent(in) :: ichan
        if (ichan < 1) then
          output_filter_ch_sd_nc_slot = 0
          return
        end if
        if (.not. ofilt_use_list) then
          if (ichan > sp_ob%chandeg) then
            output_filter_ch_sd_nc_slot = 0
          else
            output_filter_ch_sd_nc_slot = ichan
          end if
          return
        end if
        if (ichan > size(ofilt_ch_sd_slot)) then
          output_filter_ch_sd_nc_slot = 0
        else
          output_filter_ch_sd_nc_slot = ofilt_ch_sd_slot(ichan)
        end if
      end function output_filter_ch_sd_nc_slot

      subroutine output_filter_rebuild_ch_sd_slots()
        integer :: ich, slot
        if (.not. allocated(ofilt_ch_sd_slot)) return
        slot = 0
        do ich = 1, size(ofilt_ch_sd)
          if (ofilt_ch_sd(ich)) then
            slot = slot + 1
            ofilt_ch_sd_slot(ich) = slot
          else
            ofilt_ch_sd_slot(ich) = 0
          end if
        end do
      end subroutine output_filter_rebuild_ch_sd_slots

      subroutine output_filter_read()
        character(len=256) :: line, keyword
        integer :: ios, ich, nchandeg
        logical :: i_exist, in_section
        inquire(file="print_filter.prt", exist=i_exist)
        nchandeg = sp_ob%chandeg
        call output_filter_init(nchandeg)
        if (.not. i_exist) return
        if (nchandeg <= 0) return
        open (1087, file="print_filter.prt", status="old", action="read", iostat=ios)
        if (ios /= 0) return
        in_section = .false.
        do
          read (1087, '(a)', iostat=ios) line
          if (ios /= 0) exit
          if (len_trim(line) == 0) cycle
          if (line(1:1) == "#") cycle
          read (line, *, iostat=ios) keyword
          if (ios /= 0) cycle
          if (index(trim(keyword), "channel_sd") > 0) then
            in_section = .true.
            ofilt_use_list = .true.
            cycle
          end if
          if (in_section) then
            read (line, *, iostat=ios) ich
            if (ios /= 0) then
              in_section = .false.
              cycle
            end if
            if (ich >= 1 .and. ich <= nchandeg) ofilt_ch_sd(ich) = .true.
          end if
        end do
        close (1087)
        if (ofilt_use_list) call output_filter_rebuild_ch_sd_slots()
      end subroutine output_filter_read

      end module output_filter_module
