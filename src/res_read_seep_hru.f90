      subroutine res_read_seep_hru

      use input_file_module, only : in_res
      use reservoir_module, only : res_ob
      use hydrograph_module, only : sp_ob

      implicit none

      character(len=256) :: title
      character(len=256) :: header
      integer :: eof
      integer :: ires
      integer :: ihru
      integer :: i
      integer, dimension(:), allocatable :: n_receiver
      integer, dimension(:), allocatable :: receiver_pos
      real :: contact_length_m
      logical :: i_exist

      inquire(file=in_res%seep_hru, exist=i_exist)

      !! Missing mapping file means that reservoir seepage routing is inactive.
      if (.not. i_exist .or. trim(in_res%seep_hru) == "null") return

      allocate(n_receiver(0:sp_ob%res))
      allocate(receiver_pos(0:sp_ob%res))
      n_receiver = 0
      receiver_pos = 0

      !! First pass: count receiving HRUs for every reservoir.
      open(105, file=in_res%seep_hru, status="old", action="read")

      read(105, *, iostat=eof) title
      if (eof /= 0) then
        close(105)
        stop "Error reading title from reservoir HRU seepage file"
      end if

      read(105, *, iostat=eof) header
      if (eof /= 0) then
        close(105)
        stop "Error reading header from reservoir HRU seepage file"
      end if

      do
        read(105, *, iostat=eof) ires, ihru, contact_length_m
        if (eof < 0) exit
        if (eof > 0) then
          close(105)
          stop "Invalid row in reservoir HRU seepage file"
        end if

        if (ires < 1 .or. ires > sp_ob%res) then
          close(105)
          stop "Invalid reservoir number in reservoir HRU seepage file"
        end if

        if (ihru < 1 .or. ihru > sp_ob%hru) then
          close(105)
          stop "Invalid HRU number in reservoir HRU seepage file"
        end if

        if (contact_length_m <= 0.) then
          close(105)
          stop "Contact length must be positive in reservoir HRU seepage file"
        end if

        n_receiver(ires) = n_receiver(ires) + 1
      end do

      close(105)

      !! Allocate the receiver arrays for each reservoir.
      do ires = 1, sp_ob%res
        res_ob(ires)%n_seep_hru = n_receiver(ires)

        if (allocated(res_ob(ires)%seep_hru)) then
          deallocate(res_ob(ires)%seep_hru)
        end if

        if (n_receiver(ires) > 0) then
          allocate(res_ob(ires)%seep_hru(n_receiver(ires)))
        end if
      end do

      !! Second pass: store HRU numbers and contact lengths.
      open(105, file=in_res%seep_hru, status="old", action="read")

      read(105, *, iostat=eof) title
      read(105, *, iostat=eof) header

      do
        read(105, *, iostat=eof) ires, ihru, contact_length_m
        if (eof < 0) exit
        if (eof > 0) then
          close(105)
          stop "Invalid row in reservoir HRU seepage file"
        end if

        receiver_pos(ires) = receiver_pos(ires) + 1
        i = receiver_pos(ires)

        res_ob(ires)%seep_hru(i)%hru_id = ihru
        res_ob(ires)%seep_hru(i)%contact_length_m = contact_length_m
      end do

      close(105)

      deallocate(n_receiver)
      deallocate(receiver_pos)

      return
      end subroutine res_read_seep_hru
