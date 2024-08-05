    !! Function to copy a file from source to destination
    subroutine copy_file(source, destination)
    
    implicit none
    character(len=*), intent(in) :: source, destination
    integer :: eof
    character(len=9000) :: line ! line buffer may still be too small for rout.con
    logical :: i_exist

    !!Check if the source file exists
    inquire (file=source, exist=i_exist)
    if (.not. i_exist) then
          return
    end if
    !! Open the source and destination files
    open(unit=107, file=source, status='old', action='read')
    open(unit=1007, file=destination, status='replace', action='write', recl=9000) ! line buffer may still be too small

    !! Copy the contents of the source file to the destination file
    do
        read(107, '(A)', iostat=eof) line
        if (eof /= 0) exit
        write(1007, '(A)') trim(line)
    end do

    close(107)
    close(1007)

end subroutine copy_file