module utils
    IMPLICIT NONE

    integer, parameter :: MAX_TABLE_COLS = 100
    integer, parameter :: MAX_NAME_LEN = 50
    integer, parameter :: MAX_LINE_LEN = 2500

    public :: table_reader

    type :: table_reader
        character(MAX_NAME_LEN)  :: header_cols(MAX_TABLE_COLS) = ''  !array of header column names
        character(MAX_NAME_LEN)  :: row_field(MAX_TABLE_COLS) = '' !array of data fields in a data row of data
        character(len=MAX_LINE_LEN)   :: line = ''        ! character string used to read in lines from data table
        character(len=:), allocatable :: left_str    ! portion of line left of comment delimiter '#'
        character(len=:), allocatable :: file_name   ! name of the file being read
        character(len=:), allocatable :: min_cols! string of minimum required column names
        character (len=80)     :: titldum = ""       ! first line in data file that that will be ignored 
        integer                :: nrow = 0           ! data row number
        integer                :: ncols = 0          ! number of header columns   
        integer                :: nfields = 0        ! number of data columns/fields in a data row
        integer                :: skipped_rows = 0   ! number of rows skipped (empty or comment lines)
        integer                :: start_row_numbr = 1! the number of the row in the file to start reading table data
                                                     ! This number cannot greater than the line number of the header row.
        integer                :: unit = 0           ! file unit number
        logical                :: found_header_row = .false. ! flag to indicate if header row has been found
        logical, allocatable   :: col_okay(:)        ! array used to track if warning message has already
                                                     ! been printed out for unknown column headers
        logical                :: file_exists  = .false. ! flag to indicate if file exists

    contains
        procedure              :: init
        procedure              :: get_num_data_lines
        procedure              :: get_header_columns
        procedure              :: get_row_fields
        procedure              :: output_column_warning
        procedure              :: get_row_idx
        procedure              :: get_col_count
        procedure              :: min_header_cols
        procedure              :: min_req_cols

    end type table_reader

contains

subroutine init(self, unit, file_name, start_row_numbr)
    class(table_reader), intent(inout) :: self
    integer, intent(in), optional                       :: unit          
    ! character(len=:), allocatable, intent(in), optional :: file_name
    character(len=*), optional :: file_name
    integer, intent(in), optional                       :: start_row_numbr    

    self%left_str  = ''
    self%file_name = ''
    self%min_cols = ''
    if (present(unit)) self%unit = unit
    if (present(file_name)) self%file_name = file_name
    if (present(start_row_numbr)) then
        self%start_row_numbr = start_row_numbr
        if (start_row_numbr < 1) then
            self%start_row_numbr = 1
        end if
    end if
    !! read all curve number data from cn.tbl
    inquire (file=self%file_name, exist=self%file_exists)
    if (.not. self%file_exists .or. trim(self%file_name) == "null") then
        write(9001,'(3A)') 'Warning: Input file named ', self%file_name, ' is missing or null.'
        print('(3A)'), 'Warning: Input file named ', self%file_name, ' is missing or null.'
    end if

end subroutine init


real function exp_w(y)
!===============================================================================
! Author: fgeter
!
! Function: exp_w
!
! Purpose:
!   Computes the exponential of 'y' (exp(y)) with safe handling of severe
!   underflow conditions. If 'y' is less than -80.0, the result is set to 0.0
!   to avoid floating-point underflow, and an optional diagnostic warning is
!   emitted to the error unit (stderr).
!
!   This "wrapped" exponential prevents program crashes or silent NaN/Inf in
!   environments where very small exponents underflow to zero anyway, while
!   providing traceability via compiler-specific stack traces when enabled.
!
! Arguments:
!   y          : real, intent(in)
!                The exponent value (any real number)
!
! Returns:
!   exp_w      : real
!                exp(y) if y >= -80.0; otherwise 0.0
!
! Features:
!   - Underflow threshold: -80.0 (chosen as exp(-80) ≈ 1.8e-35, safely below
!     single-precision denormalized range and near zero for most practical purposes)
!   - Optional error reporting controlled by 'err_output' flag:
!       * .true.  → prints warning message + compiler-specific stack trace
!       * .false. → silent (default; no output or performance overhead)
!   - Compiler-specific backtrace support:
!       * Intel Fortran (__INTEL_COMPILER): tracebackqq
!       * GNU Fortran (__GFORTRAN__): backtrace()
!       * Other compilers: fallback message (no trace)
!   - Uses iso_fortran_env for portable 'error_unit'
!   - Implicit none and intent(in) for safety
!
! Example usage:
!   real :: safe_exp
!   safe_exp = exp_w(-90.0)   ! Returns 0.0 (with optional warning + trace)
!   safe_exp = exp_w(10.0)    ! Returns exp(10.0) ≈ 22026.465
!
! Notes:
!   - The err_output flag is hardcoded to .false. for production use.
!     Uncomment the .true. line for debugging underflow issues.
!   - Stack traces are expensive; keep err_output=.false. in release builds.
!   - Threshold (-80.0) is conservative for real(kind=real32); adjust if using
!     higher precision (e.g., real64) and stricter needs.
!   - No allocation or side effects beyond optional I/O and trace.
!
!===============================================================================

      use iso_fortran_env
#ifdef __INTEL_COMPILER
      use ifcore, only: tracebackqq
#endif
    implicit none
    real, intent(in) :: y
    logical :: err_output
    
    ! err_output = .true.
    err_output = .false.
    ! err_output = .false.

    if (y < -80.) then
        exp_w = 0.
        if (err_output) then
            write(error_unit,'(A)') ""
            write(error_unit,'(A,F6.1,A)') "Warning: exp(", y, ") causes an underflow."
            write(error_unit,'(A)') "Setting exp_w result to zero"
#ifdef __INTEL_COMPILER
            write(error_unit,'(A)') "Intel Fortran compiler stack trace"
            call tracebackqq(USER_EXIT_CODE=-1)
#elif defined(__GFORTRAN__)
            write(error_unit,'(A)') "GNU Fortran compiler stack trace"
            call backtrace()
#else
            write(error_unit, *)  "No stack trace available: Unknown compiler"
#endif
        endif
    else  
        exp_w = exp(y)
    endif
end function exp_w


pure function to_lower(str) result(lower)
!===============================================================================
! Author: fgeter
!
! Function: to_lower
!
! Purpose:
!   Converts the input string to lowercase using a pure, elemental-style approach.
!   Only ASCII uppercase letters (A-Z) are converted; all other characters
!   (lowercase letters, digits, symbols, spaces, etc.) remain unchanged.
!
!   This is a lightweight, portable, dependency-free alternative to using
!   intrinsic routines that may not be available in all compilers/environments.
!
! Arguments:
!   str    : character(len=*), intent(in)
!            The input string to convert (can be any length)
!
! Returns:
!   lower  : character(len=len(str))
!            The input string with all ASCII uppercase letters converted to
!            their lowercase equivalents. The length is identical to the input.
!
! Features:
!   - Pure function: no side effects, suitable for use in expressions and
!     initialization expressions
!   - Uses iachar() and achar() for reliable, portable ASCII code manipulation
!   - Handles only basic Latin alphabet (A-Z → a-z); does not perform locale-
!     sensitive conversions (e.g., no handling of accented characters)
!
! Example usage:
!   print *, to_lower("Hello WORLD! 123")   ! Outputs: "hello world! 123"
!   character(len=20) :: name = "Fortran ROCKS"
!   print *, to_lower(name)                 ! Outputs: "fortran rocks"
!
! Notes:
!   - Performance is good for typical string lengths (character-by-character loop)
!   - Safe for strings containing non-letter characters or already-lowercase text
!   - No allocation is performed (fixed-length result matches input length)
!
!===============================================================================
    character(len=*), intent(in) :: str
    character(len=len(str))      :: lower
    integer                      :: i, code

    do i = 1, len(str)
        code = iachar(str(i:i))
        if (code >= iachar('A') .and. code <= iachar('Z')) then
            lower(i:i) = achar(code + 32)
        else
            lower(i:i) = str(i:i)
        end if
    end do
end function to_lower


subroutine left_of_delim(input, delim, result)
!===============================================================================
! Author: fgeter
!
! Subroutine: left_of_delim
!
! Purpose:
!   Extracts the substring from the beginning of 'input' up to (but not including)
!   the first occurrence of the specified delimiter character 'delim'.
!   If the delimiter is not found, the entire input string is returned.
!
!   This routine is typically used to strip trailing comments or extract the
!   meaningful part of a line (e.g., everything before a '#' comment marker).
!
! Arguments:
!   input    : character(len=*), intent(in)
!              The input string to process (may contain leading/trailing spaces)
!
!   delim    : character(len=1), intent(in)
!              Single character delimiter to search for (e.g., '#', '!', ':', etc.)
!
!   result   : character(len=:), allocatable, intent(out)
!              Output: the substring from start of 'input' to just before the first
!              'delim', or the entire trimmed input if 'delim' is not found.
!              Allocated automatically to the exact required length.
!
! Notes:
!   - The returned string does NOT include the delimiter itself.
!   - Leading and trailing whitespace are preserved in the result only if they
!     appear before the delimiter (or in the whole string if no delimiter).
!   - Uses Fortran intrinsic 'index()' for efficient delimiter search.
!   - Safe for empty input strings (returns empty allocated string).
!
! Example usage:
!   character(len=:), allocatable :: comment_free
!   call left_of_delim("data  1 2 3  # this is a comment", "#", comment_free)
!   ! comment_free now contains "data  1 2 3  " (note preserved trailing spaces)
!
!===============================================================================

    character(len=*), intent(in)               :: input
    character(len=1), intent(in)               :: delim
    character(len=:), allocatable, intent(out) :: result

    integer :: pos

    pos = index(input, delim)

    if (pos == 0) then
      ! Delimiter not found → return whole string, trimmed
      result = input
    else
      ! Return everything before the delimiter (exclude the delimiter itself)
      result = input(1:pos-1)
    end if

end subroutine left_of_delim


subroutine split_line(line2, fields2, nfields, delim, maxsplit)
    !===============================================================================
    ! SUBROUTINE: split_line
    ! PURPOSE:    Splits a string into fields using fixed-size arrays.
    ! AUTHOR:     Developed by user fgeter through many failed iterations 
    !             by Grok/xAI. Final corrections and working code was done by fgeter
    ! DATE:       December 19, 2025
    !
    ! DESCRIPTION:
    !   This subroutine splits an input line into individual fields and stores them
    !   in a fixed-size output array. It is designed to be robust and debugger-friendly
    !   with gfortran/VS Code by avoiding allocatable or deferred-length arrays.
    !
    !   Behaviour:
    !     * If an optional delimiter (delim) is provided:
    !         - Splits on that single character.
    !         - Preserves empty fields (leading, trailing, and consecutive delimiters
    !           all produce empty strings).
    !     * If no delimiter is provided:
    !         - Splits on whitespace (spaces and tabs).
    !         - Collapses consecutive whitespace.
    !         - Ignores leading and trailing whitespace (no empty fields created).
    !     * If an optional maxsplit is provided:
    !         - Performs at most maxsplit splits.
    !         - The remainder of the line becomes the last field.
    !
    ! PARAMETERS:
    !   line2     (in)  : character(len=*)          - Input string to split
    !   fields2   (out) : character(len=*) :: fields2(:) - Fixed-size array to receive fields
    !   nfields   (out) : integer                   - Number of fields found (size of result)
    !   delim     (in, optional) : character(len=1) - Single character delimiter
    !   maxsplit  (in, optional) : integer          - Maximum number of splits to perform
    !
    ! USAGE EXAMPLES:
    !
    !   character(len=1000) :: line
    !   character(len=50)   :: fields(100)
    !   integer             :: nf
    !
    !   ! 1. Default whitespace splitting (collapse whitespace)
    !   line = "  hello   world  example  "
    !   call split_line(line, fields, nf)
    !   ! nf = 3, fields = 'hello', 'world', 'example'
    !
    !   ! 2. Split on comma, preserve empty fields
    !   line = ",,  apple  ,,banana,"
    !   call split_line(line, fields, nf, delim=",")
    !   ! nf = 6, fields = '', '', 'apple', '', 'banana', ''
    !
    !   ! 3. Split on semicolon with maxsplit=1 (remainder as last field)
    !   line = "one;two;three;four"
    !   call split_line(line, fields, nf, delim=";", maxsplit=1)
    !   ! nf = 2, fields = 'one', 'two;three;four'
    !
    !   ! 4. Empty line
    !   line = ""
    !   call split_line(line, fields, nf)
    !   ! nf = 0
    !
    !   ! 5. Parsing line comma delimiter
    !   line = '43   # a,  b,c,  d,e,f'
    !   call split_line(line, fields, nf)
    !   ! nf = 6, fields = '43   # a', 'b', 'c', 'd', 'e', 'f'
    !
    ! NOTES:
    !   - Fields are left-justified with trailing blanks removed via adjustl(trim(...)).
    !   - If the number of fields exceeds the size of fields2, an error message is printed
    !     and the subroutine returns early.
    !   - Safe for gfortran debugging (no allocatables or deferred-length components).
    !===============================================================================

    character(len=*), intent(in)                 :: line2
    ! The following line uses deferred-length strings for fields2 array, however, gfortran debugger has issues with it.
    ! The gfortran debugging issue can be worked around by using fixed-length strings instead 
    ! (comment out the next line and uncomment the following line to get gfortran debugger to work).
    character(len=*), intent(out)                :: fields2(:)
    ! character(len=30), intent(out)                :: fields2(:)  
    integer,          intent(out)                :: nfields
    character(len=1), intent(in), optional       :: delim
    integer,          intent(in), optional       :: maxsplit

    integer :: pos1, pos2, len_line, splits_done
    character(len=1) :: current_delim
    logical :: use_custom_delim

    nfields = 0
    fields2 = ''  ! Clear all fields

    use_custom_delim = present(delim)
    if (use_custom_delim) then
        current_delim = delim
    end if

    splits_done = 0
    len_line = len(trim(line2))
    if (len_line == 0) return

    pos1 = 1

    if (use_custom_delim) then

        ! Leading empty fields
        do while (pos1 <= len_line .and. line2(pos1:pos1) == current_delim)
            nfields = nfields + 1
            if (nfields > size(fields2)) then
                print *, 'Error: too many fields'
                return
            end if
            fields2(nfields) = ''
            pos1 = pos1 + 1
            splits_done = splits_done + 1
            if (present(maxsplit)) then
                if (splits_done >= maxsplit) then
                if (pos1 <= len_line) then
                    nfields = nfields + 1
                    if (nfields > size(fields2)) return
                    fields2(nfields) = adjustl(trim(line2(pos1:)))
                end if
                end if
                return
            end if
        end do

        do while (pos1 <= len_line)
            pos2 = pos1

            do while (pos2 <= len_line .and. line2(pos2:pos2) /= current_delim)
                pos2 = pos2 + 1
            end do

            nfields = nfields + 1
            if (nfields > size(fields2)) then
                print *, 'Error: too many fields'
                return
            end if

            fields2(nfields) = adjustl(trim(line2(pos1:min(pos2-1, pos1 + len(fields2) - 1))))

            pos1 = pos2

            if (pos1 <= len_line .and. line2(pos1:pos1) == current_delim) then
                splits_done = splits_done + 1
                if (pos1 == len_line) then
                    nfields = nfields + 1
                    if (nfields > size(fields2)) then
                        print *, 'Error: too many fields'
                        return
                    end if
                    fields2(nfields) = ''
                    return
                end if
                pos1 = pos1 + 1
                if (present(maxsplit)) then
                    if (splits_done >= maxsplit) then
                        if (pos1 <= len_line) then
                            nfields = nfields + 1
                            if (nfields > size(fields2)) return
                            fields2(nfields) = trim(adjustl(line2(pos1:)))
                        end if
                    return
                    end if
                end if
            end if
        end do
    else
        ! Whitespace mode
        do while (pos1 <= len_line .and. (line2(pos1:pos1) == ' ' .or. line2(pos1:pos1) == char(9)))
            pos1 = pos1 + 1
        end do

        do while (pos1 <= len_line)
            pos2 = pos1

            do while (pos2 <= len_line .and. .not. (line2(pos2:pos2) == ' ' .or. line2(pos2:pos2) == char(9)))
                pos2 = pos2 + 1
            end do

            nfields = nfields + 1
            if (nfields > size(fields2)) then
                print *, 'Error: too many fields'
                return
            end if

            fields2(nfields) = line2(pos1:min(pos2-1, pos1 + len(fields2) - 1))

            pos1 = pos2

            do while (pos1 <= len_line .and. (line2(pos1:pos1) == ' ' .or. line2(pos1:pos1) == char(9)))
                pos1 = pos1 + 1
            end do

            splits_done = splits_done + 1
            if (present(maxsplit) ) then
                if (splits_done >= maxsplit) then
                if (pos1 <= len_line) then
                    nfields = nfields + 1
                    if (nfields > size(fields2)) return
                    fields2(nfields) = trim(adjustl(line2(pos1:)))
                end if
                endif
                return
            end if
        end do
    end if
    return
end subroutine split_line

function get_row_idx(self) result(row)
! returns the current row index
  class(table_reader), intent(inout) :: self
  integer :: row
  row = self%nrow
  return
end function get_row_idx

function get_col_count(self) result(col)
! returns the number of columns
  class(table_reader), intent(inout) :: self
  integer :: col
  col = self%ncols
  return
end function get_col_count



function get_num_data_lines(self) result(imax)
!===============================================================================
!> @brief Count the number of valid data rows in the table file
!
! @author  fgeter
!
! @par Purpose
! This function scans the entire file associated with `self%unit` to determine 
! how many valid data rows exist. It performs the following steps:
! - Rewinds to the beginning of the file
! - Skips the first line (title/description line)
! - Identifies the first non-empty, non-comment line as the header row
! - Counts only those subsequent rows that:
!   - are not empty
!   - are not comment-only
!   - contain exactly the same number of fields as found in the header row
!
! @note
! **Important side effects:**
! - The file position is left at **END-OF-FILE** after execution
! - `self%found_header_row` is set to `.true.` if a header was found
! - `self%ncols` is set to the number of columns detected in the header
! - `self%line`, `self%left_str`, `self%row_field` are modified (working buffers)
! - This function leaves the file at EOF. Rewind if needed before further reads.
!   → Call this function **before** reading the actual data, or rewind afterward.
!
! @warning
! This function leaves the file at EOF. Use rewind if needed before further reads.
!
! @param[inout] self   The `table_reader` object containing file unit and buffers
! @return       imax   Number of valid data rows (rows with correct column count)
!===============================================================================
    class(table_reader), intent(inout) :: self
    integer :: imax
    integer :: eof = 0              !           |end of file
    integer :: i

    imax = 0
    self%found_header_row = .false.
    open (self%unit,file=self%file_name)

    if (self%start_row_numbr == 1) then
        read (self%unit,*,iostat=eof) self%titldum
    else
        ! Skip to the specified starting row number
        do i = 1, self%start_row_numbr - 1
            read(self%unit, '(A)', iostat=eof) self%line
            if (eof /= 0) exit
        end do
    end if
    if (eof == 0) then 
        do
            read(self%unit, '(A)', iostat=eof) self%line
            if (eof /= 0) exit  ! EOF
            self%line = adjustl(trim(self%line))
            call left_of_delim(self%line, '#', self%left_str)    ! remove comments
            self%left_str = trim(adjustl(self%left_str))
            if ( len(self%left_str) == 0) cycle                  ! skip empty lines
            self%line = self%left_str
            if (.not. self%found_header_row) then                ! check to see if the header row has not yet been processed
                self%found_header_row = .true.
                call split_line(self%line, self%row_field, self%ncols) ! process header row into header columns
                cycle
            end if
            call split_line(self%line, self%row_field, self%nfields)    ! split data row into fields

            ! Ignore datarow if the number of data fields does not match the number of header columns
            if (self%ncols /= self%nfields) then
                cycle
            end if
            imax = imax + 1
        end do
    endif
    return
end function get_num_data_lines

subroutine min_req_cols(self, min_cols)
! Sets the minimum required columns data element
    class(table_reader), intent(inout) :: self
    character(len=*), intent(in) :: min_cols
    self%min_cols = trim(adjustl(min_cols))
end subroutine min_req_cols


subroutine min_header_cols(self, min_cols)
!===============================================================================
! Author fgeter
! Purpose: Checks to see if user specified required columns are in the header 
!          columns that are read in and if not print error and stop.
!===============================================================================

    class(table_reader), intent(inout) :: self
    character(len=*), intent(in) :: min_cols
    character(MAX_NAME_LEN)  :: min_hdr_cols(MAX_TABLE_COLS) = ''  !array of header column names
    integer :: i, ncols 
    character(len=:), allocatable :: min_col

    call split_line(min_cols, min_hdr_cols, ncols) ! process into header columns

    ! Check if all minimum required columns are present in the header
    do i = 1, ncols
        min_col = trim(adjustl(min_hdr_cols(i)))
        if (index(self%line, min_col) == 0) then
            write(9001, '(4A)') "Error: Required column ", min_col, " not found in ", self%file_name
            print *, "Error: Required column ", min_col, " not found in ", self%file_name
            stop
        end if
    end do
end subroutine min_header_cols

subroutine get_header_columns(self, eof)
!===============================================================================
! Author: fgeter
!
! Subroutine: get_header_columns
!
! Purpose:
!   Locates, reads, and processes the header row from the table file associated
!   with tblr%unit. It:
!     - Rewinds the file to the beginning
!     - Skips the first line (treated as a title/dummy line)
!     - Skips any blank lines or comment-only lines
!     - Identifies the first meaningful (non-empty, non-comment) line as the header
!     - Splits that line into individual column names
!     - Converts each column name to lowercase and trims whitespace
!     - Stores the cleaned column names in tblr%header_cols(:)
!     - Sets tblr%ncols to the number of columns found
!     - Updates tblr%skipped_rows to reflect how many lines were skipped before the header
!     - Sets tblr%found_header_row = .true. upon successful header detection
!
!   The subroutine leaves the file positioned immediately after the header row.
!
! Arguments:
!   eof    : integer, intent(out)
!            Final IOSTAT value from the last read operation:
!              0    = success (header found)
!              < 0  = end-of-file reached before finding a header
!              > 0  = read error occurred
!
! Side effects:
!   - Rewinds and reads from tblr%unit
!   - Modifies: tblr%found_header_row, tblr%ncols, tblr%header_cols,
!               tblr%skipped_rows, tblr%line, tblr%left_str
!   - Calls external routines: left_of_delim(), split_line(), to_lower()
!
! Notes:
!   - Assumes tblr%unit is already open for reading
!   - Column names are normalized to lowercase and trimmed for consistency
!   - If no valid header is found before EOF, tblr%ncols remains 0 and
!     tblr%found_header_row stays .false.
!   - Uses '(A)' format for robust line reading, then processes manually
!
! Example behavior:
!   File content:
!     Some title line
!     # comment
!       
!     ID,Name,Value   ← header
!     1,Alice,10.5
!
!   → After call: tblr%ncols = 3, tblr%header_cols = ['id','name','value'],
!     tblr%skipped_rows incremented by 4 (title + comment + blank + header)
!
!============================================o===================================

    class(table_reader), intent(inout) :: self
    integer                     :: i
    integer                     :: eof

    eof = 0

    rewind (self%unit)  ! reset file position to beginning

    ! Start reading from the specified starting row number.
    ! If start_row_numbr=1, read and skip the title line.
    if (self%start_row_numbr == 1) then
        read (self%unit,*,iostat=eof) self%titldum
        self%skipped_rows = self%skipped_rows + 1
    else
        ! Skip to the specified starting row number
        do i = 1, self%start_row_numbr - 1
            read(self%unit, '(A)', iostat=eof) self%line
            self%skipped_rows = self%skipped_rows + 1
            if (eof /= 0) exit
        end do
    end if

    self%found_header_row = .false.

    if (eof == 0) then 
        do
            read(self%unit, '(A)', iostat=eof) self%line
            if (eof /= 0) exit  ! EOF
            self%line = trim(adjustl(self%line))
            call left_of_delim(self%line, '#', self%left_str)    ! remove comments
            self%left_str = trim(adjustl(self%left_str))
            if ( len(self%left_str) == 0) then              ! skip empty lines 
                self%skipped_rows = self%skipped_rows + 1
                cycle                  
            end if
            self%line = self%left_str
            if (.not. self%found_header_row) then                 ! check to see if the header row has not yet been processed
                self%found_header_row = .true.
                call split_line(self%line, self%header_cols, self%ncols) ! process header row into header columns
                do i = 1 , self%ncols
                    self%header_cols(i) = to_lower(trim(adjustl(self%header_cols(i))))
                end do
                self%skipped_rows = self%skipped_rows + 1
                exit
            end if
        end do
        allocate (self%col_okay(self%ncols))
        self%col_okay = .true.
        if (len(self%min_cols) > 0) call min_header_cols(self, self%min_cols)
    end if
    return
end subroutine get_header_columns


subroutine get_row_fields(self, eof)
!===============================================================================
! Subroutine: get_row_fields
!
! Author:     fgeter
!
! Purpose:    Reads lines from the input file (unit=tblr%unit) until a valid 
!             data row is found. Processes the line by:
!               - removing comments (everything after #)
!               - skipping blank lines
!               - splitting into fields
!               - checking correct number of columns
!
!             The routine stops at the first valid data row or when EOF is reached.
!             Keeps track of skipped rows (blank lines or wrong column count).
!
! Arguments:
!   eof   : intent(out) integer
!           IOSTAT value from the last READ operation.
!           0   = success (valid data row found)
!           >0  = end of file or error condition
!
! Side effects / modifies:
!   tblr%line         - last read line (raw)
!   tblr%left_str     - line content left of comment delimiter
!   tblr%row_field    - array of trimmed fields from the current valid row
!   tblr%nfields      - number of fields found in current row
!   tblr%skipped_rows    - counter of skipped rows (blank + wrong column count)
!   tblr%nrow         - assumed to be updated outside (current data row count)
!
! Dependencies:
!   - Module variable/type: tblr (table reader context)
!   - External routines: left_of_delim, split_line
!===============================================================================
    class(table_reader), intent(inout) :: self
    integer, intent(out)          :: eof
    integer                       :: i

    self%row_field = ''
    self%nfields = 0
    do
        read(self%unit, '(A)', iostat=eof) self%line
        if (eof /= 0) exit
        self%line = adjustl(trim(self%line))

        ! get portion of line left of comment delimiter '#'
        call left_of_delim(self%line, '#', self%left_str)
        self%left_str = trim(adjustl(self%left_str))
        self%line = self%left_str

        ! skip empty lines
        if (len(self%left_str) == 0 ) then
            self%skipped_rows = self%skipped_rows + 1
            cycle ! get next line
        endif
        
        ! split data row into fields
        call split_line(self%line, self%row_field, self%nfields)
        do i=1, self%nfields
            self%row_field(i) = trim(adjustl(self%row_field(i)))
        end do
        
        ! check for correct number of columns and if incorrect skip row with warning
        if (self%ncols /= self%nfields) then
            self%skipped_rows = self%skipped_rows + 1
            write(9001,'(A,I3, 3A)') 'Warning: Row ', self%nrow + self%skipped_rows, ' in the input file ', &
                                      self%file_name, ' has the wrong number of columns: skipping'
            print('(A,I3, 3A)'), 'Warning: Row ', self%nrow + self%skipped_rows, ' in the input file ', & 
                                      self%file_name, ' has the wrong number of columns: skipping'
            cycle
        end if
        self%nrow = self%nrow + 1
        exit
    enddo
    return
end subroutine get_row_fields


subroutine output_column_warning(self, i)
!===============================================================================
! Subroutine: output_column_warning
!
! Author:     fgeter
!
! Purpose:    Issues a one-time warning (to diagnostic file unit 9001 and stdout)
!             when an unrecognized column header is encountered in the input file.
!             The warning is printed only the first time a particular unknown 
!             column is detected (subsequent occurrences are silently ignored).
!
!             This routine helps inform the user about columns that are present 
!             in the input file but are not recognized/expected by the program.
!
! Arguments:
!   i     : intent(in) integer
!           Index of the column in tblr%header_cols whose name is unknown
!
! Side effects / modifies:
!   tblr%col_okay(i)  - set to .false. after first warning (prevents repeated messages)
!
! Dependencies / assumptions:
!   - Module variable/type: tblr (table reader context)
!   - tblr%header_cols(:)   contains the column header names read from file
!   - tblr%col_okay(:)      logical array tracking which columns are recognized
!   - tblr%sub_name         name of the current subroutine/context (for message clarity)
!   - Function to_lower()   converts string to lowercase (assumed available)
!   - Output units:
!       9001 = log/warning file
!       *    = standard output (print statement)
!
! Note: The warning is printed only once per unknown column name.
!===============================================================================
    class(table_reader), intent(inout) :: self
    integer, intent(in) :: i
    if (self%col_okay(i) .eqv. .true.) then
        self%col_okay(i) = .false.
        write(9001,'(5A)') 'Warning: unknown column header named ', &
                           to_lower(trim(self%header_cols(i))), ' in the input file ', self%file_name, ' : skipping:'
        print('(5A)'), 'Warning: unknown column header named ', &
                           to_lower(trim(self%header_cols(i))), ' in the input file ', self%file_name, ' : skipping:'
    endif
    return
end subroutine output_column_warning
end module utils