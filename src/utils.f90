module utils
    IMPLICIT NONE

contains

real function exp_w(y)
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
end module utils





