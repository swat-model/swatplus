module utils
    IMPLICIT NONE

contains

real function exp_w(y)
#ifdef __INTEL_COMPILER
      use ifcore, only: tracebackqq
#endif
    implicit none
    real, intent(in) :: y
    if (y < -80.) then
        print *, "exp(", y, ") causes an underflow. Setting exp_w result to zero"
        exp_w = 0.
        print *
        print *, "Stack Trace:" 
#ifdef __INTEL_COMPILER
        print *, "Using Intel Fortran compiler stack trace"
        call tracebackqq(USER_EXIT_CODE=-1)
#elif defined(__GFORTRAN__)
        print *, "Using GNU Fortran compiler stack trace"
        call backtrace()
#else
        print *, "No stack trace available: Unknown compiler"
#endif
    else  
        exp_w = exp(y)
    endif
end function exp_w
end module utils





