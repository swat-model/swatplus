      subroutine ascrv(x1,x2,x3,x4,x5,x6)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine computes shape parameters x5 and x6 for the S curve 
!!    equation x = y/(y + exp(x5 + x6*y)) given 2 (x,y) points along the curve.
!!    x5 is determined by solving the equation with x and y values measured 
!!    around the midpoint of the curve (approx. 50% of the maximum value for x)
!!    and x6 is determined by solving the equation with x and y values measured
!!    close to one of the endpoints of the curve (100% of the maximum value for
!!    x) This subroutine is called from readbsn.f and readcrop.f


      implicit none
      
      real :: xx                  !none          |temp variable, used to hold calculated
                                  !              |value needed in later equations

      real, intent (in) :: x1     !none          |value for x in the above equation for first
                                  !              |datapoint, x1 should be close to 0.5 (the 
                                  !              |midpoint of the curve)
      real, intent (in) :: x2     !none          |value for x in the above equation for second
                                  !              |datapoint, x2 should be close to 0.0 or 1.0   
      real, intent (in) :: x3     !none          |value for y in the above equation corresponding to x1
      real, intent (in) :: x4     !none          |value for y in the above equation corresponding to x2
          
      real, intent (out) :: x5    !none          |1st shape parameter for S curve equation
                                  !              |characterizing the midpoint of the curve
      real, intent (out) :: x6     !none          |2nd shape parameter for S curve equation
                                  !              |characterizing the regions close to the
                                  !              |endpoints of the curve

      xx = Log(x3/x1 - x3)
      x6 = (xx - Log(x4/x2 - x4)) / (x4 - x3)
      x5 = xx + (x3 * x6)

      return
      end