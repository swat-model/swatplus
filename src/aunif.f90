      real function aunif (x1) result (unif)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This function generates random numbers ranging from 0.0 to 1.0.
!!    In the process of calculating the random number, the seed (x1) is 
!!    set to a new value.
!!    This function implements the prime-modulus generator
!!    xi = 16807 xi Mod(2**(31) - 1)
!!    using code which ensures that no intermediate result uses more than 31 bits
!!    the theory behind the code is summarized in
!!    Bratley, P., B.L. Fox and L.E. Schrage. 1983. A Guide to Simulation.
!!        Springer-Verlag, New York. (pages 199-202)

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name       |units          |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    unif       |none           |random number between 0.0 and 1.0
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name       |units          |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    x2         |none           |variable to hold calculation results 
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!     ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      implicit none

      integer, intent (in out) :: x1   !none           |random number generator seed (integer)
                                       !               |where  0 < x1 < 2147483647
      integer :: x2

      x2 = x1 / 127773
      x1 = 16807 * (x1-x2*127773) - x2 * 2836
      if (x1 < 0) x1 = x1 + 2147483647
      unif = x1 * 4.656612875d-10

      return
      end