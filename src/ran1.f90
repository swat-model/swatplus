      real*8 function ran1(idum)
!
!
!  THIS SUBROUTINE IS FROM "NUMERICAL RECIPES" BY PRESS ET AL.
      
      implicit none

      integer :: m1, ia1, ic1, m2, ia2, ic2, m3, ia3, ic3, iff, idum, ix1, ix2, ix3
      integer :: j 
      real :: rm1, rm2, rqq

      dimension rqq(97)
      parameter (m1 = 259200, ia1 = 7141, ic1 = 54773, rm1 = 3.8580247e-6)
      parameter (m2 = 134456, ia2 = 8121, ic2 = 28411, rm2 = 7.4373773e-6)
      parameter (m3 = 243000, ia3 = 4561, ic3 = 51349)
      save
      data iff / 0 /
      if ((idum .lt. 0) .or. (iff .eq. 0)) then
      iff = 1
      ix1 = mod(ic1 - idum,m1)
      ix1 = mod((ia1 * ix1) + ic1,m1)
      ix2 = mod(ix1,m2)
      ix1 = mod((ia1 * ix1) + ic1,m1)
      ix3 = mod(ix1,m3)
      do 11 j = 1, 97
      ix1 = mod((ia1 * ix1) + ic1,m1)
      ix2 = mod((ia2 * ix2) + ic2,m2)
      rqq(j) = (dble(ix1) + (dble(ix2) * rm2)) * rm1
   11 continue
!!    idum = 1
      end if
      ix1 = mod((ia1 * ix1) + ic1,m1)
      ix2 = mod((ia2 * ix2) + ic2,m2)
      ix3 = mod((ia3 * ix3) + ic3,m3)
      j = 1 + ((97 * ix3) / m3)
      ran1 = rqq(j)
      rqq(j) = (dble(ix1) + (dble(ix2) * rm2)) * rm1

!  END OF SUBROUTINE RAN1
      return
      end