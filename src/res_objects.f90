      subroutine res_objects

      use reservoir_module
      use hydrograph_module
      
      implicit none 
      
      integer :: iob1            !none         |beginning of loop
      integer :: iob2            !none         |ending of loop
      integer :: ires            !none         |counter
      integer :: i               !none         |counter

      !! set reservoir object numbers for reservoir objects
      iob1 = sp_ob1%res
      iob2 = sp_ob1%res + sp_ob%res - 1
      ires = 0
      do i = iob1, iob2
        ires = ires + 1
        res_ob(ires)%ob = i
        res_ob(ires)%props = ob(i)%props
      end do

      return
      end subroutine res_objects