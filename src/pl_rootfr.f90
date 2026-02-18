    subroutine pl_rootfr(j)
    !! This subroutine distributes dead root mass through the soil profile
    !! code developed by Armen R. Kemanian in 2008 
    !! March, 2009 further adjustments expected
    
    use hru_module, only : ipl
    use soil_module
    use plant_module
    
    implicit none    

    real :: cum_rd = 0.               !            |
    real :: cum_d = 0.                !            | 
    real :: cum_rf = 0.               !            |
    real :: x1 = 0.                   !            |
    real :: x2 = 0.                   !            |
    integer, intent (in) :: j         !none               |HRU number
    integer :: k = 0                  !            |
    integer :: ly = 0                 !none        |number of soil layer that manure applied
    real :: a = 0.                    !            |
    real :: b = 0.                    !            |
    real :: c = 0.                    !            | 
    real :: d = 0.                    !            |
    real :: rtfr = 0.                 !none        |root fraction
    real :: xx1 = 0.                  !            |
    real :: xx2 = 0.                  !            |
    real :: xx = 0.                   !            |
    
    if (pcom(j)%plg(ipl)%root_dep < 1.e-6) then
      pcom(j)%plg(ipl)%rtfr(ly) = 1.
      return
    endif

    ! Normalized Root Density = 1.15*exp[-11.7*NRD] + 0.022, where NRD = normalized rooting depth
    ! Parameters of Normalized Root Density Function from Dwyer et al 19xx
    a = 1.15
    b = 11.7
    c = 0.022
    d = 0.12029 ! Integral of Normalized Root Distribution Function 
                ! from 0 to 1 (normalized depth) = 0.12029

    k = 0
    !cum_d = 0.
    cum_rf = 0.
    rtfr = 0.

    do ly = 1, soil(j)%nly
      if (soil(j)%phys(ly)%d >= pcom(j)%plg(ipl)%root_dep) then
        cum_rd = pcom(j)%plg(ipl)%root_dep
      else
        cum_rd = soil(j)%phys(ly)%d
      end if
      !cum_d = cum_d + soil(j)%phys(ly)%thick
      !if (cum_d >= pcom(j)%plg(ipl)%root_dep) cum_rd = pcom(j)%plg(ipl)%root_dep
      !if (cum_d < pcom(j)%plg(ipl)%root_dep) cum_rd = cum_d
      
      x1 = (cum_rd - soil(j)%phys(ly)%thick) / pcom(j)%plg(ipl)%root_dep
      x2 = cum_rd / pcom(j)%plg(ipl)%root_dep
      xx1 = -b * x1
      if (xx1 > 20.) xx1 = 20.
      xx2 = -b * x2
      if (xx2 > 20.) xx2 = 20.
      pcom(j)%plg(ipl)%rtfr(ly) = (a/b*(Exp(xx1) - Exp(xx2)) + c *(x2 - x1))/d
      xx = cum_rf
      cum_rf = cum_rf + pcom(j)%plg(ipl)%rtfr(ly)
      if (cum_rf > 1.) then
        pcom(j)%plg(ipl)%rtfr(ly) = 1. - xx
        cum_rf = 1.0
      end if
      k = ly
      if (cum_rd >= pcom(j)%plg(ipl)%root_dep) Exit
         
    end do

    !!   ensures that cumulative fractional root distribution = 1
    do ly = 1, soil(j)%nly
      pcom(j)%plg(ipl)%rtfr(ly) = pcom(j)%plg(ipl)%rtfr(ly) / cum_rf
      if (ly == k) exit ! exits loop on the same layer as the previous loop
    end do
    
    return
   
    end subroutine pl_rootfr