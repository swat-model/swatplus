       subroutine mgt_tillfactor(jj, bio_mix_event, emix,dtil)
    !!!!!!!!!!!!!!!!!!!!!!!
    ! Armen 16 January 2008
    ! This procedure increases tillage factor (tillagef(l,jj) per layer for each operation
    ! The tillage factor settling will depend of soil moisture (tentatively) and must be called every day
    ! For simplicity the settling is calculated now at the soil carbon subroutine because soil water content is available

    ! The tillage factor depends on the cumulative soil disturbance rating = csdr
    ! For simplicity, csdr is a function of emix
    ! First step is to calculate "current" csdr by inverting tillage factor function
    ! The effect of texture on tillage factor (ZZ) is removed first (and recovered at the end of the procedure)
    ! YY = tillagef(l,jj) / ZZ
    ! Since the tillage factor function is non linear, iterations are needed 
    ! XX = 0.5 is the initial value that works OK for the range of values observed
    ! If a layer is only partially tilled then emix is corrected accordingly

    use soil_module
    use basin_module
    use utils
    
    implicit none
    
    integer, intent (in) :: jj        !none           |HRU number
    logical, intent(in)  :: bio_mix_event !note       |True if this is a biological mixing event
    real, intent (inout) :: emix         !none           |mixing efficiency 
    integer :: l = 0                  !none           |counter 
    integer :: m1 = 0                 !none           |array location (see definition of ndays)
    integer :: m2 = 0                 !               |
    real :: dtil                      !mm             |depth of mixing
    real :: XX = 0.                   !varies         |variable to hold calculation results
    integer :: j = 0                  !none           |counter
    real :: zz = 0.                   !               |
    real :: yy = 0.                   !               |
    real :: xx1 = 0.                  !               | 
    real :: xx2 = 0.                  !               | 
    real :: csdr = 0.                 !               | 
    
    j = jj

    if (emix > 0.) then

      do l = 1, soil(j)%nly
            
        if (soil(jj)%phys(l)%d <= dtil) then
          emix = emix
        else if (soil(jj)%phys(l)%d > dtil .and. soil(jj)%phys(l-1)%d < dtil) then 
           emix = emix * (dtil - soil(jj)%phys(l-1)%d) / soil(jj)%phys(l)%thick
        else
            emix = 0.
        end if
            
        ! to save computation time if emix = 0 here then the other layers can be avoided
        ! tillage always proceeds from top to bottom
        if (emix == 0.) exit

        xx = 0.
        zz = 3. + (8. - 3.)*exp(-5.5*soil(jj)%phys(1)%clay/100.)
        yy = soil(jj)%ly(l)%tillagef / zz
        m1 = 1
        m2 = 2

        ! empirical solution for x when y is known and y=x/(x+exp(m1-m2*x)) 
        if (yy > 0.01) then
         xx1 = yy ** exp_w(-0.13 + 1.06 * yy)
         ! xx2 = exp_w(0.64 + 0.64 * yy ** 100.)   ! This causes an arithmatic error that is ignored by intel but not by gfortran
         xx2 = exp_w(0.64 + 0.64 * yy ** 10.)
         xx = xx1 * xx2
        end if

        csdr = xx + emix
        if (bsn_cc%cswat == 2) then                                       !! fg added this to do bio_mixing on a daily basis
          if (soil(jj)%phys(l)%tmp <= 0. .and. bio_mix_event) then 
            soil(jj)%ly(l)%tillagef = 0.
          else
            soil(jj)%ly(l)%tillagef = zz * (csdr / (csdr + exp(m1 - m2*csdr)))
          endif
        else
          soil(jj)%ly(l)%tillagef = zz * (csdr / (csdr + exp(m1 - m2*csdr)))
        endif
      end do        
    end if
        
    return
    end subroutine mgt_tillfactor