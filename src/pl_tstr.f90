      subroutine pl_tstr
      
!!     ~ ~ ~ PURPOSE ~ ~ ~
!!     computes temperature stress for crop growth - strstmp

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    t_base(:)   |deg C         |minimum temperature for plant growth
!!    t_opt(:)    |deg C         |optimal temperature for plant growth
!!    tmp_an(:)   |deg C         |average annual air temperature
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    rto         |
!!    tgx         |
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use climate_module
      use plant_data_module
      use hru_module, only : ihru, ipl, iwgen
      use plant_module
      
      implicit none 
      
      integer :: j             !none        |HRU number
      integer :: idp           !            | 
      real :: tgx              !            |
      real :: rto              !none        |cloud cover factor

      j = ihru

      idp = pcom(j)%plcur(ipl)%idplt
      tgx = w%tave - pldb(idp)%t_base

      if (tgx <= 0.) then
        pcom(j)%plstr(ipl)%strst = 0.
      else
        if (w%tave > pldb(idp)%t_opt) then
         tgx = 2. * pldb(idp)%t_opt - pldb(idp)%t_base - w%tave
        end if

        rto = 0.
        rto = ((pldb(idp)%t_opt - w%tave) / (tgx + 1.e-6)) ** 2

        if (rto <= 200. .and. tgx > 0.) then
          pcom(j)%plstr(ipl)%strst = Exp(-0.1054 * rto)
        else
          pcom(j)%plstr(ipl)%strst = 0.
        end if

        if(w%tmin <= wgn_pms(iwgen)%tmp_an - 15.) pcom(j)%plstr(ipl)%strst = 0.

      end if
      
      !! APEX temperature stress equation
      rto = (w%tave - pldb(idp)%t_base) / (pldb(idp)%t_opt - pldb(idp)%t_base)
      if (rto > 0. .or. rto < 2.) then
        pcom(j)%plstr(ipl)%strst = Sin(1.5707 * rto)
      else
        pcom(j)%plstr(ipl)%strst = 0.
      end if
      
      pcom(j)%plstr(ipl)%strst = max (0., pcom(j)%plstr(ipl)%strst)
      pcom(j)%plstr(ipl)%strst = amin1 (1., pcom(j)%plstr(ipl)%strst)

      return
      end subroutine pl_tstr