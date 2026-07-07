      subroutine pl_waterup
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine distributes potential plant evaporation through
!!    the root zone and calculates actual plant water use based on soil
!!    water availability. Also estimates water stress factor.     

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    epco(:)     |none          |plant water uptake compensation factor (0-1)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ep_day      |mm H2O        |actual amount of transpiration that occurs
!!                               |on day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    gx          |
!!    ir          |
!!    j           |none          |HRU number
!!    k           |none          |counter (soil layer)
!!    reduc       |none          |fraction of water uptake by plants achieved
!!                               |where the reduction is caused by low water
!!                               |content
!!    sum         |
!!    sump        |
!!    wuse        |mm H2O        |water uptake by plants in each soil layer
!!    sum_wuse    |mm H2O        |water uptake by plants from all layers
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
    
!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Max

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use plant_data_module
      use basin_module
      use hru_module, only : hru, ihru, epmax, ipl, ep_day, uptake
      use soil_module
      use plant_module
      use urban_data_module
      
      implicit none
      
      integer :: j           !none      |hru number
      integer :: k           !none      |counter 
      integer :: ir          !none      |flag to denote bottom of root zone reached
      integer :: idp         !          | 
      integer :: ulu         !          |urban land use from urban.urb
      real :: sum            !          |
      real :: sum_wuse       !mm H2O    |water uptake by plants from all layers
      real :: sum_wusep      !mm H2O    |previous water uptake by plants from all layers
      real :: reduc          !none      |fraction of water uptake by plants achieved
                             !          |where the reduction is caused by low water
                             !          |content
      real :: sump           !          |
      real :: gx             !mm        |lowest depth in layer from which nitrogen
                             !          |may be removed
      real :: wuse           !mm H2O    |water uptake by plants in each soil layer
      real :: satco          !          | 
      real :: scparm         !          |  
      real :: uobw           !none      |water uptake normalization parameter
                             !          |This variable normalizes the water uptake so
                             !          |that the model can easily verify that uptake
                             !          |from the different soil layers sums to 1.0
      real :: ubw            !          |the uptake distribution for water is hardwired
      real :: yy             !          | 
      

      j = ihru
      idp = pcom(j)%plcur(ipl)%idplt
       
      !! compute aeration stress
      if (soil(j)%sw > soil(j)%sumfc) then
        satco = (soil(j)%sw - soil(j)%sumfc) / (soil(j)%sumul - soil(j)%sumfc)
        scparm = 100. * (satco - pldb(idp)%aeration) / (1.0001 - pldb(idp)%aeration)
        if (scparm > 0.) then
          pcom(j)%plstr(ipl)%strsa = 1. - (scparm / (scparm + Exp(2.9014 - .03867 * scparm)))
        else
          pcom(j)%plstr(ipl)%strsa = 1.
        end if
      end if

      !! compute limiting water stress
      if (epmax(ipl) <= 1.e-6) then
        pcom(j)%plstr(ipl)%strsw = 1.
      else
        !! initialize variables
        gx = 0.
        ir = 0
        sump = 0.
        wuse = 0.
        sum_wuse = 0.
        sum_wusep = 0.

        do k = 1, soil(j)%nly
          if (ir > 0) exit

          if (pcom(j)%plg(ipl)%root_dep <= soil(j)%phys(k)%d) then
            gx = pcom(j)%plg(ipl)%root_dep
            ir = k
          else
            gx = soil(j)%phys(k)%d
          end if

          if (pcom(j)%plg(ipl)%root_dep <= 0.01) then
            sum = epmax(ipl) / uptake%water_norm
          else
            sum = epmax(ipl) * (1. - Exp(-uptake%water_dis * gx / pcom(j)%plg(ipl)%root_dep)) / uptake%water_norm
          end if

          wuse = sum - sump * (1. - pcom(j)%plcur(ipl)%epco)
          ! adjust for impervious area
          ulu = hru(j)%luse%urb_lu
          !wuse = wuse * urbdb(ulu)%fcimp
          wuse = amin1 (wuse, soil(j)%phys(k)%st)
          
          sum_wuse = sum_wuse + wuse
          if (sum_wuse > epmax(ipl)) then
            wuse = epmax(ipl) - sum_wusep
            sum_wuse = epmax(ipl)
          end if
          sump = sum
          sum_wusep = sum_wuse

          if (soil(j)%phys(k)%st < wuse) then
            wuse = soil(j)%phys(k)%st
          end if

          soil(j)%phys(k)%st = Max(1.e-6, soil(j)%phys(k)%st - wuse)
          
        end do      !! soil layer loop
        
        !! update total soil water in profile
        soil(j)%sw = 0.
        do k = 1, soil(j)%nly
          soil(j)%sw = soil(j)%sw + soil(j)%phys(k)%st
        end do

        !new epco adjustment requires epmax adjustment of water stress is too high
        pcom(j)%plstr(ipl)%strsw = sum_wuse / epmax(ipl) !* hru(j)%hyd%epco)
        ep_day = ep_day + sum_wuse
      end if

      return
      end subroutine pl_waterup