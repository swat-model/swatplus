      subroutine mgt_plantop
    
      use hru_module, only: ihru, ipl
      use soil_module
      use plant_module
      use plant_data_module
      use organic_mineral_mass_module
  
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs the plant operation

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name           |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ihru           |none          |HRU number
!!    pcom%plcur     
!!         idorm     |none          |dormancy status code; 0=land cover growing 1=land cover dormant
!!         phuacc    |fraction      |fraction of plant heat unit accumulated
!!    pcom%plm 
!!         nmass     |kg/ha         |nitrogen mass
!!         pmass     |kg/ha         |phosphorus mass
!!    pcom%plg
!!         plet      |mm H2O        |actual ET simulated during life of plant
!!         plpet     |mm H2O        |potential ET simulated during life of plant
!!         laimxfr   | 
!!         hi_adj    |(kg/ha)/(kg/ha)  |optimal harvest index for current time during growing season
!!    soil()%zmx     |mm            |maximum depth of soil 
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: curno

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      implicit none

      integer :: j            !none          |HRU number
      integer :: min          !              | 
      real :: plt_zmx         !mm            |rooting depth of plant
      
      j = ihru
      
      pcom(j)%plcur(ipl)%gro = "y"
      pcom(j)%plcur(ipl)%idorm = "n"
      pcom(j)%plcur(ipl)%phuacc = 0.
      pl_mass(j)%tot(ipl) = orgz
      pl_mass(j)%ab_gr(ipl) = orgz
      pl_mass(j)%leaf(ipl) = orgz
      pl_mass(j)%stem(ipl) = orgz
      pl_mass(j)%seed(ipl) = orgz
      pl_mass(j)%root(ipl) = orgz
      pcom(j)%plg(ipl)%plet = 0.
      pcom(j)%plg(ipl)%plpet = 0.                         
      pcom(j)%plg(ipl)%laimxfr = 0.
      pcom(j)%plg(ipl)%hi_adj = 0.
      pcom(j)%plg(ipl)%olai = 0.
      pcom(j)%plg(ipl)%dphu = 0.
      pl_mass(j)%root(ipl)%m = 0.
      pcom(j)%plstr(ipl) = plstrz 

      !! compare maximum depth in soil to maximum rooting depth of plant
      plt_zmx = 1000. * pldb(pcom(j)%plcur(ipl)%idplt)%rdmx
      soil(ihru)%zmx = Min(soil(ihru)%zmx, plt_zmx)

      return
      end subroutine mgt_plantop