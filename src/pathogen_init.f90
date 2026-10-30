      subroutine pathogen_init

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calls subroutines which read input data for the 
!!    databases and the HRUs

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    i             |none          |number of specific reservoir or HRU
!!    ndays(:)      |julian date   |julian date for last day of preceding 
!!                                 |month (where the array location is the 
!!                                 |number of the month) The dates are for
!!                                 |leap years
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: soil_chem, soil_phys, rteinit, h2omgt_init, hydro_init,

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use hru_module, only : hru, ihru, sol_plt_ini
      use soil_module
      use plant_module
      use pathogen_data_module
      use channel_module
      use basin_module
      use conditional_module
      use organic_mineral_mass_module
      use hydrograph_module, only : sp_ob
      use constituent_mass_module
      use output_ls_pathogen_module
      
      implicit none

      integer :: mpath = 0      !          |
      integer :: ly = 0         !none      |counter
      integer :: ipath = 0      !none      |counter
      integer :: ipath_db = 0   !          |
      integer :: isp_ini = 0
      integer :: ipl = 0        !none      |plant number
      real :: pl_frac = 0.      !0-1       |fraction of the plant pathogen placed on each plant
      real :: lai_sum_local = 0. !m2/m2    |sum of the HRU's plant LAI at the start

      do ihru = 1, sp_ob%hru  
        !! allocate pathogens
        mpath = cs_db%num_paths
        if (mpath > 0) then
          !! allocate pathogens associated with soil and plant
          do ly = 1, soil(ihru)%nly
            allocate (cs_soil(ihru)%ly(ly)%path(mpath), source = 0.)
          end do
          do ipl = 1, pcom(ihru)%npl
            allocate (cs_pl(ihru)%pl_in(ipl)%path(mpath), source = 0.)
            allocate (cs_pl(ihru)%pl_on(ipl)%path(mpath), source = 0.)
            allocate (cs_pl(ihru)%pl_up(ipl)%path(mpath), source = 0.)
          end do
          allocate (cs_irr(ihru)%path(mpath))
        end if

        isp_ini = hru(ihru)%dbs%soil_plant_init
        ipath_db = sol_plt_ini(isp_ini)%path
        if (mpath > 0) then
          !! pcom%lai_sum is first set by pl_community on day 1, so sum the plants' LAI here, as pest_apply does
          lai_sum_local = 0.
          do ipl = 1, pcom(ihru)%npl
            lai_sum_local = lai_sum_local + pcom(ihru)%plg(ipl)%lai
          end do
          do ipath = 1, mpath
            do ly = 1, soil(ihru)%nly
              if (ly == 1) then
                cs_soil(ihru)%ly(1)%path(ipath) = path_soil_ini(ipath_db)%soil(ipath)
              else
                cs_soil(ihru)%ly(1)%path(ipath) = 0.
              end if
            end do
            
            hpath_bal(ihru)%path(ipath)%plant = path_soil_ini(ipath_db)%plt(ipath)
            !! put the initial plant pathogen on the plants, split by each plant's share of the LAI
            do ipl = 1, pcom(ihru)%npl
              if (lai_sum_local > 1.e-6) then
                pl_frac = pcom(ihru)%plg(ipl)%lai / lai_sum_local
              else
                pl_frac = 0.
              end if
              cs_pl(ihru)%pl_on(ipl)%path(ipath) = pl_frac * path_soil_ini(ipath_db)%plt(ipath)
            end do
          end do
        end if
      end do

      return
      end subroutine pathogen_init