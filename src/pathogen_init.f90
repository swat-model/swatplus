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

      use hru_module, only : hru, ihru, sol_plt_ini_cs
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

      integer :: mpath          !          |
      integer :: ly             !none      |counter
      integer :: ipath          !none      |counter
      integer :: ipath_db       !          |
      integer :: isp_ini
      integer :: ipl            !none      |plant number

      do ihru = 1, sp_ob%hru  
        !! allocate pathogens
        mpath = cs_db%num_paths
        if (mpath > 0) then
          !! allocate pathogens associated with soil and plant
          do ly = 1, soil(ihru)%nly
            allocate (cs_soil(ihru)%ly(ly)%path(mpath))
          end do
          do ipl = 1, pcom(ihru)%npl
            allocate (cs_pl(ihru)%pl_in(ipl)%path(mpath))
            allocate (cs_pl(ihru)%pl_on(ipl)%path(mpath))
            allocate (cs_pl(ihru)%pl_up(ipl)%path(mpath))
          end do
          allocate (cs_irr(ihru)%path(mpath))
        end if

        isp_ini = hru(ihru)%dbs%soil_plant_init
        ipath_db = sol_plt_ini_cs(isp_ini)%path
        if (mpath > 0) then
          do ipath = 1, mpath
            do ly = 1, soil(ihru)%nly
              if (ly == 1) then
                cs_soil(ihru)%ly(1)%path(ipath) = path_soil_ini(ipath_db)%soil(ipath)
              else
                cs_soil(ihru)%ly(1)%path(ipath) = 0.
              end if
            end do
            
            hpath_bal(ihru)%path(ipath)%plant = path_soil_ini(ipath_db)%plt(ipath)
          end do
        end if
      end do

      return
      end subroutine pathogen_init