      subroutine path_ls_runoff
    
      use pathogen_data_module
      use constituent_mass_module
      use output_ls_pathogen_module
      use hru_module, only : hru, sol_plt_ini, ihru, sedyld, qday, enratio
      use soil_module
      
      implicit none

      integer :: j          !none          |hru number
      integer :: ipath      !none          |pathogen counter
      integer :: ipath_db   !none          |pathogen number from data file
      integer :: isp_ini    !none          |soil-plant initialization number from data file
      real :: cpath         !              |concentration of pathogen in soil
      real :: path_kd

      j = ihru
         
      do ipath = 1, cs_db%num_paths
        isp_ini = hru(ihru)%dbs%soil_plant_init
        ipath_db = sol_plt_ini(isp_ini)%path
        
        path_kd = path_db(ipath_db)%kd
        !! compute soluble bacteria in the surface runoff
        hpath_bal(j)%path(ipath)%surq = path_kd * cs_soil(j)%ly(1)%path(ipath) * qday /                        &
                 (soil(j)%phys(1)%bd * soil(j)%phys(1)%d * path_db(ipath_db)%kd)
        hpath_bal(j)%path(ipath)%surq = Min(hpath_bal(j)%path(ipath)%surq, cs_soil(j)%ly(1)%path(ipath))
        hpath_bal(j)%path(ipath)%surq = Max(cs_soil(j)%ly(1)%path(ipath), 0.)
        cs_soil(j)%ly(1)%path(ipath) = cs_soil(j)%ly(1)%path(ipath) - hpath_bal(j)%path(ipath)%surq

        !! compute bacteria transported with sediment
        if (enratio > 0.) then 
          cpath = (1. - path_kd) * cs_soil(j)%ly(1)%path(ipath) * enratio / soil(j)%phys(1)%conv_wt
          hpath_bal(j)%path(ipath)%sed = .0001 * cpath * sedyld(j) / (hru(j)%area_ha + 1.e-6)
          hpath_bal(j)%path(ipath)%sed = Min(hpath_bal(j)%path(ipath)%sed, cs_soil(j)%ly(1)%path(ipath))
          cs_soil(j)%ly(1)%path(ipath) = cs_soil(j)%ly(1)%path(ipath) - hpath_bal(j)%path(ipath)%sed
        end if
      end do
      
      return
      end subroutine path_ls_runoff