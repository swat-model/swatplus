      subroutine path_ls_process
    
      use pathogen_data_module
      use constituent_mass_module
      use output_ls_pathogen_module
      use hru_module, only : hru, sol_plt_ini, ihru
      use soil_module
      use plant_module
      use climate_module
      
      implicit none

      integer :: j          !none          |hru number
      integer :: ipath      !none          |pathogen counter
      integer :: ipath_db   !none          |pathogen number from data file
      integer :: isp_ini    !none          |soil-plant initialization number from data file
      real :: pl_ini        !              |
      real :: sol_ini       !              |
      real :: pl_die_gro
      real :: sol_die_gro
      real :: bacdiegrosol_out
      real :: bacdiegroplt_out
      real :: theta 

      j = ihru
         
      do ipath = 1, cs_db%num_paths
        isp_ini = hru(ihru)%dbs%soil_plant_init
        ipath_db = sol_plt_ini(isp_ini)%path
      
        !! compute pathogen wash off
        if (w%precip >= 2.54) then
          hpath_bal(j)%path(ipath)%wash = path_db(ipath_db)%washoff * cs_pl(j)%path(ipath)
          if (hpath_bal(j)%path(ipath)%wash > cs_pl(j)%path(ipath)) hpath_bal(j)%path(ipath)%wash = cs_pl(j)%path(ipath)
          cs_soil(j)%ly(1)%path(ipath) = cs_soil(j)%ly(1)%path(ipath) + hpath_bal(j)%path(ipath)%wash
          cs_pl(j)%path(ipath) = cs_pl(j)%path(ipath) - hpath_bal(j)%path(ipath)%wash
        end if

        !! compute pathogen die-off and re-growth on foilage
        pl_ini = cs_pl(j)%path(ipath)
        pl_die_gro = path_db(ipath_db)%do_plnt - path_db(ipath_db)%gr_plnt
        cs_pl(j)%path(ipath) = cs_pl(j)%path(ipath) * Exp(-Theta(pl_die_gro, path_db(ipath_db)%t_adj, w%tave)) -     &
                                              path_db(ipath_db)%conc_min
        cs_pl(j)%path(ipath) = Max(0., cs_pl(j)%path(ipath))
        if (cs_pl(j)%path(ipath) < path_db(ipath_db)%conc_min) cs_pl(j)%path(ipath) = path_db(ipath_db)%conc_min

        !! compute pathogen die-off and re-growth in surface soil layer
        sol_ini = cs_soil(j)%ly(1)%path(ipath)
        sol_die_gro = path_db(ipath_db)%do_soln - path_db(ipath_db)%gr_soln
        cs_soil(j)%ly(1)%path(ipath) = cs_soil(j)%ly(1)%path(ipath) * Exp(-Theta(sol_die_gro, path_db(ipath_db)%t_adj, & 
            w%tave)) - path_db(ipath_db)%conc_min
        cs_soil(j)%ly(1)%path(ipath) = Max(0., cs_soil(j)%ly(1)%path(ipath))
        if (cs_soil(j)%ly(1)%path(ipath) < path_db(ipath_db)%conc_min) cs_soil(j)%ly(1)%path(ipath) = path_db(ipath_db)%conc_min

        bacdiegrosol_out = sol_ini - cs_soil(j)%ly(1)%path(ipath)
        bacdiegroplt_out = pl_ini - cs_pl(j)%path(ipath)
        !! net die_off - negative is regrowth
        hpath_bal(j)%path(ipath)%die_off = bacdiegrosol_out + bacdiegroplt_out
        
      end do

      return
      end subroutine path_ls_process