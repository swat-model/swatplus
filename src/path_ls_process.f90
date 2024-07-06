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
      integer :: ipl        !none          |plant number
      integer :: ipath_db   !none          |pathogen number from data file
      integer :: isp_ini    !none          |soil-plant initialization number from data file
      real :: pl_ini        !              |
      real :: sol_ini       !              |
      real :: pl_die_gro
      real :: sol_die_gro
      real :: bacdiegrosol_out
      real :: bacdiegroplt_out
      real :: theta
      real :: wash_off      !               |pathogen wash off

      j = ihru
         
      pl_die_gro = 0.
      sol_die_gro = 0.
      
      do ipath = 1, cs_db%num_paths
        isp_ini = hru(ihru)%dbs%soil_plant_init
        ipath_db = sol_plt_ini(isp_ini)%path
        hpath_bal(j)%path(ipath)%wash = 0.
        bacdiegroplt_out = 0.
        
      do ipl = 1, pcom(j)%npl
        !! compute pathogen wash off
        if (w%precip >= 2.54) then
          wash_off = path_db(ipath_db)%washoff * cs_pl(j)%pl_on(ipl)%path(ipath)
          if (wash_off > cs_pl(j)%pl_on(ipl)%path(ipath)) wash_off = cs_pl(j)%pl_on(ipl)%path(ipath)
          cs_soil(j)%ly(1)%path(ipath) = cs_soil(j)%ly(1)%path(ipath) + wash_off
          cs_pl(j)%pl_on(ipl)%path(ipath) = cs_pl(j)%pl_on(ipl)%path(ipath) - wash_off
          hpath_bal(j)%path(ipath)%wash = hpath_bal(j)%path(ipath)%wash + wash_off
        end if

        !! compute pathogen die-off and re-growth on foilage
        pl_ini = cs_pl(j)%pl_on(ipl)%path(ipath)
        pl_die_gro = path_db(ipath_db)%do_plnt - path_db(ipath_db)%gr_plnt
        cs_pl(j)%pl_on(ipl)%path(ipath) = cs_pl(j)%pl_on(ipl)%path(ipath) *                                 &
                Exp(-Theta(pl_die_gro, path_db(ipath_db)%t_adj, w%tave)) - path_db(ipath_db)%conc_min
        cs_pl(j)%pl_on(ipl)%path(ipath) = Max(0., cs_pl(j)%pl_on(ipl)%path(ipath))
        if (cs_pl(j)%pl_on(ipl)%path(ipath) < path_db(ipath_db)%conc_min)                                   &
                                         cs_pl(j)%pl_on(ipl)%path(ipath) = path_db(ipath_db)%conc_min
        bacdiegroplt_out = bacdiegroplt_out + (pl_ini - cs_pl(j)%pl_on(ipl)%path(ipath))
      end do
      
        !! compute pathogen die-off and re-growth in surface soil layer
        sol_ini = cs_soil(j)%ly(1)%path(ipath)
        sol_die_gro = path_db(ipath_db)%do_soln - path_db(ipath_db)%gr_soln
        cs_soil(j)%ly(1)%path(ipath) = cs_soil(j)%ly(1)%path(ipath) * Exp(-Theta(sol_die_gro, path_db(ipath_db)%t_adj,  & 
                                                w%tave)) - path_db(ipath_db)%conc_min
        cs_soil(j)%ly(1)%path(ipath) = Max(0., cs_soil(j)%ly(1)%path(ipath))
        if (cs_soil(j)%ly(1)%path(ipath) < path_db(ipath_db)%conc_min) cs_soil(j)%ly(1)%path(ipath) = path_db(ipath_db)%conc_min

        bacdiegrosol_out = sol_ini - cs_soil(j)%ly(1)%path(ipath)
        !! net die_off - negative is regrowth
        hpath_bal(j)%path(ipath)%die_off = bacdiegrosol_out + bacdiegroplt_out
        
      end do

      return
      end subroutine path_ls_process