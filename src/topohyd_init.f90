      subroutine topohyd_init
    
      use hydrograph_module, only : sp_ob, sp_ob1, ob
      use hru_module, only : hru, ihru, snodb
      use hydrology_data_module
      use topography_data_module
      use soil_data_module
      use plant_module
      
      implicit none

      integer :: isno                 !none       |counter
      integer :: ifield_db            !           |
      integer :: itopohd_db           !           |
      integer :: ihyd_db              !           |
      integer :: itopo_db             !           |
      integer :: isno_db
      integer :: iob                  !           |
      real :: perc_ln_func            !none       |function to convert perco to perc_lim
    
      !!assign topography and hyd parameters
      do ihru = 1, sp_ob%hru
        iob = sp_ob1%hru + ihru - 1
        itopo_db = hru(ihru)%dbs%topo
        ihyd_db = hru(ihru)%dbs%hyd
        itopohd_db = hru(ihru)%dbs%topo
        ifield_db = hru(ihru)%dbs%field
        hru(ihru)%topo%name = topo_db(itopo_db)%name
        hru(ihru)%topo%elev = ob(iob)%elev
        hru(ihru)%topo%slope = topo_db(itopohd_db)%slope
        hru(ihru)%topo%slope_len = topo_db(itopohd_db)%slope_len
        hru(ihru)%hyd%name = hyd_db(ihyd_db)%name
        hru(ihru)%hyd%lat_ttime = hyd_db(ihyd_db)%lat_ttime
        hru(ihru)%hyd%lat_sed = hyd_db(ihyd_db)%lat_sed
        hru(ihru)%topo%lat_len = topo_db(itopohd_db)%lat_len
        if (hru(ihru)%topo%lat_len < 1.e-6) hru(ihru)%topo%lat_len = 50.
        hru(ihru)%hyd%canmx = hyd_db(ihyd_db)%canmx
        hru(ihru)%hyd%esco = hyd_db(ihyd_db)%esco
        !hru(ihru)%hyd%esco = 1.0
        hru(ihru)%hyd%epco = hyd_db(ihyd_db)%epco
        hru(ihru)%hyd%erorgn = hyd_db(ihyd_db)%erorgn
        hru(ihru)%hyd%erorgp = hyd_db(ihyd_db)%erorgp
        hru(ihru)%hyd%cn3_swf = hyd_db(ihyd_db)%cn3_swf
        hru(ihru)%hyd%perco = hyd_db(ihyd_db)%perco
        
        !! set hru snow paramters
        isno_db = hru(ihru)%dbs%snow
        hru(ihru)%sno = snodb(isno_db)
        !! shape parameters to describes area of snow cover as a function of amount of snow
        call ascrv(.5, .95, hru(ihru)%sno%cov50, .95, hru(ihru)%snocov1, hru(ihru)%snocov2)

        !! try setting for tile
        if (hru(ihru)%tiledrain > 0) then
          hru(ihru)%hyd%cn3_swf = 0.95
          hru(ihru)%hyd%perco = 0.1
        end if
        
        if (hru(ihru)%hyd%perco > 1.e-9) then
          perc_ln_func = 1.0052 * log(-log(hru(ihru)%hyd%perco - 1.e-6)) + 5.6862
          hru(ihru)%hyd%perco_lim = exp(-perc_ln_func)
          hru(ihru)%hyd%perco_lim = amin1 (1., hru(ihru)%hyd%perco_lim)
        else
          hru(ihru)%hyd%perco_lim = 0.
        end if
        
        hru(ihru)%topo%dis_stream = topo_db(itopohd_db)%dis_stream
        hru(ihru)%hyd%biomix = hyd_db(ihyd_db)%biomix
        hru(ihru)%hyd%lat_orgn = hyd_db(ihyd_db)%lat_orgn
        hru(ihru)%hyd%lat_orgp = hyd_db(ihyd_db)%lat_orgp
        hru(ihru)%hyd%latq_co = hyd_db(ihyd_db)%latq_co
        hru(ihru)%hyd%pet_co = hyd_db(ihyd_db)%pet_co
        !! changing hrg_pet to pet_co - works for old harg_pet input and new pet_co 
        if (hru(ihru)%hyd%pet_co < 1.e-6) then
          hru(ihru)%hyd%pet_co = 1.
        else
          if (hru(ihru)%hyd%pet_co < 0.01) then
            hru(ihru)%hyd%pet_co = hru(ihru)%hyd%pet_co / .0023
          end if
        end if
        
        ! set field data
        hru(ihru)%field%length = field_db(ifield_db)%length
        hru(ihru)%field%wid = field_db(ifield_db)%wid
        hru(ihru)%field%ang = field_db(ifield_db)%ang
        hru(ihru)%topo%dep_co = topo_db(itopohd_db)%dep_co
        ! set initial snow cover
        isno = hru(ihru)%dbs%snow 
        hru(ihru)%sno_mm = snodb(isno)%init_mm
        
        ! initialize hydrology parms for water balance soft cal (cn3_swf)
        hru(ihru)%hydcal = hru(ihru)%hyd
        
      end do
      
      return
      end subroutine topohyd_init