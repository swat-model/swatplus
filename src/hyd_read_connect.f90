      subroutine hyd_read_connect(con_file, obtyp, nspu1, nspu, nhyds, ndsave)
    
      !  con_file ==> connect file for spatial object
      !  nspu     ==> number of spatial units
      !  nspu1    ==> first object number of the spatial unit
      !  nhyds    ==> number of hydrographs for each object
      !  ndsave   ==> number of days of hydrographs to save for subdaily

      use hydrograph_module
      use constituent_mass_module
      use time_module
      use climate_module
      use maximum_data_module
      use gwflow_module, only: nat_model
      
      implicit none
      
      integer, intent(in) :: nhyds    !           |
      integer, intent(in) :: ndsave   !           |
      integer, intent(in) :: nspu     !           | 
      integer, intent(in) :: nspu1    !           |
      character (len=80) :: titldum = ""!           |title of file
      character (len=80) :: header = "" !           |header of file
      integer :: eof = 0              !           |end of file
      integer :: imax = 0             !none       |determine max number for array (imax) and total number in file
      logical :: i_exist              !none       |check to determine if file exists
      character (len=20) :: con_file  !           |
      character (len=8) :: obtyp      !           |
      integer :: isp = 0              !none       |counter
      integer :: cmd_prev = 0         !none       |previous command (object) number
      integer :: ob1 = 0              !none       |beginning of loop
      integer :: ob2 = 0              !none       |ending of loop
      integer :: i = 0                !none       |object counter
      integer :: isp_ob = 0           !none       |spatial object counter
      integer :: nout = 0             !           |       
      integer :: iout = 0             !           |       
      integer :: k = 0                !           |
      integer :: ihyd = 0             !           |hydrograph counter
      integer :: npests = 0           !           |pesticides counter
      integer :: npaths = 0           !           |pathogens counter
      integer :: nmetals = 0          !           |heavy metals counter
      integer :: nsalts = 0           !           |salts counter
      integer :: ncs = 0              !           |constituent counter
      integer :: aqu_found = 0        !           |rtb gwflow
      
      eof = 0
      imax = 0
      cmd_prev = 0
      aqu_found = 0
  
      !! read hru spatial data
      inquire (file=con_file, exist=i_exist)
      if (i_exist ) then
        do
          open (107,file=con_file)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit

          if (nspu > 0) then
            ob1 = nspu1
            ob2 = nspu1 + nspu - 1
            isp_ob = 0

            do i = ob1, ob2
              ob(i)%typ = obtyp
              ob(i)%nhyds = nhyds
              isp_ob = isp_ob + 1
              ob(i)%sp_ob_no = isp_ob
              allocate (ob(i)%hd(nhyds))
              allocate (ob(i)%hd_aa(nhyds))
              ob(i)%trans = hz
              ob(i)%hin_tot = hz
              ob(i)%hout_tot = hz
                                      
              ob(i)%hd_aa(:) = hz
              if (cs_db%num_tot > 0) then
                
                !set flag for allocating obcs
                obcs_alloc(i) = 1
              
                !allocate
                allocate (obcs(i)%hd(nhyds))
                
                !allocate additional hydrographs
                allocate (obcs(i)%hin(1))
                allocate (obcs(i)%hin_sur(1))
                allocate (obcs(i)%hin_lat(1))
                allocate (obcs(i)%hin_til(1))
                allocate (obcs(i)%hin_aqu(1))
                
                npests = cs_db%num_pests
                if (npests > 0) then
                  allocate (obcs(i)%hin(1)%pest(npests), source = 0.)
                  allocate (obcs(i)%hin_sur(1)%pest(npests), source = 0.)
                  allocate (obcs(i)%hin_lat(1)%pest(npests), source = 0.)
                  allocate (obcs(i)%hin_til(1)%pest(npests), source = 0.)
                  
                end if
                npaths = cs_db%num_paths
                if (npaths > 0) then
                  allocate (obcs(i)%hin(1)%path(npaths), source = 0.)
                  allocate (obcs(i)%hin_sur(1)%path(npaths), source = 0.)
                  allocate (obcs(i)%hin_lat(1)%path(npaths), source = 0.)
                  allocate (obcs(i)%hin_til(1)%path(npaths), source = 0.)
                end if
                nmetals = cs_db%num_metals
                if (nmetals > 0) then 
                  allocate (obcs(i)%hin(1)%hmet(nmetals), source = 0.)
                  allocate (obcs(i)%hin_sur(1)%hmet(nmetals), source = 0.)
                  allocate (obcs(i)%hin_lat(1)%hmet(nmetals), source = 0.)
                  allocate (obcs(i)%hin_til(1)%hmet(nmetals), source = 0.)
                end if
                nsalts = cs_db%num_salts
                if (nsalts > 0) then 
                  allocate (obcs(i)%hin(1)%salt(nsalts), source = 0.)
                  allocate (obcs(i)%hin_sur(1)%salt(nsalts), source = 0.)
                  allocate (obcs(i)%hin_lat(1)%salt(nsalts), source = 0.)
                  allocate (obcs(i)%hin_til(1)%salt(nsalts), source = 0.)
                  allocate (obcs(i)%hin(1)%salt_min(nsalts), source = 0.)
                  allocate (obcs(i)%hin(1)%saltc(nsalts), source = 0.)
                  allocate (obcs(i)%hin_sur(1)%salt_min(nsalts), source = 0.)
                  allocate (obcs(i)%hin_sur(1)%saltc(nsalts), source = 0.)
                  allocate (obcs(i)%hin_lat(1)%salt_min(nsalts), source = 0.)
                  allocate (obcs(i)%hin_lat(1)%saltc(nsalts), source = 0.)
                  allocate (obcs(i)%hin_til(1)%salt_min(nsalts), source = 0.)
                  allocate (obcs(i)%hin_til(1)%saltc(nsalts), source = 0.)
                  obcs(i)%hin(1)%salt = 0.
                  obcs(i)%hin(1)%salt_min = 0.
                  obcs(i)%hin(1)%saltc = 0.
                  obcs(i)%hin_sur(1)%salt = 0.
                  obcs(i)%hin_sur(1)%salt_min = 0.
                  obcs(i)%hin_sur(1)%saltc = 0.
                  obcs(i)%hin_lat(1)%salt = 0.
                  obcs(i)%hin_lat(1)%salt_min = 0.
                  obcs(i)%hin_lat(1)%saltc = 0.
                  obcs(i)%hin_til(1)%salt = 0.
                  obcs(i)%hin_til(1)%salt_min = 0.
                  obcs(i)%hin_til(1)%saltc = 0.
                end if
                ncs = cs_db%num_cs !rtb cs
                if (ncs > 0) then 
                  allocate (obcs(i)%hin(1)%cs(ncs), source = 0.)
                  allocate (obcs(i)%hin(1)%cs_sorb(ncs), source = 0.)
                  allocate (obcs(i)%hin(1)%csc(ncs), source = 0.)
                  allocate (obcs(i)%hin(1)%csc_sorb(ncs), source = 0.)
                  allocate (obcs(i)%hin_sur(1)%cs(ncs), source = 0.)
                  allocate (obcs(i)%hin_sur(1)%cs_sorb(ncs), source = 0.)
                  allocate (obcs(i)%hin_sur(1)%csc(ncs), source = 0.)
                  allocate (obcs(i)%hin_sur(1)%csc_sorb(ncs), source = 0.)
                  allocate (obcs(i)%hin_lat(1)%cs(ncs), source = 0.)
                  allocate (obcs(i)%hin_lat(1)%cs_sorb(ncs), source = 0.)
                  allocate (obcs(i)%hin_lat(1)%csc(ncs), source = 0.)
                  allocate (obcs(i)%hin_lat(1)%csc_sorb(ncs), source = 0.)
                  allocate (obcs(i)%hin_til(1)%cs(ncs), source = 0.)
                  allocate (obcs(i)%hin_til(1)%cs_sorb(ncs), source = 0.)
                  allocate (obcs(i)%hin_til(1)%csc(ncs), source = 0.)
                  allocate (obcs(i)%hin_til(1)%csc_sorb(ncs), source = 0.)
                  obcs(i)%hin(1)%cs = 0.
                  obcs(i)%hin(1)%cs_sorb = 0.
                  obcs(i)%hin(1)%csc = 0.
                  obcs(i)%hin(1)%csc_sorb = 0.
                  obcs(i)%hin_sur(1)%cs = 0.
                  obcs(i)%hin_sur(1)%cs_sorb = 0.
                  obcs(i)%hin_sur(1)%csc = 0.
                  obcs(i)%hin_sur(1)%csc_sorb = 0.
                  obcs(i)%hin_lat(1)%cs = 0.
                  obcs(i)%hin_lat(1)%cs_sorb = 0.
                  obcs(i)%hin_lat(1)%csc_sorb = 0.
                  obcs(i)%hin_lat(1)%csc = 0.
                  obcs(i)%hin_til(1)%cs = 0.
                  obcs(i)%hin_til(1)%cs_sorb = 0.
                  obcs(i)%hin_til(1)%csc = 0.  
                  obcs(i)%hin_til(1)%csc_sorb = 0.
                endif
                
                do ihyd = 1, nhyds
                  if (npests > 0) then 
                    allocate (obcs(i)%hd(ihyd)%pest(npests))
                  end if
                  if (npaths > 0) then 
                    allocate (obcs(i)%hd(ihyd)%path(npaths), source = 0.)
                  end if
                  if (nmetals > 0) then 
                    allocate (obcs(i)%hd(ihyd)%hmet(nmetals), source = 0.)
                  end if
                  if (nsalts > 0) then !rtb salt
                    allocate (obcs(i)%hd(ihyd)%salt(nsalts))
                    allocate (obcs(i)%hd(ihyd)%salt_min(nsalts), source = 0.)
                    allocate (obcs(i)%hd(ihyd)%saltc(nsalts), source = 0.)
                    obcs(i)%hd(ihyd)%salt = 0.
                    obcs(i)%hd(ihyd)%salt_min = 0.
                    obcs(i)%hd(ihyd)%saltc = 0.
                  end if
                  if (ncs > 0) then !rtb cs
                    allocate (obcs(i)%hd(ihyd)%cs(ncs), source = 0.)
                    allocate (obcs(i)%hd(ihyd)%cs_sorb(ncs), source = 0.)
                    allocate (obcs(i)%hd(ihyd)%csc(ncs), source = 0.)
                    allocate (obcs(i)%hd(ihyd)%csc_sorb(ncs), source = 0.)
                    obcs(i)%hd(ihyd)%cs = 0.
                    obcs(i)%hd(ihyd)%cs_sorb = 0.
                    obcs(i)%hd(ihyd)%csc = 0.
                    obcs(i)%hd(ihyd)%csc_sorb = 0.
                  end if
                end do
              end if
                
              !if (time%step > 0) then
                ob(i)%day_max = ndsave
                allocate (ob(i)%ts(ob(i)%day_max,time%step))
                allocate (ob(i)%tsin(time%step), source = 0.)
                allocate (ob(i)%uh(ob(i)%day_max,time%step), source = 0.)
                allocate (ob(i)%hyd_flo(ob(i)%day_max,time%step), source = 0.)
                ob(i)%uh = 0.
                ob(i)%hyd_flo = 0.
              !end if
              read (107,*,iostat=eof) ob(i)%num, ob(i)%name, ob(i)%gis_id, ob(i)%area_ha, ob(i)%lat, ob(i)%long, &
                ob(i)%elev, ob(i)%props, ob(i)%wst_c, ob(i)%constit, ob(i)%props2, ob(i)%ruleset, ob(i)%src_tot
              
              !! initialize area to calculate drainage areas in hyd_connect
              if (ob(i)%typ == "hru" .or. ob(i)%typ == "ru" .or. ob(i)%typ == "recall") then
                ob(i)%area_ha_calc = ob(i)%area_ha
              else
                ob(i)%area_ha_calc = 0.
              end if
              
              if (eof < 0) exit

              if (ob(i)%src_tot > 0) then
                nout = ob(i)%src_tot
                allocate (ob(i)%obj_out(nout), source = 0)
                allocate (ob(i)%obtyp_out(nout))
                allocate (ob(i)%obtypno_out(nout), source = 0)
                allocate (ob(i)%htyp_out(nout))
                allocate (ob(i)%ihtyp_out(nout), source = 0)
                allocate (ob(i)%frac_out(nout), source = 0.)
                allocate (ob(i)%rcvob_inhyd(nout), source = 0)
                allocate (ob(i)%hout_m(nout))
                allocate (ob(i)%hout_y(nout))
                allocate (ob(i)%hout_a(nout))
                allocate (obcs(i)%hcsout_m(nout))
                allocate (obcs(i)%hcsout_y(nout))
                allocate (obcs(i)%hcsout_a(nout))
                
                if (cs_db%num_tot > 0) then
                  npests = cs_db%num_pests
                  do iout = 1, nout
                    if (npests > 0) then 
                      allocate (obcs(i)%hcsout_m(iout)%pest(npests), source = 0.)
                      allocate (obcs(i)%hcsout_y(iout)%pest(npests), source = 0.)
                      allocate (obcs(i)%hcsout_a(iout)%pest(npests), source = 0.)
                      
                    end if
                    npaths = cs_db%num_paths
                    if (npaths > 0) then 
                      allocate (obcs(i)%hcsout_m(iout)%path(npaths), source = 0.)
                      allocate (obcs(i)%hcsout_y(iout)%path(npaths), source = 0.)
                      allocate (obcs(i)%hcsout_a(iout)%path(npaths), source = 0.)
                    end if
                    
                  if (nmetals > 0) then 
                    allocate (obcs(i)%hcsout_m(iout)%hmet(nmetals), source = 0.)
                    allocate (obcs(i)%hcsout_y(iout)%hmet(nmetals), source = 0.)
                    allocate (obcs(i)%hcsout_a(iout)%hmet(nmetals), source = 0.)
                  end if
                  if (nsalts > 0) then !rtb salt
                    allocate (obcs(i)%hcsout_m(iout)%salt(nsalts), source = 0.)
                    allocate (obcs(i)%hcsout_m(iout)%salt_min(nsalts), source = 0.)
                    allocate (obcs(i)%hcsout_m(iout)%saltc(nsalts), source = 0.)
                    allocate (obcs(i)%hcsout_y(iout)%salt(nsalts), source = 0.)
                    allocate (obcs(i)%hcsout_y(iout)%salt_min(nsalts), source = 0.)
                    allocate (obcs(i)%hcsout_y(iout)%saltc(nsalts), source = 0.)
                    allocate (obcs(i)%hcsout_a(iout)%salt(nsalts), source = 0.)
                    allocate (obcs(i)%hcsout_a(iout)%salt_min(nsalts), source = 0.)
                    allocate (obcs(i)%hcsout_a(iout)%saltc(nsalts), source = 0.)
                  end if
                  if (ncs > 0) then !rtb cs
                    allocate (obcs(i)%hcsout_m(iout)%cs(ncs), source = 0.)
                    allocate (obcs(i)%hcsout_m(iout)%cs_sorb(ncs), source = 0.)
                    allocate (obcs(i)%hcsout_m(iout)%csc(ncs), source = 0.)
                    allocate (obcs(i)%hcsout_m(iout)%csc_sorb(ncs), source = 0.)
                    allocate (obcs(i)%hcsout_y(iout)%cs(ncs), source = 0.)
                    allocate (obcs(i)%hcsout_y(iout)%cs_sorb(ncs), source = 0.)
                    allocate (obcs(i)%hcsout_y(iout)%csc(ncs), source = 0.)
                    allocate (obcs(i)%hcsout_y(iout)%csc_sorb(ncs), source = 0.)
                    allocate (obcs(i)%hcsout_a(iout)%cs(ncs), source = 0.)
                    allocate (obcs(i)%hcsout_a(iout)%cs_sorb(ncs), source = 0.)
                    allocate (obcs(i)%hcsout_a(iout)%csc(ncs), source = 0.)
                    allocate (obcs(i)%hcsout_a(iout)%csc_sorb(ncs), source = 0.)
                  end if
                end do
                end if

                backspace (107)
                read (107,*,iostat=eof) ob(i)%num, ob(i)%name, ob(i)%gis_id, ob(i)%area_ha, ob(i)%lat, ob(i)%long, ob(i)%elev,    &
                  ob(i)%props, ob(i)%wst_c, ob(i)%constit, ob(i)%props2, ob(i)%ruleset, ob(i)%src_tot,      &
                  (ob(i)%obtyp_out(isp), ob(i)%obtypno_out(isp), ob(i)%htyp_out(isp),                       &
                  ob(i)%frac_out(isp), isp = 1, nout)
                
                  !rtb gwflow
                  if (sp_ob%gwflow > 0) then
                    aqu_found = 0
                    do k=1,ob(i)%src_tot
                      if(ob(i)%obtyp_out(k).eq.'aqu') then
                        aqu_found = 1  
                      endif
                    enddo
                  endif
                  if(aqu_found.eq.1 .and. nat_model == 1) then
                    ob(i)%src_tot = ob(i)%src_tot - 1
                  endif
                  
                  if (eof < 0) exit
              else
                !! set outflow object type to 0 - needed in final hyd_read_connect loop 
                allocate (ob(i)%obtypno_out(1), source = 0)
                ob(i)%obtypno_out(1) = 0
              end if

              !set arrays for flow duration curves
              !if (printcode == 1) then
                allocate (ob(i)%fdc_ll(366))
                allocate (ob(i)%fdc_lla(time%nbyr))
                allocate (ob(i)%fdc%yr(time%nbyr))
              !end if
          
            end do
          endif
          exit
        enddo
      endif
      
      !crosswalk weather station 
      do i = ob1, ob2
        call search (wst_n, db_mx%wst, ob(i)%wst_c, ob(i)%wst)
      end do
      
      
      close (107)
      
      return
      end subroutine hyd_read_connect