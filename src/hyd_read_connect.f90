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
      
      implicit none
      
      integer, intent(in) :: nhyds    !           |
      integer, intent(in) :: ndsave   !           |
      integer, intent(in) :: nspu     !           | 
      integer, intent(in) :: nspu1    !           |
      character (len=80) :: titldum   !           |title of file
      character (len=80) :: header    !           |header of file
      character (len=16) :: namedum   !           |
      integer :: eof                  !           |end of file
      integer :: imax                 !none       |determine max number for array (imax) and total number in file
      logical :: i_exist              !none       |check to determine if file exists
      character (len=16) ::con_file   !           |
      character (len=3) :: ihtyp      !           |
      character (len=8) :: obtyp      !           |
      integer :: isp                  !none       |counter
      integer :: cmd_prev             !none       |previous command (object) number
      integer :: ob1                  !none       |beginning of loop
      integer :: ob2                  !none       |ending of loop
      integer :: iob
      integer :: i                    !none       |counter
      integer :: nout                 !           |       
      integer :: iout                 !           |       
      integer :: k                    !           |
      integer :: ics                  !           |
      integer :: ihyd                 !           |hydrograph counter
      integer :: npests               !           |pesticides counter
      integer :: npaths               !           |pathogens counter
      integer :: nmetals              !           |heavy metals counter
      integer :: nsalts               !           |salts counter
      integer :: aqu_found            !           |rtb gwflow
      
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

            do i = ob1, ob2
              ob(i)%typ = obtyp
              ob(i)%nhyds = nhyds
              allocate (ob(i)%hd(nhyds))
              if (obtyp == "hru") then
                allocate (ob(i)%hd_aa(nhyds))
                ob(i)%hd_aa(:) = hz
              end if
              if (cs_db%num_tot > 0) then
                allocate (obcs(i)%hd(nhyds))
                npests = cs_db%num_pests
                if (npests > 0) then
                  allocate (obcs(i)%hin%pest(npests))
                  allocate (obcs(i)%hin_sur%pest(npests))
                  allocate (obcs(i)%hin_lat%pest(npests))
                  allocate (obcs(i)%hin_til%pest(npests))
                end if
                npaths = cs_db%num_paths
                if (npaths > 0) then
                  allocate (obcs(i)%hin%path(npaths))
                  allocate (obcs(i)%hin_sur%path(npaths))
                  allocate (obcs(i)%hin_lat%path(npaths))
                  allocate (obcs(i)%hin_til%path(npaths))
                end if
                nmetals = cs_db%num_metals
                if (nmetals > 0) then 
                  allocate (obcs(i)%hin%hmet(nmetals))
                  allocate (obcs(i)%hin_sur%hmet(nmetals))
                  allocate (obcs(i)%hin_lat%hmet(nmetals))
                  allocate (obcs(i)%hin_til%hmet(nmetals))
                end if
                nsalts = cs_db%num_salts
                if (nsalts > 0) then 
                  allocate (obcs(i)%hin%salt(nsalts))
                  allocate (obcs(i)%hin_sur%salt(nsalts))
                  allocate (obcs(i)%hin_lat%salt(nsalts))
                  allocate (obcs(i)%hin_til%salt(nsalts))
                end if
                
                do ihyd = 1, nhyds
                  if (npests > 0) then 
                    allocate (obcs(i)%hd(ihyd)%pest(npests))
                  end if
                  if (npaths > 0) then 
                    allocate (obcs(i)%hd(ihyd)%path(npaths))
                  end if
                  if (nmetals > 0) then 
                    allocate (obcs(i)%hd(ihyd)%hmet(nmetals))
                  end if
                  if (nsalts > 0) then 
                    allocate (obcs(i)%hd(ihyd)%salt(nsalts))
                  end if
                end do
              end if
                
              if (time%step > 0) then
                ob(i)%day_max = ndsave
                allocate (ob(i)%ts(ob(i)%day_max,time%step))
                allocate (ob(i)%tsin(time%step))
                allocate (ob(i)%uh(ob(i)%day_max,time%step))
                allocate (ob(i)%hyd_flo(ob(i)%day_max,time%step))
                ob(i)%uh = 0.
                ob(i)%hyd_flo = 0.
              end if
              read (107,*,iostat=eof) ob(i)%num, ob(i)%name, ob(i)%gis_id, ob(i)%area_ha, ob(i)%lat, ob(i)%long, &
                ob(i)%elev, ob(i)%props, ob(i)%wst_c, ob(i)%constit, ob(i)%props2, ob(i)%ruleset, ob(i)%src_tot
              
              !! intialize area to calcluate drainage areas in hyd_connect
              if (ob(i)%typ == "hru" .or. ob(i)%typ == "ru" .or. ob(i)%typ == "recall") then
                ob(i)%area_ha_calc = ob(i)%area_ha
              else
                ob(i)%area_ha_calc = 0.
              end if
              
              if (eof < 0) exit

              if (ob(i)%src_tot > 0) then
                nout = ob(i)%src_tot
                allocate (ob(i)%obj_out(nout))
                allocate (ob(i)%obtyp_out(nout))
                allocate (ob(i)%obtypno_out(nout))
                allocate (ob(i)%htyp_out(nout))
                allocate (ob(i)%ihtyp_out(nout))
                allocate (ob(i)%frac_out(nout))
                allocate (ob(i)%rcvob_inhyd(nout))
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
                      allocate (obcs(i)%hcsout_m(iout)%pest(npests))
                      allocate (obcs(i)%hcsout_y(iout)%pest(npests))
                      allocate (obcs(i)%hcsout_a(iout)%pest(npests))
                    end if
                    npaths = cs_db%num_paths
                    if (npaths > 0) then 
                      allocate (obcs(i)%hcsout_m(iout)%path(npaths))
                      allocate (obcs(i)%hcsout_y(iout)%path(npaths))
                      allocate (obcs(i)%hcsout_a(iout)%path(npaths))
                    end if
                    
                  if (nmetals > 0) then 
                    allocate (obcs(i)%hcsout_m(iout)%hmet(nmetals))
                    allocate (obcs(i)%hcsout_y(iout)%hmet(nmetals))
                    allocate (obcs(i)%hcsout_a(iout)%hmet(nmetals))
                  end if
                  if (nsalts > 0) then 
                    allocate (obcs(i)%hcsout_m(iout)%salt(nsalts))
                    allocate (obcs(i)%hcsout_y(iout)%salt(nsalts))
                    allocate (obcs(i)%hcsout_a(iout)%salt(nsalts))
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
                  if(aqu_found.eq.1) then
                    ob(i)%src_tot = ob(i)%src_tot - 1
                  endif
                  
                  if (eof < 0) exit
              else
                !! set outflow object type to 0 - needed in final hyd_read_connect loop 
                allocate (ob(i)%obtypno_out(1))
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