      subroutine hru_fr_change (lsu_elem_upd, ru_elem_upd)
    
      use hydrograph_module
      use maximum_data_module
      use dr_module
      use calibration_data_module
      use hru_module, only : hru
      use reservoir_data_module
      use reservoir_module
      use ru_module
      
      implicit none
  
      character(len=25), intent (in) :: lsu_elem_upd    !file name of updated lsu_unit.ele 
      character(len=25), intent (in) :: ru_elem_upd     !file name of updated rout_unit.ele 
      character (len=80) :: titldum   !           |title of file
      character (len=80) :: header    !           |header of file
      integer :: eof                  !           |end of file
      logical :: i_exist              !none       |check to determine if file exists
      integer :: i                    !none       |counter
      integer :: isp                  !none       |counter
      integer :: k                    !           |
      integer :: iob                  !           |
      integer :: idr                  !none       |counter
      integer :: ii                   !none       |counter
      integer :: ihru                 !none       |counter
      integer :: iprop
      integer :: ihyd
      integer :: ielem
      
      eof = 0
      
      !!read data for each element in all routing units
      inquire (file=ru_elem_upd, exist=i_exist)
      if (i_exist .or. ru_elem_upd /= "null") then
        do
        open (107,file=ru_elem_upd)

        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit

        do isp = 1, db_mx%ru_elem
          read (107,*,iostat=eof) i
          if (eof < 0) exit
          backspace (107)
          read (107,*,iostat=eof) k, ru_elem(i)%name, ru_elem(i)%obtyp, ru_elem(i)%obtypno,     &
                                ru_elem(i)%frac, ru_elem(i)%dr_name
          if (eof < 0) exit
          
          ! xwalk ru_elem(i)%dr_name with dr_db()%name from delratio.del file
          do idr = 1, db_mx%dr_om
            if (ru_elem(i)%dr_name == dr_db(idr)%name) then
              !! dr_om_num was previously xwalked with dr_db()%om_file
              ru_elem(i)%dr = dr(dr_om_num(idr))
              exit
            end if
          end do
      
        end do
        exit
      end do
      end if
      
      close (107)

      !!read data for each element in all landscape cataloging units
      inquire (file=lsu_elem_upd, exist=i_exist)
      if (i_exist .or. lsu_elem_upd /= "null") then
      do
        open (107,file=lsu_elem_upd)

        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit

        do isp = 1, db_mx%lsu_elem
          read (107,*,iostat=eof) i
          if (eof < 0) exit
          backspace (107)
          read (107,*,iostat=eof) k, lsu_elem(i)%name, lsu_elem(i)%obtyp, lsu_elem(i)%obtypno,      &
                                    lsu_elem(i)%bsn_frac, lsu_elem(i)%ru_frac
          if (eof < 0) exit
        end do
        exit
      end do
      end if

      close (107)
    
      !! set new hru areas
      do i = 1, db_mx%lsu_elem
        if (lsu_elem(i)%obtyp == "hru") then
          ihru = lsu_elem(i)%obtypno
          hru(ihru)%area_ha = lsu_elem(i)%bsn_frac * bsn%area_ls_ha
          hru(ihru)%km = hru(ihru)%area_ha / 100.
          iob = hru(ihru)%obj_no
          ob(iob)%area_ha = hru(ihru)%area_ha
        end if
      end do
    
      !! reset wetland parameters
      do ihru = 1, sp_ob%hru
        !! reset volumes and surface areas
        iprop = hru(ihru)%dbs%surf_stor
        if (iprop > 0) then
          ihyd = wet_dat(iprop)%hyd
          !! ha*mm*10. => m**3  - assume entire hru is wet and don't use fractional inputs (for simplicity)
          wet_ob(ihru)%evol = hru(ihru)%area_ha * wet_hyd(ihyd)%edep * 10.  ! * wet_hyd(ihyd)%esa
          wet_ob(ihru)%pvol = hru(ihru)%area_ha * wet_hyd(ihyd)%pdep * 10.  ! * wet_hyd(ihyd)%psa
          wet_ob(ihru)%psa = wet_hyd(ihyd)%psa * hru(ihru)%area_ha 
          wet_ob(ihru)%esa = wet_hyd(ihyd)%esa * hru(ihru)%area_ha
        end if
      end do
      
      !! compute weighted Mannings n for each subbasin
      do iru = 1, sp_ob%ru
        ru_n(iru) = 0.
        do ii = 1, ru_def(iru)%num_tot
          ielem = ru_def(iru)%num(ii)
          if (ru_elem(ielem)%obtyp == "hru") then
            ihru = ru_elem(ielem)%obtypno 
            ru_n(iru) = ru_n(iru) + hru(ihru)%luse%ovn * hru(ihru)%km
          else
            ru_n(iru) = 0.1
          end if
        end do
      end do
            
      return
      end subroutine hru_fr_change