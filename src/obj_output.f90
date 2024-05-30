      subroutine obj_output
      
      use time_module
      use hydrograph_module
      use soil_module
      use hru_module, only : ihru
      use organic_mineral_mass_module
      use plant_module
      
      implicit none
        
      integer :: ihd           !            |
      integer :: iob           !            | 
      integer :: iunit         !            |
      integer :: itot          !none        |counter       
      integer :: nly
      integer :: ly
      integer :: j
      integer :: ii
      integer :: ipl
      
      character(100) :: lineFmt !used to format plant status write statement
      
      lineFmt ='1((4X, 1A),(16X, 1A),(18X, 1A), 10(4X,F)),1(17XA, 2(12X,A), 4X, 10(4X,F)))'

      j = ihru
      
      do
        do itot = 1, mobj_out
          iob = ob_out(itot)%objno
          ihd = ob_out(itot)%hydno
          iunit = ob_out(itot)%unitno 
          
          if (iob <= sp_ob%objs) then
            select case (ob_out(itot)%hydno)
            case (1,2,3,4,5)  ! hydrograph output
              if (ob_out(itot)%hydtyp == "subday") then
                do ii = 1, time%step 
                  !write (iunit+itot,*) iob, time%yrc,time%day, ii, ob(iob)%hyd_flo(1,ii)
                  write (iunit+itot,*) time%day, time%mo, time%day_mo, time%yrc, ob(iob)%typ, ob(iob)%name, iob, ii, &
                            ob(iob)%hyd_flo(1,ii)
	            end do
              else  
                write (iunit+itot,*) time%day, time%mo, time%day_mo, time%yrc, ob(iob)%typ, ob(iob)%name, ob(iob)%hd(ihd)
              end if
              
            case (6)    ! soil water
              if (iob == 0) then
                do j = 1, sp_ob%hru
                 write (iunit+itot,*) time%day, time%mo, time%day_mo, time%yrc, ob(j)%name, ob(j)%typ, &
                   (soil(j)%phys(nly)%st, nly = 1,soil(j)%nly)
                end do
              else
                 write (iunit+itot,*) time%day, time%mo, time%day_mo, time%yrc, ob(iob)%name, ob(iob)%typ, &
                   (soil(iob)%phys(nly)%st, nly = 1,soil(iob)%nly)
              end if
            
            case (7)    ! soil layer nutrients
              if (iob == 0) then
                do j = 1, sp_ob%hru
                  do nly = 1, soil(iob)%nly
                    write (iunit+itot,*) time%day, time%mo, time%day_mo, time%yrc, ob(j)%name, ob(j)%typ,      &    
                     soil1(j)%mn(nly), soil1(j)%hact(nly)%n, soil1(j)%hsta(nly)%n, soil1(j)%hs(nly)%n,         &
                     soil1(j)%hp(nly)%n, soil1(j)%rsd(nly)%n, soil1(j)%mp(nly), soil1(j)%hact(nly)%p,          &
                     soil1(j)%hsta(nly)%p, soil1(j)%hs(nly)%p, soil1(j)%hp(nly)%p, soil1(j)%rsd(nly)%p
                  end do
                end do
              else
                write (iunit+itot,*) time%day, time%mo, time%day_mo, time%yrc, ob(j)%name, ob(j)%typ,       &    
                   soil1(j)%mn(nly), soil1(j)%hact(nly)%n, soil1(j)%hsta(nly)%n, soil1(j)%hs(nly)%n,        &
                   soil1(j)%hp(nly)%n, soil1(j)%rsd(nly)%n, soil1(j)%mp(nly), soil1(j)%hact(nly)%p,         &
                   soil1(j)%hsta(nly)%p, soil1(j)%hs(nly)%p, soil1(j)%hp(nly)%p, soil1(j)%rsd(nly)%p
              end if
              
            case (8)    ! soil entire profile nutrients 
              if (iob == 0) then
                do j = 1, sp_ob%hru
                  do ly = 1, soil(j)%nly
                    soil_prof_mn = soil_prof_mn + soil1(j)%mn(ly)
                    soil_prof_mp = soil_prof_mp + soil1(j)%mp(ly)
                    soil_prof_hact = soil_prof_hact + soil1(j)%hact(ly)
                    soil_prof_hsta = soil_prof_hsta + soil1(j)%hsta(ly)
                    soil_prof_hs = soil_prof_hs + soil1(j)%hs(ly)
                    soil_prof_hp = soil_prof_hp + soil1(j)%hp(ly)
                    soil_prof_rsd = soil_prof_rsd + soil1(j)%rsd(ly)
                  end do
                  write (iunit+itot,*) soil_prof_mn, soil_prof_mp, soil_prof_hact,       &
                     soil_prof_hsta,  soil_prof_hs, soil_prof_hp, soil_prof_rsd
                end do
              else
                j = iob
                do ly = 1, soil(j)%nly
                  soil_prof_mn = soil_prof_mn + soil1(j)%mn(ly)
                  soil_prof_mp = soil_prof_mp + soil1(j)%mp(ly)
                  soil_prof_hact = soil_prof_hact + soil1(j)%hact(ly)
                  soil_prof_hsta = soil_prof_hsta + soil1(j)%hsta(ly)
                  soil_prof_hs = soil_prof_hs + soil1(j)%hs(ly)
                  soil_prof_hp = soil_prof_hp + soil1(j)%hp(ly)
                  soil_prof_rsd = soil_prof_rsd + soil1(j)%rsd(ly)
                end do
                 write (iunit+itot,*) soil_prof_mn, soil_prof_mp, soil_prof_hact,       &
                  soil_prof_hsta,  soil_prof_hs, soil_prof_hp, soil_prof_rsd          
              end if
            
            case (9)    ! plant status
              if (iob == 0) then
                do j = 1, sp_ob%hru
                 write (iunit+itot,'(1(4I), 1((7X, 1A), (1X, 1A))'//lineFmt) time%day, time%mo,         &
                    time%day_mo, time%yrc, ob(j)%name, ob(j)%typ,                                       &
                    (pcom(j)%pl(ipl), pcom(j)%plcur(ipl)%gro, pcom(j)%plcur(ipl)%idorm,                 &
                    pcom(j)%plg(ipl)%lai, pcom(j)%plg(ipl)%cht, pcom(j)%plg(ipl)%root_dep,              &
                    pcom(j)%plcur(ipl)%phuacc, pl_mass(j)%tot(ipl)%m, pl_mass(j)%ab_gr(ipl)%m,          &
                    pl_mass(j)%leaf(ipl)%m, pl_mass(j)%root(ipl)%m, pl_mass(j)%stem(ipl)%m,             &
                    pl_mass(j)%seed(ipl)%m, ipl = 1, pcom(j)%npl)
                end do
              else
                 j = iob
                 write (iunit+itot,'(1(4I), 1((7X, 1A), (1X, 1A))'//lineFmt) time%day, time%mo,         &
                    time%day_mo, time%yrc, ob(j)%name, ob(j)%typ,  &
                    (pcom(j)%pl(ipl), pcom(j)%plcur(ipl)%gro, pcom(j)%plcur(ipl)%idorm,                  &
                    pcom(j)%plg(ipl)%lai, pcom(j)%plg(ipl)%cht, pcom(j)%plg(ipl)%root_dep,              &
                    pcom(j)%plcur(ipl)%phuacc, pl_mass(j)%tot(ipl)%m, pl_mass(j)%ab_gr(ipl)%m,          &
                    pl_mass(j)%leaf(ipl)%m, pl_mass(j)%root(ipl)%m, pl_mass(j)%stem(ipl)%m,             &
                    pl_mass(j)%seed(ipl)%m, ipl = 1, pcom(j)%npl)
              end if
            
            case (10)    ! channel and flood plain water balance
              if (iob == 0) then
                do jrch = 1, sp_ob%chandeg
                  write (iunit+itot,*) time%day, time%mo, time%day_mo, time%yrc, ob(iob)%name, ob(iob)%typ, ch_fp_wb(jrch)
                end do
              else
                jrch = ob(iob)%sp_ob_no
                write (iunit+itot,*) time%day, time%mo, time%day_mo, time%yrc, ob(iob)%name, ob(iob)%typ, ch_fp_wb(jrch)
              end if
              
            case (11)    !!!! DUMMY OBJ FOR CARBON
                !!!! check for specfic days
                if ((time%yrc == 2007 .AND. time%day == 213) .OR. (time%yrc == 2010 .AND. time%day == 319) &
                    .OR.(time%yrc == 2011 .AND. time%day == 324)) then
                   write (iunit+itot,*) '---', '    jday                    m           d           yr'
                   write (iunit+itot,*) '***',time%day, time%mo, time%day_mo, time%yrc
                    else  
                        write (iunit+itot,*) '---', '    jday        m       d       yr'
                        write (iunit+itot,*)time%day, time%mo, time%day_mo, time%yrc
                end if
                do j = 1, sp_ob%hru
                    soil_prof_microb%c = soil_org_z%c !!!! zero out microb accumalated for each hru
                        write (iunit+itot,*) ob(j)%name, "  hact         hsta         microb(ly)        tot%C     Mgc/ha", &
                                "           hact         hsta         microb(acc)        SOM%C         Mgc/ha"      
                        do nly = 1, soil(iob)%nly
                          soil1(j)%tot_org%c = soil_org_z%c !!!! zero out tot
                          soil_prof_somc%c = soil_org_z%c !!!! zero out totc
                          soil1(j)%tot_org%c = soil1(j)%hact(nly)%c + soil1(j)%hsta(nly)%c + soil1(j)%microb(nly)%c !!!! Carbon
                          soil_prof_microb%c = soil_prof_microb%c + soil1(j)%microb(nly)%c !!!! tot accumulated microb for layer
                          soil_prof_somc%c =    soil1(j)%hact(nly)%c + soil1(j)%hsta(nly)%c + soil_prof_microb%c !!!! Carbon w microb accumulated
                          
                        write (iunit+itot,*)nly, soil1(j)%hact(nly)%c, soil1(j)%hsta(nly)%c, soil1(j)%microb(nly)%c, &
                            soil1(j)%tot_org%c, (soil1(j)%tot_org%c/1000), &  
                            soil1(j)%hact(nly)%c, soil1(j)%hsta(nly)%c, soil_prof_microb%c, &
                            soil_prof_somc%c, (soil_prof_somc%c/1000)
                        end do
                      
                end do
                
            case (12)  !!!! Dummy Carbon output for hru
                    if ((time%yrc == 2007 .AND. time%day == 213) .OR. (time%yrc == 2010 .AND. time%day == 319) &
                    .OR.(time%yrc == 2011 .AND. time%day == 324)) then 
                        do nly = 1, soil(iob)%nly
                            soil1(iob)%tot_org%c = soil1(iob)%hact(nly)%c + soil1(iob)%hsta(nly)%c + soil1(iob)%microb(nly)%c
                            write (iunit+itot,*) time%day, time%mo, time%day_mo, time%yrc, ob(iob)%typ, ob(iob)%name, iob, nly, (soil1(iob)%tot_org%c/1000)
                        end do
                end if
              
            end select
          end if        ! iob <= sp_ob%objs
        end do          ! itot = 1, mobj_out
        exit
      end do        
      
      return

      end subroutine obj_output