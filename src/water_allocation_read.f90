      subroutine water_allocation_read
      
      use water_allocation_module
      use maximum_data_module
      use hydrograph_module
      use input_file_module
      use conditional_module
      use reservoir_module
      use sd_channel_module
      use hru_module
      use aquifer_module
      
      implicit none 
      
      character (len=80) :: titldum = ""!           |title of file
      character (len=80) :: header = "" !           |header of file
      character (len=5) :: pp = "" !                |POD or POR
      integer :: eof = 0              !           |end of file
      integer :: imax = 0             !none       |determine max number for array (imax) and total number in file
      logical :: i_exist              !none       |check to determine if file exists
      
      integer :: i = 0                !none       |POU loop counter
      integer :: ip = 0                !none       |POU loop counter
      integer :: ipou = 0             !none       |POU number
      integer :: ipod = 0             !none       |POD number
      integer :: ipor = 0             !none       |POR number
      integer :: ipous = 0            !none       |number of POUs
      integer :: ipods = 0            !none       |number of PODs for each POU
      integer :: ipors = 0            !none       |number of PORs for each POU
      integer :: idb = 0              !none       |decision table data file number
      integer :: irrhru = 0           !none       |number of irrigation hrus for each POU
      
      eof = 0
      imax = 0
      
      !! read water allocation POU inputs
      inquire (file=in_wallo%pou, exist=i_exist)
      if (.not. i_exist .or. in_wallo%pou /= "null") then
      do 
        open (107,file=in_wallo%pou)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) imax
        db_mx%wallo_pou = imax
        if (eof < 0) exit
        
        allocate (pou(imax))           !! place of use (pou)
        allocate (poud_met(imax))      !! daily duty and delivery
        allocate (poum_met(imax))      !! monthly duty and delivery
        allocate (pouy_met(imax))      !! yearly duty and delivery
        allocate (poua_met(imax))      !! average annual duty and delivery
        allocate (poud_om(imax))       !! daily hydrographs
        allocate (poum_om(imax))       !! monthly hydrographs
        allocate (pouy_om(imax))       !! yearly hydrographs
        allocate (poua_om(imax))       !! ave annual hydrographs
        !! add constituent types for each pou if needed

        read (107,*,iostat=eof) header
        
        !! read POU data
        do i = 1, imax
          
          read (107,*,iostat=eof) ipou
          if (eof < 0) exit
          backspace (107)
          
          read (107,*,iostat=eof) ip, pou(ipou)%name, pou(ipou)%typ, pou(ipou)%typ_num,  &
              pou(ipou)%pods, pou(ipou)%pors, pou(ipou)%dtbl_mx, pou(ipou)%rate_max,     &
              pou(ipou)%dtbl_pod_fr, pou(ipou)%dtbl_por_fr
          
          ipods = pou(ipou)%pods
          ipors = pou(ipou)%pors
          allocate (pou(ipou)%pod(ipods))
          if (ipors > 0) then
            allocate (pou(ipou)%por(ipors))
            allocate (poud_om(ipou)%por(ipors))       !! daily hydrographs
            allocate (poum_om(ipou)%por(ipors))       !! monthly hydrographs
            allocate (pouy_om(ipou)%por(ipors))       !! yearly hydrographs
            allocate (poua_om(ipou)%por(ipors))       !! ave annual hydrographs
          end if
          allocate (poud_met(ipou)%pod(ipods))
          allocate (poum_met(ipou)%pod(ipods))
          allocate (pouy_met(ipou)%pod(ipods))
          allocate (poua_met(ipou)%pod(ipods))
          allocate (poud_om(ipou)%pod(ipods))       !! daily hydrographs
          allocate (poum_om(ipou)%pod(ipods))       !! monthly hydrographs
          allocate (pouy_om(ipou)%pod(ipods))       !! yearly hydrographs
          allocate (poua_om(ipou)%pod(ipods))       !! ave annual hydrographs
          !! add constituent types for each pou if needed
          
          !! read all POD input data
          do ipod = 1, ipods
            read (107,*,iostat=eof) pp, pou(ipou)%pod(ipod)%num, pou(ipou)%pod(ipod)%name, pou(ipou)%pod(ipod)%typ, &
                pou(ipou)%pod(ipod)%num, pou(ipou)%pod(ipod)%conv_typ, pou(ipou)%pod(ipod)%conv_num,            &
                pou(ipou)%pod(ipod)%dtbl_min, pou(ipou)%pod(ipod)%const_min, pou(ipou)%pod(ipod)%dtbl_wdraw,    &
                pou(ipou)%pod(ipod)%ann_max, pou(ipou)%pod(ipod)%frac, pou(ipou)%pod(ipod)%comp
            if (eof < 0) exit
          end do
          
          !! read all POR input data
          do ipor = 1, ipors
            read (107,*,iostat=eof) pp,pou(ipou)%por(ipor)%num, pou(ipou)%por(ipor)%name, pou(ipou)%por(ipor)%typ, &
                pou(ipou)%por(ipor)%num, pou(ipou)%por(ipor)%conv_typ, pou(ipou)%por(ipor)%conv_num,            &
                pou(ipou)%por(ipor)%dtbl_max, pou(ipou)%por(ipor)%const_max, pou(ipou)%por(ipor)%ann_max,       &
                pou(ipou)%por(ipor)%frac
          end do
          
          !! decision table for setting POU duty - max demand
          if (pou(ipou)%dtbl_mx /= "null") then
            !! xwalk with con decision table
            do idb = 1, db_mx%dtbl_flo
              if (pou(ipou)%dtbl_mx == dtbl_flo(idb)%name) then
                pou(ipou)%dtbl_mx_num = idb
                exit
              end if
            end do
          end if
          
          !! crosswalk with lum decision table for all hru in the irrigation district
          if (pou(ipou)%typ == "irr") then
            irrhru = hruirr_db(pou(ipou)%typ_num)%hrus
            pou(ipou)%irr%hru_num = irrhru
            allocate (pou(ipou)%irr%hru(irrhru))
            allocate (pou(ipou)%irr%dtbl_lum(irrhru))
            allocate (pou(ipou)%irr%dtbl_num(irrhru))
            do ihru = 1, irrhru
                pou(ipou)%irr%hru(ihru) = hruirr_db(pou(ipou)%typ_num)%hru_num(ihru)
                pou(ipou)%irr%dtbl_lum(ihru) = hruirr_db(pou(ipou)%typ_num)%dtbl_lum(ihru)
              do idb = 1, db_mx%dtbl_lum
                if (pou(ipou)%irr%dtbl_lum(ihru) == dtbl_lum(idb)%name) then
                  pou(ipou)%irr%dtbl_num(ihru) = idb
                  exit
                end if
              end do
            end do
          end if
            
          !! decision table for setting POD fractions
          if (pou(ipou)%dtbl_pod_fr /= "null") then
            !! xwalk with con decision table
            do idb = 1, db_mx%dtbl_flo
              if (pou(ipou)%dtbl_pod_fr == dtbl_flo(idb)%name) then
                pou(ipou)%dtbl_pod_fr_num = idb
                exit
              end if
            end do
          end if
            
          !! decision table for setting POR fractions
          if (pou(ipou)%dtbl_por_fr /= "null") then
            !! xwalk with con decision table
            do idb = 1, db_mx%dtbl_flo
              if (pou(ipou)%dtbl_por_fr == dtbl_flo(idb)%name) then
                pou(ipou)%dtbl_por_fr_num = idb
                exit
              end if
            end do
          end if
            
          !! set initial maximum withdrawal
          do ipod = 1, ipods
            pou(ipou)%pod(ipod)%wdraw_max = pou(ipou)%pod(ipod)%ann_max
            pou(ipou)%pod(ipod)%wdraw_cur = 0.
          end do
    
          !! decision table for setting max withdrawal for each POD
          do ipod = 1, ipods
            if (pou(ipou)%pod(ipod)%dtbl_wdraw /= "null") then
              !! xwalk with con decision table
              do idb = 1, db_mx%dtbl_flo
                if (pou(ipou)%pod(ipod)%dtbl_wdraw == dtbl_flo(idb)%name) then
                  pou(ipou)%pod(ipod)%dtbl_wdraw_num = idb
                  exit
                end if
              end do
            end if
          end do
            
        end do    !ipou = 1, imax
        
        exit
      end do
      end if
      close(107)
      
      
      eof = 0
      imax = 0
      
      !! read water allocation POD inputs
      inquire (file=in_wallo%pod, exist=i_exist)
      if (.not. i_exist .or. in_wallo%pod /= "null") then
      do 
        open (107,file=in_wallo%pod)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) imax
        db_mx%wallo_pod = imax
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        
        allocate (pod(imax))           !! place of use (pod)
        allocate (podd_om(imax))       !! daily hydrographs
        allocate (podm_om(imax))       !! monthly hydrographs
        allocate (pody_om(imax))       !! yearly hydrographs
        allocate (poda_om(imax))       !! ave annual hydrographs

        do ipod = 1, imax
          read (107,*,iostat=eof) pod(ipod)%num, pod(ipod)%name, pod(ipod)%typ, pod(ipod)%typ_num, ipous
          
          allocate (pod(ipod)%pou(ipous))
          allocate (podd_om(ipod)%pou(ipous))       !! daily hydrographs
          allocate (podm_om(ipod)%pou(ipous))       !! monthly hydrographs
          allocate (pody_om(ipod)%pou(ipous))       !! yearly hydrographs
          allocate (poda_om(ipod)%pou(ipous))       !! ave annual hydrographs
          !! add constituent types for each pou if needed
          
          backspace (107)
          read (107,*,iostat=eof) pod(ipod)%num, pod(ipod)%name, pod(ipod)%typ, pod(ipod)%typ_num,           &
            pod(ipod)%pous, (pod(ipod)%pou(ipou)%num, pod(ipod)%pou(ipou)%name, pod(ipod)%pou(ipou)%pod_num, &
            pod(ipod)%pou(ipou)%typ, pod(ipod)%pou(ipou)%typ_num, pod(ipod)%pou(ipou)%right, ipou = 1, ipous)
          
          !! store the POD number for each POU object for use in water allocation calculations
          select case (pod(ipod)%typ)
          case ("osrc")
            osrc(pod(ipod)%typ_num)%wallo_pod = pod(ipod)%num
            
          case ("res")
            res_ob(pod(ipod)%typ_num)%wallo_pod = pod(ipod)%num
            
          case ("cha")
            sd_ch(pod(ipod)%typ_num)%wallo_pod = pod(ipod)%num
            
          case ("hru")
            hru(pod(ipod)%typ_num)%wallo_pod = pod(ipod)%num
            
          case ("aqu")
            aqu_prm(pod(ipod)%typ_num)%wallo_pod = pod(ipod)%num
            
          case ("can")
            canal(pod(ipod)%typ_num)%wallo_pod = pod(ipod)%num
            
          case ("wtow")
            wtow(pod(ipod)%typ_num)%wallo_pod = pod(ipod)%num
          end select
          
        end do    !ipod = 1, imax
        
        exit
      end do
      end if
      close(107)

    return
    end subroutine water_allocation_read