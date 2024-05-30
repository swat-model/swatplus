      subroutine read_mgtops(isched)
      
      use maximum_data_module
      use plant_data_module
      use mgt_operations_module
      use tillage_data_module
      use fertilizer_data_module
      use pesticide_data_module
      use time_module
      
      implicit none      
      
      integer :: iyear       !            |
      integer :: day         !            |
      integer :: mon         !            |
      integer :: iop         !none        |
      integer :: isched      !            |
      integer :: jdt         !none        |julian date
      integer :: idb         !none        | 

      iyear = 1
      do iop = 1, sched(isched)%num_ops                             !! operation loop
        read (107,*)   sched(isched)%mgt_ops(iop)%op,               & !! operation character
                       sched(isched)%mgt_ops(iop)%mon,              &
                       sched(isched)%mgt_ops(iop)%day,              &
                       sched(isched)%mgt_ops(iop)%husc,             &
                       sched(isched)%mgt_ops(iop)%op_char,          & !! operation type character
                       sched(isched)%mgt_ops(iop)%op_plant,         & !! plant character 
                       sched(isched)%mgt_ops(iop)%op3               !! override
        
          day = sched(isched)%mgt_ops(iop)%day
          mon = sched(isched)%mgt_ops(iop)%mon
          sched(isched)%mgt_ops(iop)%jday = Jdt(ndays,day,mon)
          sched(isched)%mgt_ops(iop)%year = iyear
          if (sched(isched)%mgt_ops(iop)%op == "skip") iyear = iyear + 1
 
      select case(sched(isched)%mgt_ops(iop)%op)
          
        case ("pcom")
          do idb = 1, db_mx%plantcom
              if (sched(isched)%mgt_ops(iop)%op_char == pcomdb(idb)%name) then
                  sched(isched)%mgt_ops(iop)%op1 = idb
                  exit
              endif
          end do
          
        case ("plnt")
          do idb = 1, db_mx%plantparm
              if (sched(isched)%mgt_ops(iop)%op_char == pldb(idb)%plantnm) then
                  sched(isched)%mgt_ops(iop)%op1 = idb 
                  exit
              endif
          end do
          if (sched(isched)%mgt_ops(iop)%op1 == 0) then
            write (9001,*) " mgt schedule", isched, &
                " op numb", iop, sched(isched)%mgt_ops(iop)%op_char, " not found in plants.plt database" 
          end if
          !! xwalk with transplant data file
          do idb = 1, db_mx%transplant
              if (sched(isched)%mgt_ops(iop)%op_plant == transpl(idb)%name) then
                  sched(isched)%mgt_ops(iop)%op4 = idb 
                  exit
              endif
          end do
         
        case ("harv")
          do idb = 1, db_mx%harvop_db
              if (sched(isched)%mgt_ops(iop)%op_plant == harvop_db(idb)%name) then
                  sched(isched)%mgt_ops(iop)%op1 = idb
                  exit
              endif
          end do

        case ("hvkl")
          do idb = 1, db_mx%harvop_db
              if (sched(isched)%mgt_ops(iop)%op_plant == harvop_db(idb)%name) then
                  sched(isched)%mgt_ops(iop)%op1 = idb
                  exit
              endif
          end do
          
        case ("till")
          do idb = 1, db_mx%tillparm
              if (sched(isched)%mgt_ops(iop)%op_char == tilldb(idb)%tillnm) then
                  sched(isched)%mgt_ops(iop)%op1 = idb
                  exit
              endif
          end do
          
        case ("irrm","irrp")
          sched(isched)%irr = 1
          do idb = 1, db_mx%irrop_db
            if (sched(isched)%mgt_ops(iop)%op_char == irrop_db(idb)%name) then
              sched(isched)%mgt_ops(iop)%op1 = idb
              exit
            endif
          end do

        case ("fert")
          !xwalk fert name with fertilizer data base
          do idb = 1, db_mx%fertparm
            if (sched(isched)%mgt_ops(iop)%op_char == fertdb(idb)%fertnm) then
              sched(isched)%mgt_ops(iop)%op1 = idb
              exit
            endif
          end do
          !xwalk application type with chemical application data base
          do idb = 1, db_mx%chemapp_db
            if (sched(isched)%mgt_ops(iop)%op_plant == chemapp_db(idb)%name) then
              sched(isched)%mgt_ops(iop)%op4 = idb
              exit
            endif
          end do

        case ("pest")
          !xwalk fert name with fertilizer data base
          do idb = 1, db_mx%pestparm
            if (sched(isched)%mgt_ops(iop)%op_char == pestdb(idb)%name) then
              sched(isched)%mgt_ops(iop)%op1 = idb
              exit
            endif
          end do
          !xwalk application type with chemical application data base
          do idb = 1, db_mx%chemapp_db
            if (sched(isched)%mgt_ops(iop)%op_plant == chemapp_db(idb)%name) then
              sched(isched)%mgt_ops(iop)%op4 = idb
              exit
            endif
          end do

          case ("graz")
            do idb = 1, db_mx%grazeop_db
              if (sched(isched)%mgt_ops(iop)%op_char == grazeop_db(idb)%name) then
                  sched(isched)%mgt_ops(iop)%op1 = idb
                  exit
              endif
            end do
 
          case ("burn")
            do idb = 1, db_mx%fireop_db
              if (sched(isched)%mgt_ops(iop)%op_char == fire_db(idb)%name) then
                  sched(isched)%mgt_ops(iop)%op1 = idb
                  exit
              endif
            end do
          
          case ("swep")
            do idb = 1, db_mx%sweepop_db
              if (sched(isched)%mgt_ops(iop)%op_char == sweepop_db(idb)%name) then
                  sched(isched)%mgt_ops(iop)%op1 = idb
                  exit
              endif
!!herehere no null
            end do      
       end select          
      end do                                  !! operation loop
    
      return
      end