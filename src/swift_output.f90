      subroutine swift_output
    
      use hydrograph_module
      use hru_module
      use soil_module
      use output_landscape_module
      use reservoir_data_module
      use maximum_data_module
      use climate_module
      use aquifer_module
      use input_file_module
      use sd_channel_module
      use time_module

      implicit none
      
      integer :: iaqu = 0
      integer :: icha = 0
      integer :: ires = 0
      integer :: ihyd = 0
      integer :: idat = 0
      integer :: idb = 0
      integer :: iobj_out = 0
      integer :: irec = 0
      integer :: iob = 0
      character (len=8) :: wet_y_n = ""
      character(len=100) :: folderPath = ""
      character(len=100) :: command = ""
      logical :: i_exist
      
      201 format (5xA8,A5,46X,*(A12F2.0A4,1xA12F2.0A4))     ! format of precip.swf headers
      301 format (I,1xA,F,6xF)                              ! format of precip.swf
      202 format (5xA8,A5,9X,A16,27xA8,7xA,12x1A4,3x1A4)    ! format of hru_dat.swf headers
      302 format (1I,1x2A, G ,1x*(G))                       ! format of hru_dat.swf
      203 format (A8,*(2x1A,10x1A, 6x1A, 2x6A))             ! format of hru_exco.swf headers      
      303 format (I8,*(2x1A,F16.4, 7F16.4,10x))             ! format of hru_exco.swf 
      204 format (5xA8,1x*(A5,10x))    ! format of hru_dat.swf headers
      !304 format (*)                       ! format of hru_dat.swf
      
      !! check for file_cio.swf to determine if SWIFT folder exist
      inquire (file="SWIFT/file_cio.swf", exist=i_exist)
      if (.not. i_exist) then   ! if not use system-specific command to create SWIFT folder
        folderPath = "SWIFT"
        command = 'mkdir ' // trim(folderPath)
        call SYSTEM(command)
      end if
    
      !! write new file.cio
      open (107,file="SWIFT/file_cio.swf",recl = 1500)
      write (107, *) "SWIFT file.cio"
      write (107, *) "BASIN         ", in_sim%object_cnt, in_sim%object_prt, in_sim%cs_db
      write (107, *) "CLIMATE       ", "  precip.swf"
      write (107, *) "CONNECT       ", in_con%hru_con, in_con%ru_con, in_con%aqu_con, in_con%chandeg_con,  &
                                          in_con%res_con, in_con%rec_con, in_con%out_con 
      write (107, *) "CHANNEL       ", "  chan_dat.swf", "  chan_dr.swf"
      write (107, *) "RESERVOIR     ", "  res_dat.swf", "  res_dr.swf"
      write (107, *) "ROUT_UNIT     ", in_ru%ru_def, in_ru%ru_ele
      write (107, *) "HRU           ", "  hru_dat.swf", "  hru_exco.swf", "  hru_wet.swf",    &
                                       "  hru_bmp.swf", "  hru_dr.swf"
      write (107, *) "RECALL        ", "  recall.swf"
      write (107, *) "AQUIFER       ", "  aqu_dr.swf"
      write (107, *) "LS_UNIT       ", in_regs%def_lsu, in_regs%ele_lsu
      close (107)
      
      !! Call the copy_file function to copy SWAT+ Inputs to SWIFT Folder
      call copy_file(in_sim%object_cnt, "SWIFT/" // trim(adjustl(in_sim%object_cnt)))
      call copy_file(in_sim%object_prt, "SWIFT/" // trim(adjustl(in_sim%object_prt)))
      call copy_file(in_sim%cs_db, "SWIFT/" // trim(adjustl(in_sim%cs_db)))
      call copy_file(in_con%hru_con, "SWIFT/" // trim(adjustl(in_con%hru_con)))
      call copy_file(in_con%ru_con, "SWIFT/" // trim(adjustl(in_con%ru_con)))
      call copy_file(in_con%aqu_con, "SWIFT/" // trim(adjustl(in_con%aqu_con)))
      call copy_file(in_con%chandeg_con, "SWIFT/" // trim(adjustl(in_con%chandeg_con)))
      call copy_file(in_con%res_con, "SWIFT/" // trim(adjustl(in_con%res_con)))
      call copy_file(in_con%rec_con, "SWIFT/" // trim(adjustl(in_con%rec_con)))
      call copy_file(in_con%out_con, "SWIFT/" // trim(adjustl(in_con%out_con)))
      call copy_file(in_ru%ru_def, "SWIFT/" // trim(adjustl(in_ru%ru_def)))
      call copy_file(in_ru%ru_ele, "SWIFT/" // trim(adjustl(in_ru%ru_ele)))
      !call copy_file(in_rec%recall_rec, "SWIFT/" // trim(adjustl(in_rec%recall_rec)))
      call copy_file(in_regs%def_lsu, "SWIFT/" // trim(adjustl(in_regs%def_lsu)))
      call copy_file(in_regs%ele_lsu, "SWIFT/" // trim(adjustl(in_regs%ele_lsu)))

      
      !! write ave annual precip to SWIFT model
      open (107,file="SWIFT/precip.swf",recl = 1500)
      write (107, *) bsn%name
      write (107, *) db_mx%wst
      write (107, 201) "iwst ", "name ", "precip_aa/", yrs_print,'yrs', "pet_aa/", yrs_print, 'yrs'
      write (107, '(5xA8,A5,34x,A16,6xA16)') "--- ", "---- ", "mm", "mm"
      do iwst = 1, db_mx%wst
        wst(iwst)%precip_aa = wst(iwst)%precip_aa / yrs_print
        wst(iwst)%pet_aa = wst(iwst)%pet_aa / yrs_print
        write (107, 301) iwst, wst(iwst)%name, wst(iwst)%precip_aa, wst(iwst)%pet_aa
      end do
      close (107)
      
      !! write hru data to SWIFT model
      open (107,file="SWIFT/hru_dat.swf",recl = 1500)
      write (107, *) bsn%name
      write (107, *) sp_ob%hru
      write (107, 202) "iwst ", "name ", "land_use_mgt_c", "slope", "hydgrp", "null", "null"
      write (107, 202) "--- ", "---- ", "--------------", "m/m", "------", "null", "null"
      do ihru = 1, sp_ob%hru
        write (107, 302) ihru, ob(ihru)%name, hru(ihru)%land_use_mgt_c, hru(ihru)%topo%slope,    &
                                                    soil(ihru)%hydgrp, "  null", "   null"
      end do
      close (107)
      
      !! write hru export coefficients to SWIFT model
      open (107,file="SWIFT/hru_exco.swf",recl = 1500)
      write (107, *) bsn%name
      write (107, *) sp_ob%hru
      write (107, 203) "HRU ", (hru_exco_hdr%hd_type(ihyd), 'wyld_rto', &
          hru_exco_hdr%hyd_type, ihyd = 1, hd_tot%hru)

      write (107, 203) "--- ", (hru_exco_hdr%hd_type(ihyd), 'wyld_rto', &
          hru_exco_hdr%hyd_unit, ihyd = 1, hd_tot%hru)
      
      do ihru = 1, sp_ob%hru
        icmd = hru(ihru)%obj_no
        !! Allocate wyld_rto array based on the number of hydrological components
        allocate(wyld_rto(hd_tot%hru))
        
        do ihyd = 1, hd_tot%hru
            !! convert mass to concentrations
            if (ob(icmd)%hd_aa(ihyd)%flo > 1.e-6) then
                call hyd_convert_mass_to_conc(ob(icmd)%hd_aa(ihyd))
            else
                ob(icmd)%hd_aa(ihyd) = hz
            end if
            !! output runoff/precip ratio - mm=m3/(10*ha)
            wyld_rto(ihyd) = hru(ihru)%flow(ihyd) / (hru(ihru)%precip_aa + 1.e-6)
        end do
        
        !! write to SWIFT hru export coefficient file
        write(107, 303) ihru, (hru_exco_hdr%hd_type(ihyd), &
            wyld_rto(ihyd), ob(icmd)%hd_aa(ihyd)%sed, ob(icmd)%hd_aa(ihyd)%orgn, &
            ob(icmd)%hd_aa(ihyd)%sedp, ob(icmd)%hd_aa(ihyd)%no3, ob(icmd)%hd_aa(ihyd)%solp, &
            ob(icmd)%hd_aa(ihyd)%nh3, ob(icmd)%hd_aa(ihyd)%no2, ihyd = 1, hd_tot%hru)
        
        !! Deallocate the wyld_rto array
        deallocate(wyld_rto)
      end do

      close(107)
      
      !! write hru wetland inputs to SWIFT model
      open (107,file="SWIFT/hru_wet.swf",recl = 1500)
      write (107, *) bsn%name
      write (107, *) sp_ob%hru
      write (107, 204) "ires", "psa ", "pdep", "esa ", "edep"
      write (107, 204) "----", "frac", "mm  ", "frac", "mm  "
      do ihru = 1, sp_ob%hru
        icmd = hru(ihru)%obj_no
        
        !! write to SWIFT wetland input file
        if (hru(ihru)%dbs%surf_stor > 0) then
          !! wetland hru
          ires= hru(ihru)%dbs%surf_stor
          ihyd = wet_dat(ires)%hyd
          write (107, *) ihru, wet_hyd(ihyd)%psa, wet_hyd(ihyd)%pdep, wet_hyd(ihyd)%esa,    &
                                                                        wet_hyd(ihyd)%edep
        end if
      end do
      close (107)
      
      !! write channel data for SWIFT
      open (107,file="SWIFT/chan_dat.swf",recl = 1500)
      write (107, *) bsn%name
      write (107, *) " OUTPUT NAMES - NUBZ"
      do icha = 1, sp_ob%chandeg
        icmd = sp_ob1%chandeg + icha - 1
        idat = ob(icmd)%props
        idb = sd_dat(idat)%hyd
        write (107, *) icha, sd_chd(idb)
      end do
      close (107)
      
      !! write channel delivery ratios for SWIFT
      open (107,file="SWIFT/chan_dr.swf",recl = 1500)
      write (107, *) bsn%name
      write (107, *) sp_ob%chandeg
      write (107, *) " OUTPUT NAMES - NUBZ"
      write (107, *) " OUTPUT UNITS - NUBZ"
      do icha = 1, sp_ob%chandeg
        icmd = sp_ob1%chandeg + icha - 1
        ht5 = ob(icmd)%hout_tot // ob(icmd)%hin_tot
        ht5%flo = 1.    !sediment and organic transport are simulated in SWIFT
        ht5%sed = 1.
        ht5%orgn = 1.
        ht5%sedp = 1.
        ht5%nh3 = 1. 
        ht5%no2 = 1.
        write (107, *) icha, sd_chd(idb)%name, ht5%flo, ht5%sed, ht5%orgn, ht5%sedp, ht5%no3, ht5%solp, ht5%nh3, ht5%no2
      end do
      close (107)
           
      !! write aquifer delivery ratios for SWIFT
      open (107,file="SWIFT/aqu_dr.swf",recl = 1500)
      write (107, *) bsn%name
      write (107, *) sp_ob%aqu
      write (107, *) " OUTPUT NAMES - NUBZ"
      write (107, *) " OUTPUT UNITS - NUBZ"
      do iaqu = 1, sp_ob%aqu
        icmd = sp_ob1%aqu + iaqu - 1
        ht5 = ob(icmd)%hout_tot // ob(icmd)%hin_tot
        write (107, *) iaqu, ht5%flo, ht5%sed, ht5%orgn, ht5%sedp, ht5%no3, ht5%solp, ht5%nh3, ht5%no2
      end do
      close (107)
            
      !! write reservoir delivery ratios for SWIFT
      open (107,file="SWIFT/res_dat.swf",recl = 1500)
      write (107, *) bsn%name
      write (107, *) sp_ob%res
      write (107, *) " OUTPUT NAMES - NUBZ"
      write (107, *) " OUTPUT UNITS - NUBZ"
      do ires = 1, sp_ob%res
        write (107, *) ires, res_hyd(ires)%name, res_hyd(ires)%psa, res_hyd(ires)%pvol, res_hyd(ires)%esa,    &
                                                                      res_hyd(ires)%evol
      end do
      close (107)
      
      !! write reservoir delivery ratios for SWIFT
      open (107,file="SWIFT/res_dr.swf",recl = 1500)
      write (107, *) bsn%name
      write (107, *) sp_ob%res
      write (107, *) " OUTPUT NAMES - NUBZ"
      write (107, *) " OUTPUT UNITS - NUBZ"
      do ires = 1, sp_ob%res
        icmd = sp_ob1%res + ires - 1
        ht5 = ob(icmd)%hout_tot // ob(icmd)%hin_tot
        write (107, *) ires, ob(icmd)%name, ht5%flo, ht5%sed, ht5%orgn, ht5%sedp, ht5%no3, ht5%solp, ht5%nh3, ht5%no2
      end do
      close (107)
      
      !! write recal_swift.rec --> change files to average annual and use the object name for the file name
      open (107,file="SWIFT/recall.swf",recl = 1500)
      write (107,*)           "         ID            NAME              REC_TYP         FILENAME"
      do irec = 1, db_mx%recall_max
        write (107,*) irec, recall(irec)%name, recall(irec)%typ, recall(irec)%name
        
        !! write to each recall file
        open (108,file="SWIFT/" // trim(adjustl(recall(irec)%name)),recl = 1500)
        write (108,*) " AVE ANNUAL RECALL FILE  ", recall(irec)%filename
        write (108,*) "     1    1    1     1    type    ", recall(irec)%filename, rec_a(irec)%flo,     &
                rec_a(irec)%sed, rec_a(irec)%orgn, rec_a(irec)%sedp, rec_a(irec)%no3, rec_a(irec)%solp, &
                rec_a(irec)%nh3, rec_a(irec)%no2
        close (108)
      end do
      close (107)
      
      !! write object.prt file - using the same file for now
      !open (107,file="object_prt.swf",recl = 1500)
      do iobj_out = 1, mobj_out
        !write (107,*) irec, recall(irec)%name, "   4   ", recall(irec)%name
        
        !! write to each object print file
        open (108,file="SWIFT/object_prt.swf",recl = 1500)
        write (108,*) " AVE ANNUAL OBJECT OUTPUT FILE  ", ob_out(iobj_out)%filename
        iob = ob_out(iobj_out)%objno
        ihyd = ob_out(iobj_out)%hydno
        ob(iob)%hd_aa(ihyd) = ob(iob)%hd_aa(ihyd) / yrs_print
        write (108,*) "     1    1    1     1    ", ob_out(iobj_out)%name, ob_out(iobj_out)%name,       &
                        ob(iob)%hd_aa(ihyd)%flo, ob(iob)%hd_aa(ihyd)%sed, ob(iob)%hd_aa(ihyd)%orgn,     &
                        ob(iob)%hd_aa(ihyd)%sedp, ob(iob)%hd_aa(ihyd)%no3, ob(iob)%hd_aa(ihyd)%solp,    &
                        ob(iob)%hd_aa(ihyd)%nh3, ob(iob)%hd_aa(ihyd)%no2
        close (108)
      end do
      close (107)
      
            
      return
      end subroutine swift_output