      subroutine basin_print_codes_read
      
      use input_file_module
      use basin_module
      use time_module
      
      implicit none
       
      character (len=500) :: header = "" !              |header of file
      character (len=80) :: titldum = "" !              |title of file
      character (len=16) :: name = ""  !              |name
      integer :: eof = 0               !              |end of file
      logical :: i_exist               !              |check to determine if file exists
      integer :: ii = 0                !none          |counter
      integer :: result
       
      eof = 0

      !! read time codes
      inquire (file=in_sim%prt, exist=i_exist)
      if (i_exist .or. in_sim%prt /= "null") then
      do
        open (107,file=in_sim%prt)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        read (107,*,iostat=eof) pco%nyskip, pco%day_start, pco%yrc_start, pco%day_end, pco%yrc_end, pco%int_day        
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        read (107,*,iostat=eof) pco%aa_numint
        if (pco%aa_numint > 0) then
          allocate (pco%aa_yrs(pco%aa_numint), source = 0)
          backspace (107)
          read (107,*,iostat=eof) pco%aa_numint, (pco%aa_yrs(ii), ii = 1, pco%aa_numint)
          if (eof < 0) exit
        else
          allocate (pco%aa_yrs(1), source = 0)
        end if
     !! read database output
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        read (107,*,iostat=eof) pco%csvout, pco%use_obj_labels, pco%cdfout
        if (eof < 0) exit
        
     !! read other output
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        read (107,*,iostat=eof) pco%crop_yld, pco%mgtout, pco%hydcon, pco%fdcout
        if (eof < 0) exit
             
     !! read objects output
     !! basin
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        if (pco%use_obj_labels == "n") then
          read (107,*,iostat=eof) name, pco%wb_bsn%d, pco%wb_bsn%m, pco%wb_bsn%y, pco%wb_bsn%a
          if (eof < 0) exit
          read (107,*,iostat=eof) name, pco%nb_bsn%d, pco%nb_bsn%m, pco%nb_bsn%y, pco%nb_bsn%a
          if (eof < 0) exit       
          read (107,*,iostat=eof) name, pco%ls_bsn%d, pco%ls_bsn%m, pco%ls_bsn%y, pco%ls_bsn%a
          if (eof < 0) exit
          read (107,*,iostat=eof) name, pco%pw_bsn%d, pco%pw_bsn%m, pco%pw_bsn%y, pco%pw_bsn%a
          if (eof < 0) exit        
          read (107,*,iostat=eof) name, pco%aqu_bsn%d, pco%aqu_bsn%m, pco%aqu_bsn%y, pco%aqu_bsn%a
          if (eof < 0) exit            
          read (107,*,iostat=eof) name, pco%res_bsn%d, pco%res_bsn%m, pco%res_bsn%y, pco%res_bsn%a
          if (eof < 0) exit        
          read (107,*,iostat=eof) name, pco%chan_bsn%d, pco%chan_bsn%m, pco%chan_bsn%y, pco%chan_bsn%a
          if (eof < 0) exit            
          read (107,*,iostat=eof) name, pco%sd_chan_bsn%d, pco%sd_chan_bsn%m, pco%sd_chan_bsn%y, pco%sd_chan_bsn%a
          if (eof < 0) exit 
          read (107,*,iostat=eof) name, pco%recall_bsn%d, pco%recall_bsn%m, pco%recall_bsn%y, pco%recall_bsn%a
          if (eof < 0) exit            
        ! region
          read (107,*,iostat=eof) name, pco%wb_reg%d, pco%wb_reg%m, pco%wb_reg%y, pco%wb_reg%a
          if (eof < 0) exit     
          read (107,*,iostat=eof) name, pco%nb_reg%d, pco%nb_reg%m, pco%nb_reg%y, pco%nb_reg%a
          if (eof < 0) exit       
          read (107,*,iostat=eof) name, pco%ls_reg%d, pco%ls_reg%m, pco%ls_reg%y, pco%ls_reg%a
          if (eof < 0) exit
          read (107,*,iostat=eof) name, pco%pw_reg%d, pco%pw_reg%m, pco%pw_reg%y, pco%pw_reg%a
          if (eof < 0) exit        
          read (107,*,iostat=eof) name, pco%aqu_reg%d, pco%aqu_reg%m, pco%aqu_reg%y, pco%aqu_reg%a
          if (eof < 0) exit            
          read (107,*,iostat=eof) name, pco%res_reg%d, pco%res_reg%m, pco%res_reg%y, pco%res_reg%a
          if (eof < 0) exit                           
          read (107,*,iostat=eof) name, pco%sd_chan_reg%d, pco%sd_chan_reg%m, pco%sd_chan_reg%y, pco%sd_chan_reg%a
          if (eof < 0) exit 
          read (107,*,iostat=eof) name, pco%recall_reg%d, pco%recall_reg%m, pco%recall_reg%y, pco%recall_reg%a
          if (eof < 0) exit 
          read (107,*,iostat=eof) name, pco%water_allo%d, pco%water_allo%m, pco%water_allo%y, pco%water_allo%a
          if (eof < 0) exit   
        ! lsu
          read (107,*,iostat=eof) name, pco%wb_lsu%d, pco%wb_lsu%m, pco%wb_lsu%y, pco%wb_lsu%a
          if (eof < 0) exit     
          read (107,*,iostat=eof) name, pco%nb_lsu%d, pco%nb_lsu%m, pco%nb_lsu%y, pco%nb_lsu%a
          if (eof < 0) exit       
          read (107,*,iostat=eof) name, pco%ls_lsu%d, pco%ls_lsu%m, pco%ls_lsu%y, pco%ls_lsu%a
          if (eof < 0) exit
          read (107,*,iostat=eof) name, pco%pw_lsu%d, pco%pw_lsu%m, pco%pw_lsu%y, pco%pw_lsu%a
          if (eof < 0) exit                
        ! hru
          read (107,*,iostat=eof) name, pco%wb_hru%d, pco%wb_hru%m, pco%wb_hru%y, pco%wb_hru%a
          if (eof < 0) exit     
          read (107,*,iostat=eof) name, pco%nb_hru%d, pco%nb_hru%m, pco%nb_hru%y, pco%nb_hru%a
          if (eof < 0) exit       
          read (107,*,iostat=eof) name, pco%ls_hru%d, pco%ls_hru%m, pco%ls_hru%y, pco%ls_hru%a
          if (eof < 0) exit
          read (107,*,iostat=eof) name, pco%pw_hru%d, pco%pw_hru%m, pco%pw_hru%y, pco%pw_hru%a
          if (eof < 0) exit 
          
        ! hru-lte
          read (107,*,iostat=eof) name, pco%wb_sd%d, pco%wb_sd%m, pco%wb_sd%y, pco%wb_sd%a
          if (eof < 0) exit     
          read (107,*,iostat=eof) name, pco%nb_sd%d, pco%nb_sd%m, pco%nb_sd%y, pco%nb_sd%a
          if (eof < 0) exit       
          read (107,*,iostat=eof) name, pco%ls_sd%d, pco%ls_sd%m, pco%ls_sd%y, pco%ls_sd%a
          if (eof < 0) exit
          read (107,*,iostat=eof) name, pco%pw_sd%d, pco%pw_sd%m, pco%pw_sd%y, pco%pw_sd%a
          if (eof < 0) exit                   
        ! channel
          read (107,*,iostat=eof) name, pco%chan%d, pco%chan%m, pco%chan%y, pco%chan%a
          if (eof < 0) exit             
        ! channel-lte
          read (107,*,iostat=eof) name, pco%sd_chan%d, pco%sd_chan%m, pco%sd_chan%y, pco%sd_chan%a
          if (eof < 0) exit          
        ! aquifer
          read (107,*,iostat=eof) name, pco%aqu%d, pco%aqu%m, pco%aqu%y, pco%aqu%a
          if (eof < 0) exit
        ! reservoir
          read (107,*,iostat=eof) name, pco%res%d, pco%res%m, pco%res%y, pco%res%a
          if (eof < 0) exit
        ! recall
          read (107,*,iostat=eof) name, pco%recall%d, pco%recall%m, pco%recall%y, pco%recall%a
          if (eof < 0) exit        
        ! hydin and hydout
          read (107,*,iostat=eof) name, pco%hyd%d, pco%hyd%m, pco%hyd%y, pco%hyd%a
          if (eof < 0) exit
        ! routing units
          read (107,*,iostat=eof) name, pco%ru%d, pco%ru%m, pco%ru%y, pco%ru%a
          if (eof < 0) exit 
        ! all pesticide outputs
          read (107,*,iostat=eof) name, pco%pest%d, pco%pest%m, pco%pest%y, pco%pest%a
          if (eof < 0) exit  
          !! salt outputs !rtb salt
          read (107,*,iostat=eof) name, pco%salt_basin%d, pco%salt_basin%m, pco%salt_basin%y, pco%salt_basin%a
          if (eof < 0) exit
          read (107,*,iostat=eof) name, pco%salt_hru%d, pco%salt_hru%m, pco%salt_hru%y, pco%salt_hru%a
          if (eof < 0) exit
          read (107,*,iostat=eof) name, pco%salt_ru%d, pco%salt_ru%m, pco%salt_ru%y, pco%salt_ru%a
          if (eof < 0) exit
          read (107,*,iostat=eof) name, pco%salt_aqu%d, pco%salt_aqu%m, pco%salt_aqu%y, pco%salt_aqu%a
          if (eof < 0) exit
          read (107,*,iostat=eof) name, pco%salt_chn%d, pco%salt_chn%m, pco%salt_chn%y, pco%salt_chn%a
          if (eof < 0) exit
          read (107,*,iostat=eof) name, pco%salt_res%d, pco%salt_res%m, pco%salt_res%y, pco%salt_res%a
          if (eof < 0) exit
          read (107,*,iostat=eof) name, pco%salt_wet%d, pco%salt_wet%m, pco%salt_wet%y, pco%salt_wet%a
          if (eof < 0) exit
          !! constituent outputs !rtb cs
          read (107,*,iostat=eof) name, pco%cs_basin%d, pco%cs_basin%m, pco%cs_basin%y, pco%cs_basin%a
          if (eof < 0) exit
          read (107,*,iostat=eof) name, pco%cs_hru%d, pco%cs_hru%m, pco%cs_hru%y, pco%cs_hru%a
          if (eof < 0) exit
          read (107,*,iostat=eof) name, pco%cs_ru%d, pco%cs_ru%m, pco%cs_ru%y, pco%cs_ru%a
          if (eof < 0) exit
          read (107,*,iostat=eof) name, pco%cs_aqu%d, pco%cs_aqu%m, pco%cs_aqu%y, pco%cs_aqu%a
          if (eof < 0) exit
          read (107,*,iostat=eof) name, pco%cs_chn%d, pco%cs_chn%m, pco%cs_chn%y, pco%cs_chn%a
          if (eof < 0) exit
          read (107,*,iostat=eof) name, pco%cs_res%d, pco%cs_res%m, pco%cs_res%y, pco%cs_res%a
          if (eof < 0) exit
          read (107,*,iostat=eof) name, pco%cs_wet%d, pco%cs_wet%m, pco%cs_wet%y, pco%cs_wet%a
          if (eof < 0) exit
        else
          do while (eof >= 0)
            read (107,*,iostat=eof) name
            if (eof < 0) exit
            if (name == "") cycle
            select case(name) 
               case("basin_wb")
                  if (pco%wb_bsn%already_read_in) then
                     result = print_prt_error(name)
                  else
                     backspace (107)
                     read (107,*,iostat=eof) name, pco%wb_bsn%d, pco%wb_bsn%m, pco%wb_bsn%y, pco%wb_bsn%a  
                     pco%wb_bsn%already_read_in = .true.
                  endif
               case("basin_nb")
                  if (pco%nb_bsn%already_read_in) then
                     result = print_prt_error(name)
                  else
                     backspace (107)
                     read (107,*,iostat=eof) name, pco%nb_bsn%d, pco%nb_bsn%m, pco%nb_bsn%y, pco%nb_bsn%a  
                     pco%nb_bsn%already_read_in = .true.
                  endif
               case("basin_ls")
                  if (pco%ls_bsn%already_read_in) then
                     result = print_prt_error(name)
                  else
                     backspace (107)
                     read (107,*,iostat=eof) name, pco%ls_bsn%d, pco%ls_bsn%m, pco%ls_bsn%y, pco%ls_bsn%a  
                     pco%ls_bsn%already_read_in = .true.
                  endif
               case("basin_pw")
                  if (pco%pw_bsn%already_read_in) then
                     result = print_prt_error(name)
                  else
                     backspace (107)
                     read (107,*,iostat=eof) name, pco%pw_bsn%d, pco%pw_bsn%m, pco%pw_bsn%y, pco%pw_bsn%a  
                     pco%pw_bsn%already_read_in = .true.
                  endif
               case("basin_aqu")
                  if (pco%aqu_bsn%already_read_in) then
                     result = print_prt_error(name)
                  else
                     backspace (107)
                     read (107,*,iostat=eof) name, pco%aqu_bsn%d, pco%aqu_bsn%m, pco%aqu_bsn%y, pco%aqu_bsn%a  
                     pco%aqu_bsn%already_read_in = .true.
                  endif
               case("basin_res")
                  if (pco%res_bsn%already_read_in) then
                     result = print_prt_error(name)
                  else
                     backspace (107)
                     read (107,*,iostat=eof) name, pco%res_bsn%d, pco%res_bsn%m, pco%res_bsn%y, pco%res_bsn%a  
                     pco%res_bsn%already_read_in = .true.
                  endif
               case("basin_cha")
                  if (pco%chan_bsn%already_read_in) then
                     result = print_prt_error(name)
                  else
                     backspace (107)
                     read (107,*,iostat=eof) name, pco%chan_bsn%d, pco%chan_bsn%m, pco%chan_bsn%y, pco%chan_bsn%a  
                     pco%chan_bsn%already_read_in = .true.
                  endif
               case("basin_sd_cha")
                  if (pco%sd_chan_bsn%already_read_in) then
                     result = print_prt_error(name)
                  else
                     backspace (107)
                     read (107,*,iostat=eof) name, pco%sd_chan_bsn%d, pco%sd_chan_bsn%m, pco%sd_chan_bsn%y, pco%sd_chan_bsn%a  
                     pco%sd_chan_bsn%already_read_in = .true.
                  endif
               case("basin_psc")
                  if (pco%recall_bsn%already_read_in) then
                     result = print_prt_error(name)
                  else
                     backspace (107)
                     read (107,*,iostat=eof) name, pco%recall_bsn%d, pco%recall_bsn%m, pco%recall_bsn%y, pco%recall_bsn%a  
                     pco%recall_bsn%already_read_in = .true.
                  endif
               case("region_wb")
                  if (pco%wb_reg%already_read_in) then
                     result = print_prt_error(name)
                  else
                     backspace (107)
                     read (107,*,iostat=eof) name, pco%wb_reg%d, pco%wb_reg%m, pco%wb_reg%y, pco%wb_reg%a  
                     pco%wb_reg%already_read_in = .true.
                  endif
               case("region_nb")
                  if (pco%nb_reg%already_read_in) then
                     result = print_prt_error(name)
                  else
                     backspace (107)
                     read (107,*,iostat=eof) name, pco%nb_reg%d, pco%nb_reg%m, pco%nb_reg%y, pco%nb_reg%a  
                     pco%nb_reg%already_read_in = .true.
                  endif
               case("region_ls")
                  if (pco%ls_reg%already_read_in) then
                     result = print_prt_error(name)
                  else
                     backspace (107)
                     read (107,*,iostat=eof) name, pco%ls_reg%d, pco%ls_reg%m, pco%ls_reg%y, pco%ls_reg%a  
                     pco%ls_reg%already_read_in = .true.
                  endif
               case("region_pw")
                  if (pco%pw_reg%already_read_in) then
                     result = print_prt_error(name)
                  else
                     backspace (107)
                     read (107,*,iostat=eof) name, pco%pw_reg%d, pco%pw_reg%m, pco%pw_reg%y, pco%pw_reg%a  
                     pco%pw_reg%already_read_in = .true.
                  endif
               case("region_aqu")
                  if (pco%aqu_reg%already_read_in) then
                     result = print_prt_error(name)
                  else
                     backspace (107)
                     read (107,*,iostat=eof) name, pco%aqu_reg%d, pco%aqu_reg%m, pco%aqu_reg%y, pco%aqu_reg%a  
                     pco%aqu_reg%already_read_in = .true.
                  endif
               case("region_res")
                  if (pco%res_reg%already_read_in) then
                     result = print_prt_error(name)
                  else
                     backspace (107)
                     read (107,*,iostat=eof) name, pco%res_reg%d, pco%res_reg%m, pco%res_reg%y, pco%res_reg%a  
                     pco%res_reg%already_read_in = .true.
                  endif
               case("region_sd_cha")
                  if (pco%sd_chan_reg%already_read_in) then
                     result = print_prt_error(name)
                  else
                     backspace (107)
                     read (107,*,iostat=eof) name, pco%sd_chan_reg%d, pco%sd_chan_reg%m, pco%sd_chan_reg%y, pco%sd_chan_reg%a  
                     pco%sd_chan_reg%already_read_in = .true.
                  endif
               case("region_psc")
                  if (pco%recall_reg%already_read_in) then
                     result = print_prt_error(name)
                  else
                     backspace (107)
                     read (107,*,iostat=eof) name, pco%recall_reg%d, pco%recall_reg%m, pco%recall_reg%y, pco%recall_reg%a  
                     pco%recall_reg%already_read_in = .true.
                  endif
               case("water_allo")
                  if (pco%water_allo%already_read_in) then
                     result = print_prt_error(name)
                  else
                     backspace (107)
                     read (107,*,iostat=eof) name, pco%water_allo%d, pco%water_allo%m, pco%water_allo%y, pco%water_allo%a  
                     pco%water_allo%already_read_in = .true.
                  endif
               case("lsunit_wb")
                  if (pco%wb_lsu%already_read_in) then
                     result = print_prt_error(name)
                  else
                     backspace (107)
                     read (107,*,iostat=eof) name, pco%wb_lsu%d, pco%wb_lsu%m, pco%wb_lsu%y, pco%wb_lsu%a  
                     pco%wb_lsu%already_read_in = .true.
                  endif
               case("lsunit_nb")
                  if (pco%nb_lsu%already_read_in) then
                     result = print_prt_error(name)
                  else
                     backspace (107)
                     read (107,*,iostat=eof) name, pco%nb_lsu%d, pco%nb_lsu%m, pco%nb_lsu%y, pco%nb_lsu%a  
                     pco%nb_lsu%already_read_in = .true.
                  endif
               case("lsunit_ls")
                  if (pco%ls_lsu%already_read_in) then
                     result = print_prt_error(name)
                  else
                     backspace (107)
                     read (107,*,iostat=eof) name, pco%ls_lsu%d, pco%ls_lsu%m, pco%ls_lsu%y, pco%ls_lsu%a  
                     pco%ls_lsu%already_read_in = .true.
                  endif
               case("lsunit_pw")
                  if (pco%pw_lsu%already_read_in) then
                     result = print_prt_error(name)
                  else
                     backspace (107)
                     read (107,*,iostat=eof) name, pco%pw_lsu%d, pco%pw_lsu%m, pco%pw_lsu%y, pco%pw_lsu%a  
                     pco%pw_lsu%already_read_in = .true.
                  endif
               case("hru_wb")
                  if (pco%wb_hru%already_read_in) then
                     result = print_prt_error(name)
                  else
                     backspace (107)
                     read (107,*,iostat=eof) name, pco%wb_hru%d, pco%wb_hru%m, pco%wb_hru%y, pco%wb_hru%a  
                     pco%wb_hru%already_read_in = .true.
                  endif
               case("hru_nb")
                  if (pco%nb_hru%already_read_in) then
                     result = print_prt_error(name)
                  else
                     backspace (107)
                     read (107,*,iostat=eof) name, pco%nb_hru%d, pco%nb_hru%m, pco%nb_hru%y, pco%nb_hru%a  
                     pco%nb_hru%already_read_in = .true.
                  endif
               case("hru_ls")
                  if (pco%ls_hru%already_read_in) then
                     result = print_prt_error(name)
                  else
                     backspace (107)
                     read (107,*,iostat=eof) name, pco%ls_hru%d, pco%ls_hru%m, pco%ls_hru%y, pco%ls_hru%a  
                     pco%ls_hru%already_read_in = .true.
                  endif
               case("hru_pw")
                  if (pco%pw_hru%already_read_in) then
                     result = print_prt_error(name)
                  else
                     backspace (107)
                     read (107,*,iostat=eof) name, pco%pw_hru%d, pco%pw_hru%m, pco%pw_hru%y, pco%pw_hru%a  
                     pco%pw_hru%already_read_in = .true.
                  endif
               case("hru_cb")
                  if (pco%cb_hru%already_read_in) then
                     result = print_prt_error(name)
                  else
                     backspace (107)
                     read (107,*,iostat=eof) name, pco%cb_hru%d, pco%cb_hru%m, pco%cb_hru%y, pco%cb_hru%a  
                     pco%cb_hru%already_read_in = .true.
                  endif
               case("hru_cb_vars")
                  if (pco%cb_vars_hru%already_read_in) then
                     result = print_prt_error(name)
                  else
                     backspace (107)
                     read (107,*,iostat=eof) name, pco%cb_vars_hru%d, pco%cb_vars_hru%m, pco%cb_vars_hru%y, pco%cb_vars_hru%a  
                     pco%cb_hru%already_read_in = .true.
                  endif
               case("hru-lte_wb")
                  if (pco%wb_sd%already_read_in) then
                     result = print_prt_error(name)
                  else
                     backspace (107)
                     read (107,*,iostat=eof) name, pco%wb_sd%d, pco%wb_sd%m, pco%wb_sd%y, pco%wb_sd%a  
                     pco%wb_sd%already_read_in = .true.
                  endif
               case("hru-lte_nb")
                  if (pco%nb_sd%already_read_in) then
                     result = print_prt_error(name)
                  else
                     backspace (107)
                     read (107,*,iostat=eof) name, pco%nb_sd%d, pco%nb_sd%m, pco%nb_sd%y, pco%nb_sd%a  
                     pco%nb_sd%already_read_in = .true.
                  endif
               case("hru-lte_ls")
                  if (pco%ls_sd%already_read_in) then
                     result = print_prt_error(name)
                  else
                     backspace (107)
                     read (107,*,iostat=eof) name, pco%ls_sd%d, pco%ls_sd%m, pco%ls_sd%y, pco%ls_sd%a  
                     pco%ls_sd%already_read_in = .true.
                  endif
               case("hru-lte_pw")
                  if (pco%pw_sd%already_read_in) then
                     result = print_prt_error(name)
                  else
                     backspace (107)
                     read (107,*,iostat=eof) name, pco%pw_sd%d, pco%pw_sd%m, pco%pw_sd%y, pco%pw_sd%a  
                     pco%pw_sd%already_read_in = .true.
                  endif
               case("channel")
                  if (pco%chan%already_read_in) then
                     result = print_prt_error(name)
                  else
                     backspace (107)
                     read (107,*,iostat=eof) name, pco%chan%d, pco%chan%m, pco%chan%y, pco%chan%a  
                     pco%chan%already_read_in = .true.
                  endif
               case("channel_sd")
                  if (pco%sd_chan%already_read_in) then
                     result = print_prt_error(name)
                  else
                     backspace (107)
                     read (107,*,iostat=eof) name, pco%sd_chan%d, pco%sd_chan%m, pco%sd_chan%y, pco%sd_chan%a  
                     pco%sd_chan%already_read_in = .true.
                  endif
               case("aquifer")
                  if (pco%aqu%already_read_in) then
                     result = print_prt_error(name)
                  else
                     backspace (107)
                     read (107,*,iostat=eof) name, pco%aqu%d, pco%aqu%m, pco%aqu%y, pco%aqu%a  
                     pco%aqu%already_read_in = .true.
                  endif
               case("reservoir")
                  if (pco%res%already_read_in) then
                     result = print_prt_error(name)
                  else
                     backspace (107)
                     read (107,*,iostat=eof) name, pco%res%d, pco%res%m, pco%res%y, pco%res%a  
                     pco%res%already_read_in = .true.
                  endif
               case("recall")
                  if (pco%recall%already_read_in) then
                     result = print_prt_error(name)
                  else
                     backspace (107)
                     read (107,*,iostat=eof) name, pco%recall%d, pco%recall%m, pco%recall%y, pco%recall%a  
                     pco%recall%already_read_in = .true.
                  endif
               case("hyd")
                  if (pco%hyd%already_read_in) then
                     result = print_prt_error(name)
                  else
                     backspace (107)
                     read (107,*,iostat=eof) name, pco%hyd%d, pco%hyd%m, pco%hyd%y, pco%hyd%a  
                     pco%hyd%already_read_in = .true.
                  endif
               case("ru")
                  if (pco%ru%already_read_in) then
                     result = print_prt_error(name)
                  else
                     backspace (107)
                     read (107,*,iostat=eof) name, pco%ru%d, pco%ru%m, pco%ru%y, pco%ru%a  
                     pco%ru%already_read_in = .true.
                  endif
               case("pest")
                  if (pco%pest%already_read_in) then
                     result = print_prt_error(name)
                  else
                     backspace (107)
                     read (107,*,iostat=eof) name, pco%pest%d, pco%pest%m, pco%pest%y, pco%pest%a  
                     pco%pest%already_read_in = .true.
                  endif
               case("basin_salt")
                  if (pco%salt_basin%already_read_in) then
                     result = print_prt_error(name)
                  else
                     backspace (107)
                     read (107,*,iostat=eof) name, pco%salt_basin%d, pco%salt_basin%m, pco%salt_basin%y, pco%salt_basin%a  
                     pco%salt_basin%already_read_in = .true.
                  endif
               case("hru_salt")
                  if (pco%salt_hru%already_read_in) then
                     result = print_prt_error(name)
                  else
                     backspace (107)
                     read (107,*,iostat=eof) name, pco%salt_hru%d, pco%salt_hru%m, pco%salt_hru%y, pco%salt_hru%a  
                     pco%salt_hru%already_read_in = .true.
                  endif
               case("ru_salt")
                  if (pco%salt_ru%already_read_in) then
                     result = print_prt_error(name)
                  else
                     backspace (107)
                     read (107,*,iostat=eof) name, pco%salt_ru%d, pco%salt_ru%m, pco%salt_ru%y, pco%salt_ru%a  
                     pco%salt_ru%already_read_in = .true.
                  endif
               case("aqu_salt")
                  if (pco%salt_aqu%already_read_in) then
                     result = print_prt_error(name)
                  else
                     backspace (107)
                     read (107,*,iostat=eof) name, pco%salt_aqu%d, pco%salt_aqu%m, pco%salt_aqu%y, pco%salt_aqu%a  
                     pco%salt_aqu%already_read_in = .true.
                  endif
               case("channel_salt")
                  if (pco%salt_chn%already_read_in) then
                     result = print_prt_error(name)
                  else
                     backspace (107)
                     read (107,*,iostat=eof) name, pco%salt_chn%d, pco%salt_chn%m, pco%salt_chn%y, pco%salt_chn%a  
                     pco%salt_chn%already_read_in = .true.
                  endif
               case("res_salt")
                  if (pco%salt_res%already_read_in) then
                     result = print_prt_error(name)
                  else
                     backspace (107)
                     read (107,*,iostat=eof) name, pco%salt_res%d, pco%salt_res%m, pco%salt_res%y, pco%salt_res%a  
                     pco%salt_res%already_read_in = .true.
                  endif
               case("wetland_salt")
                  if (pco%salt_wet%already_read_in) then
                     result = print_prt_error(name)
                  else
                     backspace (107)
                     read (107,*,iostat=eof) name, pco%salt_wet%d, pco%salt_wet%m, pco%salt_wet%y, pco%salt_wet%a  
                     pco%salt_wet%already_read_in = .true.
                  endif
               !! constituent outputs !rtb cs
               case("basin_cs")
                  if (pco%cs_basin%already_read_in) then
                     result = print_prt_error(name)
                  else
                     backspace (107)
                     read (107,*,iostat=eof) name, pco%cs_basin%d, pco%cs_basin%m, pco%cs_basin%y, pco%cs_basin%a  
                     pco%cs_basin%already_read_in = .true.
                  endif
               case("hru_cs")
                  if (pco%cs_hru%already_read_in) then
                     result = print_prt_error(name)
                  else
                     backspace (107)
                     read (107,*,iostat=eof) name, pco%cs_hru%d, pco%cs_hru%m, pco%cs_hru%y, pco%cs_hru%a  
                     pco%cs_hru%already_read_in = .true.
                  endif
               case("ru_cs")
                  if (pco%cs_ru%already_read_in) then
                     result = print_prt_error(name)
                  else
                     backspace (107)
                     read (107,*,iostat=eof) name, pco%cs_ru%d, pco%cs_ru%m, pco%cs_ru%y, pco%cs_ru%a  
                     pco%cs_ru%already_read_in = .true.
                  endif
               case("aqu_cs")
                  if (pco%cs_aqu%already_read_in) then
                     result = print_prt_error(name)
                  else
                     backspace (107)
                     read (107,*,iostat=eof) name, pco%cs_aqu%d, pco%cs_aqu%m, pco%cs_aqu%y, pco%cs_aqu%a  
                     pco%cs_aqu%already_read_in = .true.
                  endif
               case("channel_cs")
                  if (pco%cs_chn%already_read_in) then
                     result = print_prt_error(name)
                  else
                     backspace (107)
                     read (107,*,iostat=eof) name, pco%cs_chn%d, pco%cs_chn%m, pco%cs_chn%y, pco%cs_chn%a  
                     pco%cs_chn%already_read_in = .true.
                  endif
               case("res_cs")
                  if (pco%cs_res%already_read_in) then
                     result = print_prt_error(name)
                  else
                     backspace (107)
                     read (107,*,iostat=eof) name, pco%cs_res%d, pco%cs_res%m, pco%cs_res%y, pco%cs_res%a  
                     pco%cs_res%already_read_in = .true.
                  endif
               case("wetland_cs")
                  if (pco%cs_wet%already_read_in) then
                     result = print_prt_error(name)
                  else
                     backspace (107)
                     read (107,*,iostat=eof) name, pco%cs_wet%d, pco%cs_wet%m, pco%cs_wet%y, pco%cs_wet%a  
                     pco%cs_wet%already_read_in = .true.
                  endif
               case default
                  write(*, fmt="(a,a,a)", advance="yes") "Error: The output object ", name, "in the input file print.prt is not a valid object."
                  write(*, fmt="(a)") "       and cannot be processed"
                  !print*, "Error: The output object ", name, "in the input file print.prt is not a valid object."
                  !print*, "         and cannot be processed."
                  print*
                  error stop
            end select
          end do
        end if
        exit
      end do
      end if
      close (107)
      
      if (pco%day_start == 0) pco%day_start = 1
      if (pco%day_end == 0) pco%day_end = 366
      if (pco%yrc_start == 0) pco%yrc_start = time%yrc
      if (pco%yrc_end == 0) pco%yrc_end = time%yrc + time%nbyr
      if (pco%int_day <= 0) pco%int_day = 1
      pco%int_day_cur = pco%int_day
 
      return
      end subroutine basin_print_codes_read           