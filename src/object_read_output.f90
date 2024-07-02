      subroutine object_read_output
      
      use input_file_module
      use hydrograph_module
      use maximum_data_module
        
      implicit none
       
      character (len=80) :: titldum   !           |title of file
      character (len=80) :: header    !           |header of file
      integer :: eof                  !           |end of file
      integer :: imax                 !none       |determine max number for array (imax) and total number in file
      logical :: i_exist              !none       |check to determine if file exists
      integer :: i                    !none       |counter
      integer :: ii                   !none       !counter
      integer :: k                    !           |
      integer :: iunit  
      
      mobj_out = 0
      imax = 0      
      
      !! read old saveconc properties
      inquire (file=in_sim%object_prt,exist=i_exist)
      if (.not. i_exist .or. in_sim%object_prt == "null") then         
        allocate (ob_out(0:0))
      else
      do
        open (107,file=in_sim%object_prt)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof == 0)
            read (107,*,iostat=eof) i
            if (eof < 0) exit
            imax = Max(imax,i)
            mobj_out = mobj_out + 1
          end do
          
        db_mx%object_prt = mobj_out
        allocate (ob_out(0:imax))
        rewind (107)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit

        do i = 1, mobj_out
          read (107,*,iostat=eof) ii
          if (eof < 0) exit
          backspace (107)
          read (107,*,iostat=eof) k, ob_out(ii)%obtyp,                    &
             ob_out(ii)%obtypno, ob_out(ii)%hydtyp, ob_out(ii)%filename
          if (eof < 0) exit
          
          select case (ob_out(i)%obtyp)
            case ("hru")   !hru
              ob_out(i)%objno = sp_ob1%hru + ob_out(i)%obtypno - 1
              if (ob_out(i)%obtypno == 0) ob_out(i)%objno = 0
            case ("hlt")   !hru_lte
              ob_out(i)%objno = sp_ob1%hru_lte + ob_out(i)%obtypno - 1
            case ("ru")   !hru
              ob_out(i)%objno = sp_ob1%ru + ob_out(i)%obtypno - 1
            case ("res")   !hru_lte
              ob_out(i)%objno = sp_ob1%res + ob_out(i)%obtypno - 1
            case ("cha")   !channel
              ob_out(i)%objno = sp_ob1%chan + ob_out(i)%obtypno - 1
            case ("exc")   !export coefficient
              ob_out(i)%objno = sp_ob1%exco + ob_out(i)%obtypno - 1
            case ("dr")   !delivery ratio
              ob_out(i)%objno = sp_ob1%dr + ob_out(i)%obtypno - 1
            case ("out")   !outlet
              ob_out(i)%objno = sp_ob1%outlet + ob_out(i)%obtypno - 1
            case ("sdc")   !swat-deg channel
              ob_out(i)%objno = sp_ob1%chandeg + ob_out(i)%obtypno - 1
          end select
      
          select case (ob_out(i)%hydtyp)
            case ("tot")   !total flow
               ob_out(i)%hydno = 1
            case ("rhg")   !recharge
               ob_out(i)%hydno = 2              
            case ("sur")   !surface
               ob_out(i)%hydno = 3 
            case ("lat")   !lateral
               ob_out(i)%hydno = 4
            case ("til")   !tile
               ob_out(i)%hydno = 5 
            case ("sol")  !soil moisture by layer 
               ob_out(i)%hydno = 6 
            case ("soln")  !soil n and p by layer 
               ob_out(i)%hydno = 7
            case ("solpn")  !soil n and p for profile 
               ob_out(i)%hydno = 8
            case ("plt")  !plants status  
               ob_out(i)%hydno = 9
            case ("ch_fp")  !channel and flood plain water balance  
               ob_out(i)%hydno = 10
            end select
         iunit = ob_out(i)%unitno
         
         !! open file and write header
         open (iunit+i,file = ob_out(i)%filename,recl=2000)
         !write (iunit+i,*) "OBJECT.PRT                ", ob_out(i)%filename
         select case (ob_out(i)%hydno)
         case (1,2,3,4,5)
           write (iunit+i,*) hyd_hdr_time, hyd_hdr
         !case (1-5)
         !write (iunit+i,*) hyd_hdr_time, hyd_hdr
         case (6)
           write (iunit+i,*) hyd_hdr_time, sol_hdr
         case (9)
           write (iunit+i, '(1X4A,16(4XA),2A,14(4XA))') hyd_hdr_time, plt_hdr, plt_hdr
         case (10)
           write (iunit+i,*) hyd_hdr_time, fp_hdr
         end select
         
        end do  ! mobj_out  
        exit
      end do
      end if
        
      close (107)
      
      return
      
      end subroutine object_read_output