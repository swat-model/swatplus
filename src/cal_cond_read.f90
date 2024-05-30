      subroutine cal_cond_read
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this function computes new paramter value based on 
!!    user defined change

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    val_cur     |variable      |current parameter value
!!                               |the standard temperature (20 degrees C)
!!    chg         |data type     |contains information on variable change
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
      
      use input_file_module
      use maximum_data_module
      use calibration_data_module
      use conditional_module
      
      implicit none

      character (len=80) :: titldum                      !           |title of file
      character (len=80) :: header                       !           |header of file
      integer :: eof                                     !           |end of file
      logical :: i_exist                                 !none       |check to determine if file exists
      integer :: num_dtls                                !none       |end of loop
      integer :: i                                       !none       |counter
      integer :: icond                                   !none       |counter

      num_dtls = 0
        
      !!read decision tables used for land use scenarios - xwalk with scen_lu.dtl
      inquire (file="scen_dtl.upd", exist=i_exist)
      if (.not. i_exist .or. "scen_dtl.upd" == "null") then
        allocate (upd_cond(0:0))
      else
      do
        open (107,file="scen_dtl.upd")
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) num_dtls
        if (eof < 0) exit
        
        allocate (upd_cond(0:num_dtls))
        db_mx%cond_up = num_dtls
        
        read (107,*,iostat=eof) header
        if (eof < 0) exit

      do i = 1, num_dtls
        read (107,*,iostat=eof) upd_cond(i)%max_hits, upd_cond(i)%typ, upd_cond(i)%dtbl
        if (eof < 0) exit

        !! crosswalk parameters with calibration parameter db
        do icond = 1, db_mx%dtbl_scen
          if (upd_cond(i)%dtbl == dtbl_scen(icond)%name) then
            upd_cond(i)%cond_num = icond
            exit
          end if
        end do
 
      end do
      exit
      end do
      end if      

      return
      end subroutine cal_cond_read