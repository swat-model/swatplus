      subroutine mgt_operatn
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs all management operations             

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    daylmn(:)   |hours         |shortest daylength occurring during the
!!                               |year
!!    dormhr(:)   |hours         |time threshold used to define dormant
!!                               |period for plant (when daylength is within
!!                               |the time specified by dormhr from the minimum
!!                               |daylength for the area, the plant will go
!!                               |dormant)
!!    phubase(:)  |heat units    |base zero total heat units (used when no
!!                               |land cover is growing
!!    iop(:,:,:)  |julian date   |date of tillage operation
!!    phut(:,:,:) |none          |fraction of heat units (base zero or plant)
!!                               |at which tillage occurs
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: plantop, dormant, harvkillop, harvestop, killop, tillmix

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use mgt_operations_module
      use hru_module, only : hru, yr_skip, phubase, ihru, ipl
      use plant_module
      use time_module
      
      implicit none

      integer :: j           !none          |HRU number
      real :: aphu           !heat units    |fraction of total heat units accumulated 
      integer :: isched      !              |

      j = ihru
      isched = hru(j)%mgt_ops
      if (sched(isched)%num_ops < 1) return
      
        mgt = sched(isched)%mgt_ops(hru(j)%cur_op)

        do while(mgt%mon == time%mo .and. mgt%day == time%day_mo)
          call mgt_sched (isched)
          if (sched(isched)%num_ops == 1) exit
          if (yr_skip(j) == 1) exit
        end do

        ipl = Max(mgt%op2, 1)
        if (pcom(j)%plcur(ipl)%gro == "n") then
          aphu = phubase(j)
        else
          aphu = pcom(j)%plcur(ipl)%phuacc
        end if 
        !if (dorm_flag == 1) aphu = 999.
        do while (mgt%husc > 0. .and. aphu > mgt%husc)
          call mgt_sched (isched)
          if (sched(isched)%num_ops == 1) exit
          ipl = Max(mgt%op2, 1)
          if (pcom(j)%plcur(ipl)%gro == "n") then
            aphu = phubase(j)
          else
            aphu = pcom(j)%plcur(ipl)%phuacc
          end if
          !if (dorm_flag == 1) aphu = 999.
          if (mgt%op == "skip") then
	        call mgt_sched (isched)
          end if
          if (yr_skip(j) == 1) exit
        end do
         
      return
      end subroutine mgt_operatn