      subroutine wallo_transfer (iwallo, itrn)
      
      use water_allocation_module
      use hydrograph_module
      use constituent_mass_module
      use sd_channel_module
      use aquifer_module
      use reservoir_module
      use time_module
      
      implicit none 

      integer, intent (in):: iwallo         !water allocation object number
      integer, intent (in) :: itrn          !water demand object number
      integer :: isrc = 0                   !source object number
      integer :: iconv = 0                  !conveyance object number (pipe or pump number)

      
      !! transfer water to receiving object from each source
      do isrc = 1, wallo(iwallo)%trn(itrn)%src_num
        iconv = wallo(iwallo)%trn(itrn)%src(isrc)%conv_num
        select case (wallo(iwallo)%trn(itrn)%src(isrc)%conv_typ)
        case ("pipe")
          !! organic hydrograph being transfered from the source to the receiving object
          wal_omd(iwallo)%trn(itrn)%src(isrc)%hd = (1. - pipe(iconv)%loss_fr) *            &
                                                 wal_omd(iwallo)%trn(itrn)%src(isrc)%hd
        case ("pump")
          !! include pump losses here
        end select
      end do
      return
      end subroutine wallo_transfer