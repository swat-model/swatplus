      subroutine aqu2d_init
    
      use hydrograph_module
      use sd_channel_module
      use maximum_data_module

      implicit none

      integer :: iaq                !none       |counter
      integer :: iaq_db             !none       |counter
      integer :: mfe                !none       |my first element (channel with smallest area)
      integer :: next1              !none       |counter
      integer :: iprv               !none       |counter 
      integer :: ipts               !none       |counter
      integer :: npts               !none       |counter
      integer :: icha               !none       |counter
      integer :: ichd               !none       |counter
      integer :: iob                !none       |counter
      real :: sum_len               !km         |total length of channel in aquifer
      real :: sum_left              !km         |lenght of channels after the baseflow stops
      real, dimension(:), allocatable :: next   !!next channel to dry up - sorted by drainage area
      
      !! set parameters needed to distribute gwflow to channels using geomorphical model
      if (db_mx%aqu2d <= 0) return
      do iaq = 1, sp_ob%aqu
        !! set channel drainage areas
        allocate (aq_ch(iaq)%ch(aq_ch(iaq)%num_tot))
        allocate (aqu_cha(aq_ch(iaq)%num_tot))
        allocate (next(aq_ch(iaq)%num_tot))
        sum_len = 0.
        do icha = 1, aq_ch(iaq)%num_tot
          ich = aq_ch(iaq)%num(icha)
          sd_ch(ich)%aqu_link = iaq
          sd_ch(ich)%aqu_link_ch = icha
          iob = sp_ob1%chandeg + ich - 1
          ichd = ob(iob)%props
          aqu_cha(icha)%area = ob(iob)%area_ha
          aqu_cha(icha)%len = sd_chd(ichd)%chl
          sum_len = sum_len + sd_chd(ichd)%chl
        end do
          
        !! order channels by drainage area - set linked list
        mfe = 1
        do icha = 2, aq_ch(iaq)%num_tot
          next1 = mfe
          npts = icha - 1
          do ipts = 1, npts
            if (aqu_cha(icha)%area < aqu_cha(next1)%area) then
              next(icha) = next1
              if (ipts == 1) then
                mfe = icha
              else
                next(iprv) = icha
              end if
              exit
            end if
            iprv = next1
            next1 = next(next1)
          end do
          if (npts > 0 .and. ipts == npts + 1) then
            next(iprv) = icha
          end if
        end do
            
        !! set the sorted object- aq_ch
        next1 = mfe
        do icha = 1, aq_ch(iaq)%num_tot
          aq_ch(iaq)%ch(icha) = aqu_cha(next1)
          next1 = next(next1)
        end do
        
        !! save total channel length in aquifer
        aq_ch(iaq)%len_tot = sum_len
        
        !! compute length of channel left when current channel dries up
        do icha = 1, aq_ch(iaq)%num_tot
          sum_len = sum_len - aq_ch(iaq)%ch(icha)%len
          aq_ch(iaq)%ch(icha)%len_left = sum_len
        end do

        deallocate (aqu_cha)
        deallocate (next)
        
      end do

      return
      end subroutine aqu2d_init