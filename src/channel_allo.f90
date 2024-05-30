      subroutine channel_allo
      
      use time_module
      use channel_module
      use hydrograph_module, only : sp_ob
      use channel_velocity_module
      
      implicit none
      
      integer mch       !units    |description
      
      mch = sp_ob%chan
      
      allocate (ch(mch))
      allocate (ch_vel(mch))
      allocate (ch_d(mch))
      allocate (ch_m(mch))
      allocate (ch_y(mch))
      allocate (ch_a(mch))
      allocate (rchsep(mch))

      if (time%step > 0) then
      allocate (hrtwtr(time%step))
      allocate (hharea(time%step))
      allocate (hdepth(time%step))
      allocate (rhy(time%step))
      allocate (hsdti(time%step))
      allocate (hhtime(time%step))
      allocate (hrttlc(time%step))
      allocate (hrtevp(time%step))
      allocate (hhstor(time%step))
      allocate (hrchwtr(time%step))
      allocate (halgae(time%step))
      allocate (hbactlp(time%step))
      allocate (hbactp(time%step))
      allocate (hbod(time%step))
      allocate (hchla(time%step))
      allocate (hdisox(time%step))
      allocate (hnh4(time%step))
      allocate (hno2(time%step))
      allocate (hno3(time%step))
      allocate (horgn(time%step))
      allocate (horgp(time%step))
      allocate (hsedst(time%step))
      allocate (hsedyld(time%step))
      allocate (hsolp(time%step))
      allocate (hsolpst(time%step))
      allocate (hsorpst(time%step))
      end if

      return
      end subroutine channel_allo