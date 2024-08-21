      subroutine channel_allo
      
      use time_module
      use channel_module
      use hydrograph_module, only : sp_ob
      use channel_velocity_module
      
      implicit none
      
      integer :: mch = 0  !units    |description
      
      mch = sp_ob%chan
      
      allocate (ch(mch))
      allocate (ch_vel(mch))
      allocate (ch_d(mch))
      allocate (ch_m(mch))
      allocate (ch_y(mch))
      allocate (ch_a(mch))
      allocate (rchsep(mch), source = 0.)

      if (time%step > 0) then
      allocate (hrtwtr(time%step), source = 0.)
      allocate (hharea(time%step), source = 0.)
      allocate (hdepth(time%step), source = 0.)
      allocate (rhy(time%step), source = 0.)
      allocate (hsdti(time%step), source = 0.)
      allocate (hhtime(time%step), source = 0.)
      allocate (hrttlc(time%step), source = 0.)
      allocate (hrtevp(time%step), source = 0.)
      allocate (hhstor(time%step), source = 0.)
      allocate (hrchwtr(time%step), source = 0.)
      allocate (halgae(time%step), source = 0.)
      allocate (hbactlp(time%step), source = 0.)
      allocate (hbactp(time%step), source = 0.)
      allocate (hbod(time%step), source = 0.)
      allocate (hchla(time%step), source = 0.)
      allocate (hdisox(time%step), source = 0.)
      allocate (hnh4(time%step), source = 0.)
      allocate (hno2(time%step), source = 0.)
      allocate (hno3(time%step), source = 0.)
      allocate (horgn(time%step), source = 0.)
      allocate (horgp(time%step), source = 0.)
      allocate (hsedst(time%step), source = 0.)
      allocate (hsedyld(time%step), source = 0.)
      allocate (hsolp(time%step), source = 0.)
      allocate (hsolpst(time%step), source = 0.)
      allocate (hsorpst(time%step), source = 0.)
      end if

      return
      end subroutine channel_allo