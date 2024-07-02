      !remove nutrient mass from channels with a diversion
      subroutine recall_nut(irec)
      
      
      use basin_module
      use hydrograph_module
      use time_module
      use constituent_mass_module
      
      implicit none 

      !incoming variables
      integer :: irec
      
      !local variables
      integer :: ichan        !            |id of source channel
      real :: sol_conc        !g/m3        |concentration of solute in diversion water
      real :: div_mass        !kg          |mass of constituent in diversion water
      integer :: dum
      
      
      !channel from which water is diverted
      ichan = ob(icmd)%obtypno_out(1) 
      
      !determine concentration of channel (source) water and diverted mass
      sol_conc = 0.
      if(ch_stor(ichan)%flo > 10.) then !only proceed if channel has water
        
        !no3
        sol_conc = (ch_stor(ichan)%no3*1000.) / ch_stor(ichan)%flo !g/m3 in channel water
        div_mass = (sol_conc * recall(irec)%hd(time%day,time%yrs)%flo) / 1000. !kg/day  
        if((div_mass*(-1)) > ch_stor(ichan)%no3) then
          div_mass = ch_stor(ichan)%no3 * (-1) !only take what is there
        endif
        ch_stor(ichan)%no3 = ch_stor(ichan)%no3 + div_mass
        
        !solp
        sol_conc = (ch_stor(ichan)%solp*1000.) / ch_stor(ichan)%flo !g/m3 in channel water
        div_mass = (sol_conc * recall(irec)%hd(time%day,time%yrs)%flo) / 1000. !kg/day  
        if((div_mass*(-1)) > ch_stor(ichan)%solp) then
          div_mass = ch_stor(ichan)%solp * (-1) !only take what is there
        endif
        ch_stor(ichan)%solp = ch_stor(ichan)%solp + div_mass
        
        !nh3
        sol_conc = (ch_stor(ichan)%nh3*1000.) / ch_stor(ichan)%flo !g/m3 in channel water
        div_mass = (sol_conc * recall(irec)%hd(time%day,time%yrs)%flo) / 1000. !kg/day  
        if((div_mass*(-1)) > ch_stor(ichan)%nh3) then
          div_mass = ch_stor(ichan)%nh3 * (-1) !only take what is there
        endif
        ch_stor(ichan)%nh3 = ch_stor(ichan)%nh3 + div_mass
        
        !no2
        sol_conc = (ch_stor(ichan)%no2*1000.) / ch_stor(ichan)%flo !g/m3 in channel water
        div_mass = (sol_conc * recall(irec)%hd(time%day,time%yrs)%flo) / 1000. !kg/day  
        if((div_mass*(-1)) > ch_stor(ichan)%no2) then
          div_mass = ch_stor(ichan)%no2 * (-1) !only take what is there
        endif
        ch_stor(ichan)%no2 = ch_stor(ichan)%no2 + div_mass
        
        !dox
        sol_conc = (ch_stor(ichan)%dox*1000.) / ch_stor(ichan)%flo !g/m3 in channel water
        div_mass = (sol_conc * recall(irec)%hd(time%day,time%yrs)%flo) / 1000. !kg/day  
        if((div_mass*(-1)) > ch_stor(ichan)%dox) then
          div_mass = ch_stor(ichan)%dox * (-1) !only take what is there
        endif
        ch_stor(ichan)%dox = ch_stor(ichan)%dox + div_mass
        
        !orgn
        sol_conc = (ch_stor(ichan)%orgn*1000.) / ch_stor(ichan)%flo !g/m3 in channel water
        div_mass = (sol_conc * recall(irec)%hd(time%day,time%yrs)%flo) / 1000. !kg/day  
        if((div_mass*(-1)) > ch_stor(ichan)%orgn) then
          div_mass = ch_stor(ichan)%orgn * (-1) !only take what is there
        endif
        ch_stor(ichan)%orgn = ch_stor(ichan)%orgn + div_mass
        
      else
        ob(icmd)%hd(1)%no3 = 0.
        ob(icmd)%hd(1)%solp = 0.
        ob(icmd)%hd(1)%nh3 = 0.
        ob(icmd)%hd(1)%no2 = 0.
        ob(icmd)%hd(1)%dox = 0.
        ob(icmd)%hd(1)%orgn = 0.
      endif
 
      
100   format(i8,i8,100e16.8)      
      
      return
      end subroutine recall_nut