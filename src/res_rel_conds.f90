       subroutine res_rel_conds (ictbl, stor, inflo, pdsi)
      
       use reservoir_conditions_module
       use time_module
       use hydrograph_module
       
       implicit none
       
       integer ::  icon, modu, iscon
       integer, intent(in) :: ictbl
       real, intent(in) :: stor
       real, intent(in) :: inflo
       real, intent(in) :: pdsi
      
       !!ictbl = 1     nbs
       modu = 0
       
       !! check all conditions and find module
       do icon = 1, ctbl(ictbl)%num_conds
         hit = "y"
           do iscon = 1, ctbl(ictbl)%conds(icon)%num_conds
             select case (ctbl(ictbl)%conds(icon)%scon(iscon)%var)
               case ("stor")
                 call cond_real_c (ctbl(ictbl)%conds(icon)%scon(iscon)%op, stor, ctbl(ictbl)%conds(icon)%scon(iscon)%val)
               case ("inflo")
                 call cond_real_c (ctbl(ictbl)%conds(icon)%scon(iscon)%op, inflo, ctbl(ictbl)%conds(icon)%scon(iscon)%val)
               case ("pdsi")
                 call cond_real_c (ctbl(ictbl)%conds(icon)%scon(iscon)%op, pdsi, ctbl(ictbl)%conds(icon)%scon(iscon)%val)
               case ("day")
                 call cond_integer_c (ctbl(ictbl)%conds(icon)%scon(iscon)%op, day, int(ctbl(ictbl)%conds(icon)%scon(iscon)%val))
             end select
           end do
           if (hit == "y") exit
        end do
        
        !! if no conditions were met - set release to zero and return
        if (icon > ctbl(ictbl)%num_conds) then
          release = 0.
          return
        end if
        
        !! check module conditions
        modu = int (ctbl(ictbl)%conds(icon)%action)
        modu = modu + 1
        do icon = 1, ctbl(ictbl)%mods(modu)%num_conds
          hit = "y"
          do iscon = 1, ctbl(ictbl)%mods(modu)%con(icon)%num_conds
            select case (ctbl(ictbl)%mods(modu)%con(icon)%scon(iscon)%var)
            case ("inflo")
              call cond_real_c (ctbl(ictbl)%mods(modu)%con(icon)%scon(iscon)%op, inflo,    &
                  ctbl(ictbl)%mods(modu)%con(icon)%scon(iscon)%val)
              case ("stor")
               call cond_real_c (ctbl(ictbl)%mods(modu)%con(icon)%scon(iscon)%op, stor,    &
                   ctbl(ictbl)%mods(modu)%con(icon)%scon(iscon)%val)
            end select
          end do
          if (hit == "y") exit
        end do
        
        !! if sub conditions never met set release to zero
        if (icon > ctbl(ictbl)%mods(modu)%num_conds) then
          ht2%flo = 0.
        else
          ht2%flo = ctbl(ictbl)%mods(modu)%con(icon)%action
        end if
          
        return
        end subroutine res_rel_conds         