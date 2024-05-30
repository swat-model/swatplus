    subroutine search(sch, max, cfind, iseq)
    
    implicit none
    
    character(len=50) :: cfind                   !             |
    integer :: iseq                              !             |
    integer :: max                               !             | 
    character(len=50), dimension(max) ::  sch    !             |
    integer :: nf                                !             |
    integer :: int                               !             |
    integer :: nn                                !none         |number of subdaily computation points for stable 
                                                 !             |routing in the muskingum routing method
    integer :: nl                                !none         |number of layers being mixed
    
    nf = 1
    nl = max
    
    do int = 1, 25  !max for 100,000,000
      nn = (nl - nf) / 2 + nf
      
      if (nl - nf == 1) then
        if (sch(nl) == cfind) then
          iseq = nl
          return
        end if
        if (sch(nf) == cfind) then
          iseq = nf
          return
        end if
      end if
        
      if (sch(nn) == cfind) then
        iseq = nn
        return
      end if
      
      if (sch(nn) > cfind) then
        nl = nn
      end if
      
      if (sch(nn) < cfind) then 
        nf = nn
      end if
    end do

100  format (1x,2a) 
    
    return
    end subroutine search