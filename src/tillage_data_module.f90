      module tillage_data_module
    
      implicit none
           
      integer :: bmix_idtill = 0    !!              |none          |the tilldb index of the biomix tillage. 
      integer :: till_eff_days = 30  !!              |none          |length of days a tillage operation will have an effect
      real    :: bmix_eff = 0.      !!              |none          |biological mixing efficieny
      real    :: bmix_depth = 0.    !!              |none          |biological mixing depth

      type tillage_db
        character(len=16) :: tillnm = " "
        real :: effmix = 0.          !! none               |mixing efficiency of tillage operation
        real :: deptil = 0.          !! mm                 |depth of mixing caused by tillage
        real :: ranrns = 0.          !! mm                 |random roughness
        real :: ridge_ht = 0.        !! mm                 |ridge height
        real :: ridge_sp = 0.        !! mm                 |ridge interval (or row spacing)
      end type tillage_db
      type (tillage_db), dimension(:),allocatable, save :: tilldb  

      

            
      end module tillage_data_module 