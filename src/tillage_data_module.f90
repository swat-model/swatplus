      module tillage_data_module
    
      implicit none
           
      integer :: bmix_idtill = 0    !!              |none          |the tilldb index of the biomix tillage. 
      integer :: till_eff_days = 30  !!              |none          |length of days a tillage operation will have an effect
      real    :: bmix_eff = 0.      !!              |none          |biological mixing efficieny
      real    :: bmix_depth = 0.    !!              |none          |biological mixing depth
      real    :: zz_bmix_coef_a = 3.0    !!              |none          !Base intercept in zz equation in mgt_tillfactor.f90 for biomixing
      real    :: zz_bmix_coef_b = 5.0   !!              |none          !slope of in zz equation in mgt_tillfactor.f90 for biomixing 
      real    :: zz_bmix_coef_c = -5.5   !!              |none          !exponent multiplier in zz equation in mgt_tillfactor.f90 for biomixing
      real    :: zz_emix_coef_a = 3.0    !!              |none          !Base intercept in zz equation in mgt_tillfactor.f90 for tillage mixing
      real    :: zz_emix_coef_b = 5.0   !!              |none          !slope of in zz equation in mgt_tillfactor.f90 for tillage mixing 
      real    :: zz_emix_coef_c = -5.5   !!              |none          !exponent multiplier in zz equation in mgt_tillfactor.f90 for tillage mixing

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