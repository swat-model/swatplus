      module dr_module
    
      implicit none
                    
      integer, dimension(:), allocatable :: dr_om_num
      integer, dimension(:), allocatable :: dr_pest_num
      integer, dimension(:), allocatable :: dr_path_num
      integer, dimension(:), allocatable :: dr_hmet_num
      integer, dimension(:), allocatable :: dr_salt_num
      character(len=16), dimension(:), allocatable :: dr_om_name
      character(len=16), dimension(:), allocatable :: dr_pest_name
      character(len=16), dimension(:), allocatable :: dr_path_name
      character(len=16), dimension(:), allocatable :: dr_hmet_name
      character(len=16), dimension(:), allocatable :: dr_salt_name
    
      type delivery_ratio_datafiles       
        character(len=16) :: name
        character(len=16) :: om_file
        character(len=16) :: pest_file
        character(len=16) :: path_file
        character(len=16) :: hmet_file
        character(len=16) :: salts_file 
      end type delivery_ratio_datafiles
      type (delivery_ratio_datafiles), dimension(:),allocatable, save :: dr_db
      
      end module dr_module 