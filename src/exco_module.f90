      module exco_module
    
      implicit none
          
      integer, dimension(:), allocatable :: exco_om_num
      integer, dimension(:), allocatable :: exco_pest_num
      integer, dimension(:), allocatable :: exco_path_num
      integer, dimension(:), allocatable :: exco_hmet_num
      integer, dimension(:), allocatable :: exco_salt_num
      character(len=16), dimension(:), allocatable :: exco_om_name
      character(len=16), dimension(:), allocatable :: exco_pest_name
      character(len=16), dimension(:), allocatable :: exco_path_name
      character(len=16), dimension(:), allocatable :: exco_hmet_name
      character(len=16), dimension(:), allocatable :: exco_salt_name
    
      type export_coefficient_datafiles       
        character(len=16) :: name
        character(len=16) :: om_file
        character(len=16) :: pest_file
        character(len=16) :: path_file
        character(len=16) :: hmet_file
        character(len=16) :: salts_file 
      end type export_coefficient_datafiles
      type (export_coefficient_datafiles), dimension(:), allocatable, save :: exco_db
      
      end module exco_module 