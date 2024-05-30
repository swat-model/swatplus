      module animal_herd_module
    
      implicit none

      type animals
        character(len=16) :: name   !           |animal name (cattle, sheep, goats, etc)
        real :: phyp = 0.           !           |
        real :: pthd = 0.           !           |
        real :: pthu = 0.           !           |
        real :: gzlm = 0.           !t/ha       |
        real :: gzin = 0.           !           |
        real :: gzwi = 0.           !kg/hd      |
        real :: gzwm = 0.           !kg/hd      |
        real :: pmlk = 0.           !kg/hd      |
        real :: antq = 0.           !           |
        integer :: igzd = 0         !           |
        integer :: impl = 0         !           |
        integer :: icvb = 0         !           |
        integer :: icvf = 0         !           |
        integer :: icwd = 0         !           |
      end type animals

      end module animal_herd_module 