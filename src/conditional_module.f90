      module conditional_module
    
      implicit none
    
      !integer :: rndseed_cond = 748932582   ! random number seed for dtbl conditional

      type conditions_var
        character(len=25) :: var            ! condition variable (ie volume, flow, sw, time, etc)
        character(len=25) :: ob             ! object variable (ie res, hru, canal, etc)
        integer :: ob_num                   ! object number
        character(len=25) :: lim_var        ! limit variable (ie evol, pvol, fc, ul, etc)
        character(len=25) :: lim_op         ! limit operator (*,+,-)
        real :: lim_const                   ! limit constant
      end type conditions_var
              
      type actions_var
        character(len=25) :: typ            ! type of action (ie reservoir release, irrigate, fertilize, etc)
        character(len=25) :: ob             ! object variable (ie res, hru, canal, etc)
        integer :: ob_num                   ! object number
        character(len=25) :: name           ! name of action
        character(len=25) :: option         ! action option - specific to type of action (ie for reservoir, option to
                                            ! input rate, days of drawdown, weir equation pointer, etc
        real :: const                       ! constant used for rate, days, etc
        real :: const2 = 1                  ! additional constant used for rate, days, etc
        character(len=25) :: file_pointer   ! pointer for option (ie weir equation pointer)
      end type actions_var
       
      type decision_table
        character (len=25) :: name                                      ! name of the decision table
        integer :: conds                                                ! number of conditions
        integer :: alts                                                 ! number of alternatives
        integer :: acts                                                 ! number of actions
        type (conditions_var), dimension(:), allocatable :: cond        ! conditions
        character(len=25), dimension(:,:), allocatable :: alt           ! condition alternatives
        type (actions_var), dimension(:), allocatable :: act            ! actions
        character(len=1), dimension(:,:), allocatable :: act_outcomes   ! action outcomes ("y" to perform action; "n" to not perform)
        character(len=1), dimension(:), allocatable :: act_hit          ! "y" if all condition alternatives (rules) are met; "n" if not
        integer, dimension(:), allocatable :: act_typ                   ! pointer to action type (ie plant, fert type, tillage implement, release type, etc)
        integer, dimension(:), allocatable :: act_app                   ! pointer to operation or application type (ie harvest.ops, chem_app.ops, wier shape, etc)
        integer, dimension(:), allocatable :: con_act                   ! pointer for days since last action condition to point to appropriate action
        integer :: hru_lu = 0                                           ! number of hru's in the land_use condition(s) - used for probabilistic mgt operations or lu change
        real :: ha_lu = 0.                                              ! area of land_use in ha
        integer :: hru_lu_cur = 0                                       ! number of hru's in the land_use condition(s) that have currently been applied
        real :: hru_ha_cur = 0.                                         ! area of land_use in ha that has currently been applied
        integer :: days_prob = 0                                        ! days since start of application window
        integer :: day_prev = 0                                         ! to check if same day - don't increment day in application window
        real :: prob_cum  = 0.                                          ! cumulative probability of application on current day of window
        real :: frac_app  = 0.                                          ! fraction of time (during each window) the application occurs
      end type decision_table
      type (decision_table), dimension(:), allocatable, target :: dtbl_lum
      type (decision_table), dimension(:), allocatable, target :: dtbl_res
      type (decision_table), dimension(:), allocatable, target :: dtbl_scen
      type (decision_table), dimension(:), allocatable, target :: dtbl_flo
      type (decision_table), pointer :: d_tbl
      
      end module conditional_module   