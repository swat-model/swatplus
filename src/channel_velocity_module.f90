      module channel_velocity_module
    
      implicit none

      type channel_velocity_parameters
          real :: area = 0.             !m^2        |cross sectional area of flow at bankfull depth
          real :: vel_bf = 0.           !m^3/s      |flow rate when reach is at bankful depth
          real :: wid_btm = 0.          !m          |bottom width of main channel
          real :: dep_bf = 0.           !m          |depth of water when reach is at bankfull depth
          real :: velav_bf = 0.         !m/s        |average velocity when reach is at bankfull depth
          real :: celerity_bf = 0.      !m/s        |wave celerity when reach is at bankfull depth
          real :: st_dis = 0.           !hr         |storage time constant for reach at bankfull depth
          real :: vel_1bf = 0.          !m/s        |average velocity when reach is at 0.1 bankfull depth (low flow)
          real :: celerity_1bf = 0.     !m/s        |wave celerity when reach is at 0.1 bankfull depth (low flow)
          real :: stor_dis_1bf = 0.     !hr         |storage time constant for reach at 0.1 bankfull depth (low flow)
      end type channel_velocity_parameters
      type (channel_velocity_parameters), dimension(:), allocatable :: ch_vel
      type (channel_velocity_parameters), dimension(:), allocatable :: sd_ch_vel
      type (channel_velocity_parameters), dimension(:), allocatable :: grwway_vel
 
      end module channel_velocity_module