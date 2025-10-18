module mod_constants
   use stdlib_kinds, only: dp
   use stdlib_constants, only: pi_dp
   implicit none(type, external)
   private
   public :: pi, g, min_allow_fluid_density


   real(dp), parameter :: pi = pi_dp
   real(dp), parameter :: g = 9.81_dp  ! Acceleration due to gravity (m/s^2)
   ! Limit for the cloud fluid density. Used when initializing particles in cloud
   real(dp), parameter :: min_allow_fluid_density = -1.0_dp ! Setting to small number for the time being was 0.97 gm/cc in og version
   real(dp), parameter :: nu = 1.0e-6 ! [m^2/s] Kinematic viscosity of water

end module mod_constants
