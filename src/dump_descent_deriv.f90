module mod_dump_descent_deriv
   ! Module to calculate the derivatives of the instant descent equations
   use stdlib_kinds, only: dp
   use mod_constants, only: g, pi
   use mod_scalar_interp, only: interp1d

   implicit none(type, external)
    private
    public :: calc_dM_dt, calc_dK_dt

contains

   pure function calc_dM_dt(F_b, drag, E, rho_a, U_a, S_i, rho_p, U_c) result(dM_dt)
      !! Eqn. 3.2 - Calculate the time derivative of the momentum of the cloud
      ! dM/dt = [0, F_b, 0] - D + E * rho_a * U_a - sum(S_i * rho_p) * U_c
      ! Note: S_i and rho_p are arrays for each particle type in the cloud
      real(dp), intent(in) :: F_b          ! Buoyancy force acting on the cloud
      real(dp), intent(in) :: drag(3)      ! Drag force on the cloud
      real(dp), intent(in) :: E            ! Entrainment rate in m^3/s
      real(dp), intent(in) :: rho_a        ! Density of the ambient fluid (kg/m^3)
      real(dp), intent(in) :: U_a(3)       ! Ambient velocity at the location of the cloud
      real(dp), intent(in) :: S_i(:)       ! Volume rate of solids settling out of the cloud (m^3/s)
      real(dp), intent(in) :: rho_p(:)     ! Density of particles in cloud
      real(dp), intent(in) :: U_c(3)       ! Velocity of the cloud in m/s
      real(dp)             :: dM_dt(3)     ! Time derivative of the momentum of the cloud in kg·m/s^2

      ! Not entirely sure why S_i * rho_p is multiplied by U_c, instead of the relative particle velocities
      ! but this is how it is in the paper
      dM_dt = [0.0_dp, F_b, 0.0_dp] - drag + E * rho_a * U_a - sum(S_i * rho_p) * U_c
   end function calc_dM_dt

   elemental pure function calc_dK_dt(A, density_gradient) result(dK_dt)
      !! Eqn. 3.14 - 
      ! dK/dt = A · density_gradient
      real(dp), intent(in) :: A                     ! Dissipation parameter (kg)
      real(dp), intent(in) :: density_gradient      ! Vertical density gradient of the ambient density field (kg/m^4)
      real(dp)             :: dK_dt                 ! 

      dK_dt = -A * density_gradient
   end function calc_dK_dt

end module mod_dump_descent_deriv
