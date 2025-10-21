module mod_dump_bottom_deriv
   !! Module contains procedures for calculating derivatives during the bottom collapse phase
   use stdlib_kinds, only: dp
   use mod_constants, only: pi

   implicit none(type, external)
   private
   public :: calc_dump_bot_dM_dt
   
contains

!    pure function calc_dump_bot_E()
   pure function calc_dump_bot_dM_dt(F_b, F_f, drag, E, rho_a, U_a, S_i, rho_p, U_c) result(dM_dt)
      !! Ref(1) - Eqn(3.51) - Calculate the time derivative of the momentum of the cloud.
      real(dp), intent(in) :: F_b          !! Buoyancy force acting on the cloud
      real(dp), intent(in) :: F_f          !! Skin friction drag
      real(dp), intent(in) :: drag(3)      !! Drag force on the cloud
      real(dp), intent(in) :: E            !! Entrainment rate in m^3/s
      real(dp), intent(in) :: rho_a        !! Density of the ambient fluid (kg/m^3)
      real(dp), intent(in) :: U_a(3)       !! Ambient velocity at the location of the cloud
      real(dp), intent(in) :: S_i(:)       !! Volume rate of solids settling out of the cloud
      real(dp), intent(in) :: rho_p(:)     !! Density of particles in cloud
      real(dp), intent(in) :: U_c(3)       !! Velocity of the cloud in m/s
      real(dp)             :: dM_dt(3)     !! Result: Time derivative of the momentum of the cloud in kg·m/s^2

      ! Not entirely sure why S_i * rho_p is multiplied by U_c, instead of the relative particle velocities
      ! but this is how it is in the paper
      dM_dt = [0.0_dp, F_b, 0.0_dp] - drag + E * rho_a * U_a - sum(S_i * rho_p) * U_c - [0.0_dp, F_f, 0.0_dp]
   end function calc_dump_bot_dM_dt

end module mod_dump_bottom_deriv
