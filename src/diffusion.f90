module mod_diffusion
    use stdlib_kinds, only: dp
   !! Module contains procedures for diffusion calculations
   implicit none(type, external)

contains
   pure function calc_long_diffusion_param(diss_factor, dx, dt_long) result(diff_param)
      real(dp), intent(in) :: diss_factor ! Dissipation factor was alambda in og. version
      real(dp), intent(in) :: dx          ! Spatial increment
      real(dp), intent(in) :: dt_long     ! Long term timestep was DTL in og version
      real(dp) :: diff_param

      diff_param = diss_factor * dx**(4.0_dp/3.0_dp) * dt_long / dx**2
   end function calc_long_diffusion_param
end module mod_diffusion
