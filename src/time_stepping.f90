module mod_time_stepping
   use stdlib_kinds, only: dp
   implicit none(type, external)
   
   private
   public :: calc_dt_with_v_vel


contains
   pure function calc_dt_with_v_vel(vert_radius, FF, alpha0, H, v_vel) result(dt)
      !! dt increment when vertical velocity is non-zero. 
      !! Function is from the code. I'm not sure where the derivation is from

      real(dp), intent(in) :: vert_radius !! Vertical radius of hemispherical cloud
      real(dp), intent(in) :: FF          !! ??, Might be
      real(dp), intent(in) :: alpha0      !! Entrainment coefficient for turbulent thermal
      real(dp), intent(in) :: H           !! ??, Water depth
      real(dp), intent(in) :: v_vel       !! Vertical velocity
      real(dp)             :: dt          !! Time increment

      dt =  0.01_dp * vert_radius * FF * (1.0_dp + alpha0 * H / vert_radius)**2 / (v_vel * 2.0)
   end function calc_dt_with_v_vel

! pure function calc_zero_v_vel_dt(FF, EE1) result(dt)


end module mod_time_stepping
