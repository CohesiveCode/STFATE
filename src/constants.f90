module constants
    use stdlib_kinds, only: dp
  use stdlib_constants, only: pi_dp
  implicit none
  
  real(dp), parameter :: pi = pi_dp
  real(dp), parameter :: g = 9.81_dp  ! Acceleration due to gravity (m/s^2)
  
  private
  public :: pi, g
end module constants