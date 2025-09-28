module mod_cloud
    use stdlib_kinds, only: dp
    implicit none
    private
    public :: calc_y_centroid, check_cloud_in_domain

contains

function calc_y_centroid(a) result(centroid)
   real(dp), intent(in) :: a
   real(dp) :: centroid
   ! Calculate the centroid of a hemisphere of radius a
   centroid = (3.0_dp/8.0_dp) * a
end function calc_y_centroid

subroutine check_cloud_in_domain(cloud_pos, a, b, x, y, z)
   ! Subroutine checks that a cloud fits within the domain defined by x, y, z
   ! Inputs:
   real(dp), intent(in) :: cloud_pos(3) ! Position of the cloud (x, y, z) in meters
   real(dp), intent(in) :: a             ! Radius of the cloud in meters
   real(dp), intent(in) :: b             ! Half-width of the cloud in meters
   real(dp), intent(in) :: x(:), y(:), z(:) ! Domain bounds

   ! Local variables
   integer :: nx, ny, nz
   real(dp) :: y_centroid, top_y, bottom_y
   real(dp), parameter :: tol = epsilon(1.0_dp)

   nx = size(x)
   ny = size(y)
   nz = size(z)

   if (cloud_pos(1) - b .le. x(1)-tol .or. cloud_pos(1) + b .ge. x(nx)+tol) then
      print *, cloud_pos(1) - b, cloud_pos(1) + b, x(1), x(nx)
      print *, "Error: Cloud does not fit within the x domain"
      ! stop
   end if

   ! For vertical coordinate centroid is not on the surface of the ellipsoid
   y_centroid = calc_y_centroid(a)
   top_y    = cloud_pos(2) - y_centroid
   bottom_y = cloud_pos(2) + (a - y_centroid)
   if (top_y .le. y(1) - tol .or. bottom_y .ge. y(ny) + tol) then
      print *, "top y, bottom y, y(1), y(ny): ", top_y, bottom_y, y(1), y(ny)
      print *, "Error: Cloud does not fit within the y domain"
   end if

   if(cloud_pos(3) - b .le. z(1) - tol .or. cloud_pos(3) + b .ge. z(nz) + tol) then
      print *, "Error: Cloud does not fit within the z domain"
   end if
end subroutine check_cloud_in_domain

end module mod_cloud