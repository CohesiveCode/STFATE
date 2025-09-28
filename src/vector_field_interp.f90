module mod_vec_interp
    use stdlib_kinds, only: dp
    use mod_search, only: binary_search
    use mod_scalar_interp, only: interp3d

    implicit none
    private
    public :: interp3d_vec_field

contains
    pure function interp3d_vec_field(x_vec, x, y, z, f1, f2, f3) result(fi)
        real(dp), intent(in) :: x_vec(3)      ! Point to interpolate at (x, y, z)
        real(dp), intent(in) :: x(:)          ! 1D array of x-coordinates (must be sorted)
        real(dp), intent(in) :: y(:)          ! 1D array of y-coordinates (must be sorted)
        real(dp), intent(in) :: z(:)          ! 1D array of z-coordinates (must be sorted)
        real(dp), intent(in) :: f1(:, :, :)   ! 3D array of function values at (x,y,z) grid points
        real(dp), intent(in) :: f2(:, :, :)   ! 3D array of function values at (x,y,z) grid points
        real(dp), intent(in) :: f3(:, :, :)   ! 3D array of function values at (x,y,z) grid points
        real(dp) :: fi(3)                     ! Interpolated function value at (xi, yi, zi)

        ! Local variables
        integer :: i, j, k
        integer :: nx, ny, nz
        nx = size(x)
        ny = size(y)
        nz = size(z)

        if (x_vec(1) < x(1) .or. x_vec(1) > x(nx) .or. &
            x_vec(2) < y(1) .or. x_vec(2) > y(ny) .or. &
            x_vec(3) < z(1) .or. x_vec(3) > z(nz)) then
            ! print *, "Point: ", x_vec(1), x_vec(2), x_vec(3)
            ! print *, "Coordinates: ", x(1), x(nx), y(1), y(ny), z(1), z(nz)
            error stop "Error: (xi, yi, zi) is out of bounds"
        end if
        
        ! Check that the vector fields are the same size
        if (any(shape(f1) /= shape(f2)) .or. any(shape(f1) /= shape(f3))) then
            error stop "Error: Vector field components must have the same dimensions"
        end if

        ! Find bounding indices in x, y, z
        i = binary_search(x_vec(1), x)
        j = binary_search(x_vec(2), y)
        k = binary_search(x_vec(3), z)

        ! Interpolate each component separately

        fi(1) = interp3d(x_vec(1), x_vec(2), x_vec(3), x, y, z, f1)
        fi(2) = interp3d(x_vec(1), x_vec(2), x_vec(3), x, y, z, f2)
        fi(3) = interp3d(x_vec(1), x_vec(2), x_vec(3), x, y, z, f3)

    end function interp3d_vec_field
end module mod_vec_interp