module mod_scalar_interp
    use stdlib_kinds, only: dp
    use mod_search, only: binary_search
    use mod_linear_interp, only: linear_interp, bilinear_interp, trilinear_interp

    implicit none
    private
    public :: interp1d, interp2d, interp3d

    contains

    ! Given a discrete grid and function values on that grid, interpolate to find the function value at a given point
    ! 1D linear interpolation
    pure function interp1d(xi, x, f) result(fi)
        real(dp), intent(in) :: xi          ! Point to interpolate at
        real(dp), intent(in) :: x(:)        ! 1D array of x-coordinates (must be sorted)
        real(dp), intent(in) :: f(:)        ! 1D array of function values at x-coordinates
        real(dp) :: fi                      ! Interpolated function value at xi
        
        ! Local variables
        integer :: i, n

        n = size(x)
        if (xi < x(1) .or. xi > x(n)) then
            ! print *, "Point: ", xi
            ! print *, "Coordinates: ", x(1), x(n)
            error stop "Error: xi is out of bounds"
        end if

        ! Find the interval [x(i), x(i+1)] that contains xi
        i = binary_search(xi, x(1:n))

        if (i == n) i = n - 1  ! Handle edge case where xi is exactly x(n)
        
        ! Perform linear interpolation
        fi = linear_interp(xi, x(i), x(i+1), f(i), f(i+1))
    end function interp1d

    ! 2D bilinear interpolation
    pure function interp2d(xi, yi, x, y, f) result(fi)
        real(dp), intent(in) :: xi, yi      ! Point to interpolate at
        real(dp), intent(in) :: x(:)        ! 1D array of x-coordinates (must be sorted)
        real(dp), intent(in) :: y(:)        ! 1D array of y-coordinates (must be sorted)
        real(dp), intent(in) :: f(:, :)     ! 2D array of function values at (x,y) grid points
        real(dp) :: fi                      ! Interpolated function value at (xi, yi)
        
        ! Local variables
        integer :: i, j, nx, ny

        nx = size(x)
        ny = size(y)
        if (xi < x(1) .or. xi > x(nx) .or. yi < y(1) .or. yi > y(ny)) then
            ! print *, "Point: ", xi, yi
            ! print *, "Coordinates: ", x(1), x(nx), y(1), y(ny)
            error stop "Error: (xi, yi) is out of bounds"
        end if
        ! Find lower bounding indices in x and y
        i = binary_search(xi, x(1:nx-1))
        j = binary_search(yi, y(1:ny-1))

        if (i == nx) i = nx - 1  ! Handle edge case where xi is exactly x(nx)
        if (j == ny) j = ny - 1  ! Handle edge case where yi is exactly y(ny)

        ! Perform bilinear interpolation
        fi = bilinear_interp(xi, yi, x(i), x(i+1), y(j), y(j+1), &
                             f(i,j), f(i+1,j), f(i,j+1), f(i+1,j+1))
    end function interp2d
    
    ! 3D trilinear interpolation
    pure function interp3d(xi, yi, zi, x, y, z, f) result(fi)
        real(dp), intent(in) :: xi, yi, zi  ! Point to interpolate at
        real(dp), intent(in) :: x(:)        ! 1D array of x-coordinates (must be sorted)
        real(dp), intent(in) :: y(:)        ! 1D array of y-coordinates (must be sorted)
        real(dp), intent(in) :: z(:)        ! 1D array of z-coordinates (must   be sorted)
        real(dp), intent(in) :: f(:, :, :)  ! 3D array of function values at (x,y,z) grid points
        real(dp) :: fi                      ! Interpolated function value at (xi, yi, zi)
        integer :: i, j, k, nx, ny, nz

        nx = size(x)
        ny = size(y)
        nz = size(z)
        if (xi < x(1) .or. xi > x(nx) .or. yi < y(1) .or. yi > y(ny) .or. zi < z(1) .or. zi > z(nz)) then
            ! print *, "Point: ", xi, yi, zi
            ! print *, "Coordinates: ", x(1), x(nx), y(1), y(ny), z(1), z(nz)
            error stop "Error: (xi, yi, zi) is out of bounds"
        end if

        ! Find lower bound indices in x, y, z
        i = binary_search(xi, x(1:nx))
        j = binary_search(yi, y(1:ny))
        k = binary_search(zi, z(1:nz))

        if (i == nx) i = nx - 1  ! Handle edge case where xi is exactly x(nx)
        if (j == ny) j = ny - 1  ! Handle edge case where yi is exactly y(ny)
        if (k == nz) k = nz - 1  ! Handle edge case where zi is exactly z(nz)

        ! Perform trilinear interpolation
        fi = trilinear_interp(xi, yi, zi, x(i), x(i+1), y(j), y(j+1), z(k), z(k+1), &
                             f(i,j,k), f(i+1,j,k), f(i,j+1,k), f(i+1,j+1,k), &
                             f(i,j,k+1), f(i+1,j,k+1), f(i,j+1,k+1), f(i+1,j+1,k+1))
    end function interp3d

end module mod_scalar_interp