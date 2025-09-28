pure subroutine find_low_bound2d(x_vec, x, y, i, j) 
        ! Find the indices of the grid points that bound the point x_vec in 2D
        real(dp), intent(in) :: x_vec(2)      ! Point to find bounds for (x, y)
        real(dp), intent(in) :: x(:)          ! 1D array of x-coordinates (must be sorted)
        real(dp), intent(in) :: y(:)          ! 1D array of y-coordinates (must be sorted)
        integer, intent(out):: i, j

        ! Local variables
        integer :: nx, ny

        nx = size(x)
        ny = size(y)

        ! Find bounding indices in x
        i = binary_search(x_vec(1), x(1:nx-1))

        ! Find bounding indices in y
        j = binary_search(x_vec(2), y(1:ny-1))
    end subroutine find_bounds2d

      pure function find_bounds3d(x_vec, x, y, z) result(bounds)
        ! Find the indices of the grid points that bound the point x_vec in 3D
        real(dp), intent(in) :: x_vec(3)      ! Point to find bounds for (x, y, z)
        real(dp), intent(in) :: x(:)          ! 1D array of x-coordinates (must be sorted)
        real(dp), intent(in) :: y(:)          ! 1D array of y-coordinates (must be sorted)
        real(dp), intent(in) :: z(:)          ! 1D array of z-coordinates (must be sorted)
        integer :: bounds(3, 2)               ! Indices of bounding grid points: bounds(:,1) = lower, bounds(:,2) = upper
        integer :: i, j, k, nx, ny, nz

        nx = size(x)
        ny = size(y)
        nz = size(z)
        if (x_vec(1) < x(1) .or. x_vec(1) > x(nx) .or. &
            x_vec(2) < y(1) .or. x_vec(2) > y(ny) .or. &
            x_vec(3) < z(1) .or. x_vec(3) > z(nz)) then
            error stop "Error: x_vec is out of bounds"
        end if
        
        ! Find bounding indices in x
        i = binary_search(x_vec(1), x(1:nx-1))
        bounds(1,1) = i
        bounds(1,2) = i + 1

        ! Find bounding indices in y
        j = binary_search(x_vec(2), y(1:ny-1))
        bounds(2,1) = j
        bounds(2,2) = j + 1

        ! Find bounding indices in z
        k = binary_search(x_vec(3), z(1:nz-1))
        ! Check that k is within valid range

        bounds(3,1) = k
        bounds(3,2) = k + 1
    end function find_bounds3d