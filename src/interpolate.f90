module mod_interpolate
    use stdlib_kinds, only: dp
    implicit none
    private
    public :: linear_interp, bilinear_interp, trilinear_interp

    contains
    pure function linear_interp(x, x0, x1, f0, f1) result(fx)
        ! Linearly interpolate to find fx at x given points (x0, f0) and (x1, f1)
        real(dp), intent(in) :: x, x0, x1, f0, f1
        real(dp) :: fx
        if (x1 == x0) then
            fx = f0  ! Avoid division by zero; return f0 if x0 == x1
        else
            fx = f0 + (f1 - f0) * (x - x0) / (x1 - x0)
        end if
    end function linear_interp

    pure function bilinear_interp(x, y, x0, x1, y0, y1, f00, f10, f01, f11) result(fxy)
        ! Bilinearly interpolate to find fxy at (x, y) given corner points
        ! (x0, y0, f00), (x1, y0, f10), (x0, y1, f01), (x1, y1, f11)
        real(dp), intent(in) :: x, y
        real(dp), intent(in) :: x0, x1, y0, y1
        real(dp), intent(in) :: f00, f10, f01, f11
        real(dp) :: fxy
        real(dp) :: fx0, fx1

        fx0 = linear_interp(x, x0, x1, f00, f10)
        fx1 = linear_interp(x, x0, x1, f01, f11)
        fxy = linear_interp(y, y0, y1, fx0, fx1)
    end function bilinear_interp

    pure function trilinear_interp(x, y, z, x0, x1, y0, y1, z0, z1, &
                                   f000, f100, f010, f110, f001, f101, f011, f111) result(fxyz)
        ! Trilinearly interpolate to find fxyz at (x, y, z) given corner points
        ! (x0, y0, z0, f000), (x1, y0, z0, f100), (x0, y1, z0, f010), (x1, y1, z0, f110),
        ! (x0, y0, z1, f001), (x1, y0, z1, f101), (x0, y1, z1, f011), (x1, y1, z1, f111)
        real(dp), intent(in) :: x, y, z
        real(dp), intent(in) :: x0, x1, y0, y1, z0, z1
        real(dp), intent(in) :: f000, f100, f010, f110
        real(dp), intent(in) :: f001, f101, f011, f111
        real(dp) :: fxyz
        real(dp) :: fxy0, fxy1

        fxy0 = bilinear_interp(x, y, x0, x1, y0, y1, f000, f100, f010, f110)
        fxy1 = bilinear_interp(x, y, x0, x1, y0, y1, f001, f101, f011, f111)
        fxyz = linear_interp(z, z0, z1, fxy0, fxy1)
    end function trilinear_interp
end module mod_interpolate