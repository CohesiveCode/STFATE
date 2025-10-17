module mod_cloud_helper
    use stdlib_kinds, only: dp
    use mod_bounds, only: t_bounds

    !! helper procedures for cloud calcs
    private 
    public :: calc_clouds_bounds

contains
    pure function calc_y_centroid(vert_radius) result(centroid)
        real(dp), intent(in) :: vert_radius
        real(dp) :: centroid
        ! Calculate the centroid of a hemisphere of radius vert_radius
        centroid = (3.0_dp/8.0_dp) * vert_radius
    end function calc_y_centroid

    class(t_bounds) pure function calc_clouds_bounds(pos, y_centroid, vert_radius, horiz_radius) result (cloud_bounds)
        real(dp), intent(in)          :: pos(3)
        real(dp), intent(in)          :: y_centroid
        real(dp), intent(in)          :: vert_radius, horiz_radius
        ! Local variables
        real(dp) :: xs(2), ys(2), zs(2) ! Coordinate bounds

        xs(1) = pos(1) - horiz_radius
        xs(2) = pos(1) + horiz_radius

        ys(1) = pos(2) - y_centroid
        ys(2) = pos(2) + (vert_radius - y_centroid)

        zs(1) = pos(3) - horiz_radius
        zs(2) = pos(3) + horiz_radius

        call cloud_bounds%init(xs, ys, zs)
    end function calc_clouds_bounds

    pure subroutine check_cloud_in_domain(cloud_bounds, domain_bounds, stat)
        ! Subroutine checks that a cloud fits within the domain defined by x, y, z
        ! Inputs:
        type(t_bounds), intent(in)  :: cloud_bounds         ! Cloud bounds
        type(t_bounds), intent(in)  :: domain_bounds        ! Domain bounds
        integer(int32), intent(out) :: stat  ! Error code 

        ! Local variables
        real(dp), parameter :: tol = epsilon(1.0_dp)

        ! Init stat to no error status aka 0
        stat = 0

        if (cloud_bounds%xs(1) .le. domain_bounds%xs(1)-tol .or. & ! left coordinate
            cloud_bounds%xs(2) .ge. domain_bounds%xs(2)+tol) then  ! right coordinate

            stat = 1
            ! stop
        else if (cloud_bounds%ys(1) .le. domain_bounds%ys(1)-tol .or. & ! Check coordinate closer to the surface
                    cloud_bounds%ys(2) .ge. domain_bounds%ys(2)+tol) then  ! Check coordinate closer to the bed
            ! print *, "top y, bottom y, y(1), y(ny): ", top_y, bottom_y, y(1), y(ny)
            ! print *, "Error: Cloud does not fit within the y domain"
            stat = 2
        else if(cloud_bounds%zs(1) .le. domain_bounds%zs(1)-tol .or. & 
            cloud_bounds%zs(2) .ge. domain_bounds%zs(2)+tol) then
            ! print *, "Error: Cloud does not fit within the z domain"
            stat = 3
        end if
    end subroutine check_cloud_in_domain
end module mod_cloud_helper