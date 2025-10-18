module mod_bounds
    use stdlib_kinds, only: dp
    implicit none(type, external)

    private
    
   type, public :: t_bounds
      real(dp) :: xs(2), ys(2), zs(2)
   contains
      procedure, public :: init => init_bounds
      procedure, public :: print => print_bounds
   end type

contains
    pure subroutine init_bounds(self, xs, ys, zs)
        class(t_bounds), intent(inout) :: self
        real(dp), intent(in) :: xs(2)
        real(dp), intent(in) :: ys(2)
        real(dp), intent(in) :: zs(2)

        ! TODO: Check that the mins are less than the max values
        self%xs(1) = xs(1)
        self%xs(2) = xs(2)
        
        self%ys(1) = ys(1)
        self%ys(2) = ys(2)
        
        self%zs(1) = zs(1)
        self%zs(2) = zs(2)
    end subroutine init_bounds

    subroutine print_bounds(self, ext_name)
        class(t_bounds), intent(in) :: self
        character(len=*), intent(in), optional :: ext_name

        ! Local variables
        character(:), allocatable :: header_start
        character(len=1000) :: separator = REPEAT('-', 60) ! A consistent separator line

        ! --- Construct the starting header ---
        if (present(ext_name)) then
            ! Concatenate '--- ', trimmed name, and ' ---'
            header_start = "--- " // TRIM(ext_name) // " ---"
        else
            ! Use a simple separator if no name is provided
            header_start = separator
        end if

        ! --- Print the bounds ---
        print *, TRIM(header_start)
        print *, "x-bounds:", self%xs(1), self%xs(2)
        print *, "y-bounds:", self%ys(1), self%ys(2)
        print *, "z-bounds:", self%zs(1), self%zs(2)
        print *, TRIM(separator) ! Print the consistent separator at the end
    end subroutine print_bounds
end module mod_bounds