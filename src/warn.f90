module mod_warn
    use stdlib_kinds, only: dp
    use mod_set_default, only: set_default

    implicit none
    private
    public :: warn_bounds
contains

    subroutine warn_bounds(val,  min, max, name)
        real(dp), intent(in) :: val, min, max
        character(len=*), intent(in), optional :: name
        character(len=10) :: name_loc
        ! If name is provided use it, otherwise use a default name
        name_loc = set_default(name, "Value")
        ! print a warning if val is outside the bounds of min and max
        if (val < min) then
            print *, "Warning ", trim(name_loc), ":", val, " is below the minimum bound of ", min
        else if (val > max) then
            print *, "Warning ", trim(name_loc), ":", val, " is above the maximum bound of ", max
        end if
    end subroutine warn_bounds

end module mod_warn