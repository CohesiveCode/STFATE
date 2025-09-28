module mod_set_default
    use stdlib_kinds, only: dp
    implicit none

    private
    public :: set_default  ! generic interface

    interface set_default
        module procedure set_default_dp
        module procedure set_default_int
        module procedure set_default_logical
        module procedure set_default_char
    end interface set_default

contains
    pure function set_default_dp(val, default) result(res)
        ! Return present val (real(dp)) or fallback default.
        real(dp), intent(in), optional :: val
        real(dp), intent(in) :: default
        real(dp) :: res
        if (present(val)) then
            res = val
        else
            res = default
        end if
    end function set_default_dp

    pure function set_default_int(val, default) result(res)
        integer, intent(in), optional :: val
        integer, intent(in) :: default
        integer :: res
        if (present(val)) then
            res = val
        else
            res = default
        end if
    end function set_default_int

    pure function set_default_logical(val, default) result(res)
        logical, intent(in), optional :: val
        logical, intent(in) :: default
        logical :: res
        if (present(val)) then
            res = val
        else
            res = default
        end if
    end function set_default_logical

    pure function set_default_char(val, default) result(res)
        ! Deferred-length result to accommodate differing lengths.
        character(len=*), intent(in), optional :: val
        character(len=*), intent(in) :: default
        character(len=:), allocatable :: res
        if (present(val)) then
            res = val
        else
            res = default
        end if
    end function set_default_char

end module mod_set_default