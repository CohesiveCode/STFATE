module mod_search
    use stdlib_kinds, only: dp
    implicit none
    private
    public :: binary_search
contains

   pure function binary_search(val, arr) result(idx)
        ! Perform binary search to find the index of the largest element in arr less than or equal to val
        real(dp), intent(in) :: val
        real(dp), intent(in) :: arr(:)  ! Must be sorted in ascending order
        integer :: idx
        integer :: low, high, mid
        real(dp), parameter :: tol = epsilon(1.0_dp) * 10.0_dp  ! Tolerance for floating-point comparison

        low = 1
        high = size(arr)

        if (val < arr(low) .or. val > arr(high)) then
            error stop "Error: val is out of bounds"
        end if
        
        ! Check if val is exactly equal to the first or last element
        ! Check if the val is equal to the first or last element with a tolerance
        if (abs(val - arr(low)) < tol) then
            idx = low
            return
        else if (abs(val - arr(high)) < tol) then
            idx = high
            return
        end if

        ! Binary search loop
        do while (low <= high)
            mid = (low + high) / 2
            if (arr(mid) == val) then
                idx = mid
                return
            else if (arr(mid) < val) then
                low = mid + 1
            else
                high = mid - 1
            end if
        end do

        idx = high  ! high is the index of the largest element <= val
    end function binary_search

    

end module mod_search